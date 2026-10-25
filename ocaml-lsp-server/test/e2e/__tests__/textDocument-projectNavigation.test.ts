import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { execFileSync } from "child_process";
import { URI } from "vscode-uri";

describe("project definition and declaration navigation", () => {
  let directory: string;
  let server: LanguageServer.LanguageServer | undefined;
  let texts: Record<string, string>;
  let opened: Set<string>;

  function uri(name: string) {
    return URI.file(path.join(directory, name)).toString();
  }
  async function setup(files: Record<string, string> = {}) {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), "ocamllsp-navigation-"));
    texts = {
      "lib.mli":
        "val long_name : int -> int\ntype t = Constructor\nmodule Nested : sig val value : int end\n",
      "lib.ml":
        "let long_name x = x + 1\ntype t = Constructor\nmodule Nested = struct let value = 3 end\n",
      "main.ml":
        "let use = Lib.long_name 1\nlet value : Lib.t = Lib.Constructor\nlet nested = Lib.Nested.value\n",
      ...files,
    };
    opened = new Set();
    fs.writeFileSync(path.join(directory, "dune-project"), "(lang dune 3.0)\n");
    fs.writeFileSync(
      path.join(directory, "dune"),
      "(library (name navigation) (wrapped false))\n",
    );
    for (const [name, text] of Object.entries(texts))
      fs.writeFileSync(path.join(directory, name), text);
    execFileSync("dune", ["build", "--root", directory, "@check"], {
      stdio: "pipe",
    });
    server = await LanguageServer.startAndInitialize({
      rootUri: URI.file(directory).toString(),
      workspaceFolders: [
        { name: "project", uri: URI.file(directory).toString() },
      ],
    });
  }
  function open(name: string, text = texts[name]) {
    texts[name] = text;
    if (opened.has(name)) return;
    opened.add(name);
    server!.sendNotification(Protocol.DidOpenTextDocumentNotification.type, {
      textDocument: Types.TextDocumentItem.create(uri(name), "ocaml", 0, text),
    });
  }
  async function query(
    method: "definition" | "declaration" | "typeDefinition",
    name: string,
    needle: string,
    inside = 0,
  ) {
    open(name);
    const index = texts[name].indexOf(needle);
    expect(index).toBeGreaterThanOrEqual(0);
    const lines = texts[name].slice(0, index + inside).split("\n");
    return (
      (await server!.sendRequest<Types.Location[] | null>(
        "textDocument/" + method,
        {
          textDocument: { uri: uri(name) },
          position: Types.Position.create(
            lines.length - 1,
            lines[lines.length - 1].length,
          ),
        },
      )) || []
    );
  }
  function target(
    name: string,
    needle: string,
    inside = 0,
    length = needle.length,
  ) {
    const offset = texts[name].indexOf(needle) + inside;
    const lines = texts[name].slice(0, offset).split("\n");
    return Types.Location.create(
      uri(name),
      Types.Range.create(
        lines.length - 1,
        lines[lines.length - 1].length,
        lines.length - 1,
        lines[lines.length - 1].length + length,
      ),
    );
  }
  afterEach(async () => {
    if (server) await LanguageServer.exit(server);
    server = undefined;
    if (directory) fs.rmSync(directory, { recursive: true, force: true });
  });

  it("selects complete names in closed implementation and interface files", async () => {
    await setup();
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.ml", "long_name"),
    ]);
    expect(await query("declaration", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("navigates from an interface declaration to its body", async () => {
    await setup();
    expect(await query("definition", "lib.mli", "long_name")).toEqual([
      target("lib.ml", "long_name"),
    ]);
    expect(await query("definition", "lib.mli", "t =")).toEqual([
      target("lib.ml", "t =", 0, 1),
    ]);
  });
  it("navigates from an exported binding to its interface declaration", async () => {
    await setup();
    expect(await query("declaration", "lib.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("selects constructors and nested values in closed project sources", async () => {
    await setup();
    expect(await query("definition", "main.ml", "Lib.Constructor", 4)).toEqual([
      target("lib.ml", "Constructor"),
    ]);
    expect(await query("declaration", "main.ml", "Lib.Constructor", 4)).toEqual(
      [target("lib.mli", "Constructor")],
    );
    expect(await query("definition", "main.ml", "Nested.value", 7)).toEqual([
      target("lib.ml", "value"),
    ]);
  });
  it("resolves moved unsaved target bindings by identity", async () => {
    await setup();
    open("lib.ml", "(* inserted *)\n\n" + texts["lib.ml"]);
    open("lib.mli", "(* inserted *)\n" + texts["lib.mli"]);
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.ml", "long_name"),
    ]);
    expect(await query("declaration", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("uses UTF-16 for both query and target positions", async () => {
    await setup({
      "lib.ml":
        "(* 😀 *) let long_name x = x + 1\ntype t = Constructor\nmodule Nested = struct let value = 3 end\n",
      "main.ml": 'let use = let s = "😀" in ignore s; Lib.long_name 1\n',
    });
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.ml", "long_name"),
    ]);
  });
  it("refines type definition ranges in closed sources", async () => {
    await setup();
    expect(await query("typeDefinition", "main.ml", "value :")).toEqual([
      target("lib.mli", "t =", 0, 1),
    ]);
  });
  it("does not connect private or shadowed bindings to a public declaration", async () => {
    await setup({
      "lib.ml":
        "let long_name x = x + 1\nlet private_use () = let long_name = 2 in long_name\nlet () = ignore private_use\ntype t = Constructor\nmodule Nested = struct let value = 3 end\n",
    });
    expect(await query("declaration", "lib.ml", "long_name = 2")).toEqual([]);
    expect(await query("declaration", "lib.ml", "private_use")).toEqual([]);
  });
  it("falls back to the implementation when there is no interface", async () => {
    await setup();
    fs.unlinkSync(path.join(directory, "lib.mli"));
    execFileSync("dune", ["build", "--root", directory, "@check"], {
      stdio: "pipe",
    });
    expect(await query("declaration", "main.ml", "long_name")).toEqual([
      target("lib.ml", "long_name"),
    ]);
  });
  it("does not return outdated positions in a changed closed implementation", async () => {
    await setup();
    await query("definition", "main.ml", "long_name");
    fs.writeFileSync(
      path.join(directory, "lib.ml"),
      "(* moved on disk *)\n" + texts["lib.ml"],
    );
    const locations = await query("definition", "main.ml", "long_name");
    expect(locations.every((location) => location.uri !== uri("lib.ml"))).toBe(
      true,
    );
  });
});
