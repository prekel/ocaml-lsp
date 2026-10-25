import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { execFileSync } from "child_process";
import { URI } from "vscode-uri";

describe("go to implementation", () => {
  let directory: string;
  let server: LanguageServer.LanguageServer | undefined;
  let texts: Record<string, string>;

  afterEach(async () => {
    if (server) await LanguageServer.exit(server);
    server = undefined;
    if (directory) fs.rmSync(directory, { recursive: true, force: true });
  });

  function uri(name: string) {
    return URI.file(path.join(directory, name)).toString();
  }

  async function setup(files: Record<string, string>) {
    directory = fs.mkdtempSync(
      path.join(os.tmpdir(), "ocamllsp-implementation-"),
    );
    texts = files;
    fs.writeFileSync(path.join(directory, "dune-project"), "(lang dune 3.0)\n");
    const interfaceOnly = Object.keys(files)
      .filter((name) => name.endsWith(".mli") && !(name.slice(0, -1) in files))
      .map((name) => name.slice(0, -4));
    fs.writeFileSync(
      path.join(directory, "dune"),
      `(library (name impl_test) (wrapped false) (modules_without_implementation ${interfaceOnly.join(
        " ",
      )}))\n`,
    );
    for (const [name, text] of Object.entries(files))
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
    server!.sendNotification(Protocol.DidOpenTextDocumentNotification.type, {
      textDocument: Types.TextDocumentItem.create(uri(name), "ocaml", 0, text),
    });
  }

  async function query(name: string, needle: string, inside = 0) {
    open(name);
    const index = texts[name].indexOf(needle);
    expect(index).toBeGreaterThanOrEqual(0);
    const lines = texts[name].slice(0, index + inside).split("\n");
    const result = await server!.sendRequest<Types.Location[] | null>(
      "textDocument/implementation",
      {
        textDocument: { uri: uri(name) },
        position: Types.Position.create(
          lines.length - 1,
          lines[lines.length - 1].length,
        ),
      },
    );
    return result || [];
  }

  it.each([
    ["type t", "type t = int\n", "type t = int\n", "type t", 5],
    [
      "nested module",
      "module M : sig type t end\n",
      "module M = struct type t = int end\n",
      "t end",
      0,
    ],
    [
      "include",
      "type t\n",
      "module M = struct type t = int end\ninclude M\n",
      "type t",
      5,
    ],
    [
      "module alias",
      "module A : sig type t end\nmodule B : sig type t end\n",
      "module A = struct type t = int end\nmodule B = A\n",
      "t end",
      0,
    ],
    [
      "functor result",
      "module F : functor (X : sig end) -> sig type t end\n",
      "module F (X : sig end) = struct type t = int end\n",
      "t end",
      0,
    ],
    [
      "generative functor",
      "module F : functor () -> sig type t end\n",
      "module F () = struct type t = int end\n",
      "t end",
      0,
    ],
    [
      "curried functor",
      "module F : functor (X : sig end) -> functor (Y : sig end) -> sig type t end\n",
      "module F (X : sig end) (Y : sig end) = struct type t = int end\n",
      "t end",
      0,
    ],
  ])("resolves %s", async (_title, intf, impl, needle, inside) => {
    await setup({ "lib.mli": intf as string, "lib.ml": impl as string });
    const result = await query("lib.mli", needle as string, inside as number);
    expect(result).toHaveLength(1);
    expect(result[0].uri).toBe(uri("lib.ml"));
    const offset = (impl as string).indexOf("type t") + 5;
    const before = (impl as string).slice(0, offset).split("\n");
    expect(result[0].range.start).toEqual(
      Types.Position.create(
        before.length - 1,
        before[before.length - 1].length,
      ),
    );
  });

  it("uses an unsaved implementation on repeated requests", async () => {
    await setup({ "lib.mli": "type t\n", "lib.ml": "type t = int\n" });
    open("lib.ml", "\n\ntype t = string\n");
    for (let i = 0; i < 3; i++) {
      const result = await query("lib.mli", "type t", 5);
      expect(result[0].range).toEqual(Types.Range.create(2, 5, 2, 6));
    }
  });

  it.each(["missing", "directory"])(
    "returns no body for a %s counterpart",
    async (kind) => {
      await setup({ "lib.mli": "type t\n", "lib.ml": "type t = int\n" });
      fs.unlinkSync(path.join(directory, "lib.ml"));
      if (kind === "directory") fs.mkdirSync(path.join(directory, "lib.ml"));
      expect(await query("lib.mli", "type t", 5)).toEqual([]);
    },
  );

  it("rejects interface-only dependencies", async () => {
    await setup({ "lib.mli": "type t\n", "only.mli": "type t\n" });
    expect(await query("lib.mli", "type t", 5)).toEqual([]);
  });

  it("does not resolve module type templates or functor parameters", async () => {
    await setup({
      "lib.mli":
        "module type S = sig type t end\nmodule F : functor (X : S) -> sig type t end\n",
      "lib.ml":
        "module type S = sig type t end\nmodule F (X : S) = struct type t = int end\n",
    });
    expect(await query("lib.mli", "t end")).toEqual([]);
  });
});
