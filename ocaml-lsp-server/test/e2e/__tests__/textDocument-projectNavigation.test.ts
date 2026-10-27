import * as LanguageServer from "../src/LanguageServer";
import { CancellationTokenSource } from "vscode-jsonrpc";
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
  async function setup(
    files: Record<string, string> = {},
    encoding = "utf-16",
  ) {
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
    for (const [name, text] of Object.entries(texts)) {
      fs.mkdirSync(path.dirname(path.join(directory, name)), {
        recursive: true,
      });
      fs.writeFileSync(path.join(directory, name), text);
    }
    execFileSync("dune", ["build", "--root", directory, "@check"], {
      stdio: "pipe",
    });
    server = await LanguageServer.startAndInitialize({
      capabilities: { general: { positionEncodings: [encoding] } },
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
      target("lib.mli", "long_name"),
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
      target("lib.mli", "Constructor"),
    ]);
    expect(await query("declaration", "main.ml", "Lib.Constructor", 4)).toEqual(
      [target("lib.mli", "Constructor")],
    );
    expect(await query("definition", "main.ml", "Nested.value", 7)).toEqual([
      target("lib.mli", "value"),
    ]);
  });
  it("resolves moved unsaved target bindings by identity", async () => {
    await setup();
    open("lib.ml", "(* inserted *)\n\n" + texts["lib.ml"]);
    open("lib.mli", "(* inserted *)\n" + texts["lib.mli"]);
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
    expect(await query("declaration", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("uses UTF-16 for both query and target positions", async () => {
    await setup({
      "lib.mli":
        "(* 😀 *) val long_name : int -> int\ntype t = Constructor\nmodule Nested : sig val value : int end\n",
      "main.ml": 'let use = let s = "😀" in ignore s; Lib.long_name 1\n',
    });
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
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
  it("prefers interfaces for references inside an interface", async () => {
    await setup({
      "client.mli": "val value : Lib.t\n",
      "client.ml": "let value = Lib.Constructor\n",
    });
    expect(await query("definition", "client.mli", "Lib.t", 4)).toEqual([
      target("lib.mli", "t =", 0, 1),
    ]);
    expect(await query("definition", "lib.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("uses changed target buffers on repeated requests", async () => {
    await setup();
    open("lib.mli");
    for (let version = 1; version <= 3; version++) {
      texts["lib.mli"] = "\n" + texts["lib.mli"];
      server!.sendNotification(
        Protocol.DidChangeTextDocumentNotification.type,
        {
          textDocument: { uri: uri("lib.mli"), version },
          contentChanges: [{ text: texts["lib.mli"] }],
        },
      );
      expect(await query("definition", "main.ml", "long_name")).toEqual([
        target("lib.mli", "long_name"),
      ]);
    }
  });
  it("keeps an open interface available after its disk source is removed", async () => {
    await setup();
    open("lib.mli", "\n" + texts["lib.mli"]);
    fs.unlinkSync(path.join(directory, "lib.mli"));
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("observes rebuilt annotations on the next jump", async () => {
    await setup();
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
    texts["lib.mli"] = "\n" + texts["lib.mli"];
    fs.writeFileSync(path.join(directory, "lib.mli"), texts["lib.mli"]);
    execFileSync("dune", ["build", "--root", directory, "@check"], {
      stdio: "pipe",
    });
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("does not snapshot unrelated buffers or scan unrelated artifacts", async () => {
    const extra = Object.fromEntries(
      Array.from({ length: 50 }, (_, i) => [`extra${i}.ml`, "let value = 1\n"]),
    );
    await setup(extra);
    for (const name of Object.keys(extra)) open(name);
    open("main.ml");
    // A project-wide artifact scan would try to read this malformed annotation.
    fs.writeFileSync(
      path.join(directory, "_build/default/unrelated.cmt"),
      "broken",
    );
    const warnings: string[] = [];
    server!.onNotification(
      Protocol.LogMessageNotification.type,
      ({ message }) => {
        if (message.includes("build artifacts")) warnings.push(message);
      },
    );
    let trace: { traceEvents: { name: string }[] };
    server!.onRequest("window/showDocument", ({ uri: traceUri }) => {
      const file = URI.parse(traceUri).fsPath;
      trace = JSON.parse(fs.readFileSync(file, "utf8"));
      fs.unlinkSync(file);
      return { success: true };
    });
    const snapshots = async () => {
      await server!.sendRequest("workspace/executeCommand", {
        command: "ocamllsp/view-metrics",
        arguments: [],
      });
      return trace!.traceEvents.filter(
        (event) => event.name === "project snapshot",
      ).length;
    };
    const before = await snapshots();
    const pending = query("definition", "main.ml", "long_name");
    server!.sendNotification(Protocol.DidChangeTextDocumentNotification.type, {
      textDocument: { uri: uri("extra0.ml"), version: 1 },
      contentChanges: [{ text: "let value = 2\n" }],
    });
    expect(await pending).toEqual([target("lib.mli", "long_name")]);
    expect(await snapshots()).toBe(before);
    expect(
      trace!.traceEvents.filter((event) => event.name === "definition"),
    ).toHaveLength(1);
    expect(
      trace!.traceEvents.filter((event) => event.name === "definition range"),
    ).toEqual([]);
    expect(warnings).toEqual([]);
  });
  it("keeps same-named units in separate libraries distinct", async () => {
    await setup({
      "other/dune": "(library (name other) (wrapped false))\n",
      "other/lib.ml": "let long_name = true\n",
      "other/lib.mli": "\nval long_name : bool\n",
      "other/main.ml": "let result = Lib.long_name\n",
    });
    for (const name of ["main.ml", "other/main.ml", "main.ml"]) {
      const destination = name === "main.ml" ? "lib.mli" : "other/lib.mli";
      expect(await query("definition", name, "long_name")).toEqual([
        target(destination, "long_name"),
      ]);
    }
  });
  it("honors UTF-8 positions for interface navigation", async () => {
    await setup(
      {
        "lib.mli":
          "(* 😀 *) val long_name : int -> int\ntype t = Constructor\nmodule Nested : sig val value : int end\n",
        "main.ml": 'let use = let s = "😀" in ignore s; Lib.long_name 1\n',
      },
      "utf-8",
    );
    open("main.ml");
    const character = Buffer.byteLength(
      texts["main.ml"].split("long_name")[0],
      "utf8",
    );
    const start = Buffer.byteLength(
      texts["lib.mli"].split("long_name")[0],
      "utf8",
    );
    expect(
      await server!.sendRequest("textDocument/definition", {
        textDocument: { uri: uri("main.ml") },
        position: Types.Position.create(0, character),
      }),
    ).toEqual([
      Types.Location.create(
        uri("lib.mli"),
        Types.Range.create(0, start, 0, start + 9),
      ),
    ]);
  });
  it("rejects a source changed during a jump", async () => {
    await setup();
    open("main.ml");
    const pending = query("definition", "main.ml", "long_name");
    server!.sendNotification(Protocol.DidChangeTextDocumentNotification.type, {
      textDocument: { uri: uri("main.ml"), version: 1 },
      contentChanges: [{ text: "\n" + texts["main.ml"] }],
    });
    await expect(pending).rejects.toMatchObject({ code: -32801 });
    texts["main.ml"] = "\n" + texts["main.ml"];
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("cancels a jump and remains usable", async () => {
    await setup();
    open("main.ml");
    const cancellation = new CancellationTokenSource();
    const pending = server!.sendRequest(
      "textDocument/definition",
      {
        textDocument: { uri: uri("main.ml") },
        position: Types.Position.create(0, 18),
      },
      cancellation.token,
    );
    cancellation.cancel();
    await expect(pending).rejects.toMatchObject({ code: -32800 });
    cancellation.dispose();
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("invalidates cached target syntax when a declaration is removed and restored", async () => {
    await setup();
    open("lib.mli");
    expect(await query("definition", "main.ml", "long_name")).toEqual([
      target("lib.mli", "long_name"),
    ]);
    const saved = texts["lib.mli"];
    for (const [version, text] of [
      [1, saved.replace("val long_name : int -> int\n", "")],
      [2, "\n\n" + saved],
    ] as const) {
      texts["lib.mli"] = text;
      server!.sendNotification(
        Protocol.DidChangeTextDocumentNotification.type,
        {
          textDocument: { uri: uri("lib.mli"), version },
          contentChanges: [{ text }],
        },
      );
      expect(await query("definition", "main.ml", "long_name")).toEqual(
        version === 1 ? [] : [target("lib.mli", "long_name")],
      );
    }
  });
  it("checks source digests after rapid same-size closed-file edits", async () => {
    await setup();
    await query("definition", "main.ml", "long_name");
    const file = path.join(directory, "lib.mli");
    const previous = fs.statSync(file);
    fs.writeFileSync(file, texts["lib.mli"].replace("long_name", "lost_name"));
    fs.utimesSync(file, previous.atime, previous.mtime);
    expect(await query("definition", "main.ml", "long_name")).toEqual([]);
  });
  it("keeps type and value counterparts with the same spelling separate", async () => {
    await setup({
      "lib.ml": "type t = Constructor\nlet t = Constructor\n",
      "lib.mli": "type t = Constructor\nval t : t\n",
      "main.ml": "let value : Lib.t = Lib.t\n",
    });
    expect(await query("definition", "lib.mli", "type t", 5)).toEqual([
      target("lib.ml", "type t", 5, 1),
    ]);
    expect(await query("definition", "lib.mli", "val t", 4)).toEqual([
      target("lib.ml", "let t", 4, 1),
    ]);
  });
  it("does not connect a hidden top-level binding to the public counterpart", async () => {
    await setup({
      "lib.ml":
        "let long_name x = x\nlet first = long_name 1\nlet long_name x = x + first\n",
      "lib.mli": "val long_name : int -> int\n",
      "main.ml": "let result = Lib.long_name 1\n",
    });
    expect(await query("definition", "lib.ml", "long_name x")).toEqual([]);
    expect(await query("definition", "lib.ml", "long_name x = x +")).toEqual([
      target("lib.mli", "long_name"),
    ]);
  });
  it("leaves uncertain include exports and module type members to explicit implementation navigation", async () => {
    await setup({
      "lib.ml":
        "let long_name x = x\nlet first = long_name 1\ninclude struct let long_name x = x + first end\nmodule type S = sig type t end\n",
      "lib.mli": "val long_name : int -> int\nmodule type S = sig type t end\n",
      "main.ml": "let result = Lib.long_name 1\n",
    });
    expect(await query("definition", "lib.ml", "long_name x")).toEqual([]);
    expect(await query("definition", "lib.mli", "t end")).toEqual([]);
  });
  it("selects an operator name without typing its target", async () => {
    await setup({
      "lib.ml": "let ( ++ ) a b = a + b\n",
      "lib.mli": "val ( ++ ) : int -> int -> int\n",
      "main.ml": "let result = Lib.( ++ ) 1 2\n",
    });
    const [location] = await query("definition", "main.ml", "++");
    expect(location.uri).toBe(uri("lib.mli"));
    expect(
      texts["lib.mli"].slice(
        location.range.start.character,
        location.range.end.character,
      ),
    ).toContain("++");
  });
  it("does not return outdated positions in a changed closed interface", async () => {
    await setup();
    await query("definition", "main.ml", "long_name");
    fs.writeFileSync(
      path.join(directory, "lib.mli"),
      "(* moved on disk *)\n" + texts["lib.mli"],
    );
    const locations = await query("definition", "main.ml", "long_name");
    expect(locations.every((location) => location.uri !== uri("lib.mli"))).toBe(
      true,
    );
  });
});
