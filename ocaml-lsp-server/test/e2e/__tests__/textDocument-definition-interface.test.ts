import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { execFileSync } from "child_process";
import { URI } from "vscode-uri";

const sources: Record<string, string> = {
  "lib.ml":
    "type t = Constructor of int\nlet hidden x = x + 1\nlet exported x = hidden x\nlet _ = hidden 2\n",
  "lib.mli":
    "(** Public interface. *)\ntype t = Constructor of int\n\n(** Exported function. *)\n\nval exported : int -> int\n",
  "plain.ml": "let value = 7\n",
  "main.ml":
    "module Alias = Lib\nlet result = Lib.exported 1\nlet alias = Alias.exported 2\nlet plain = Plain.value\nlet constructor = Lib.Constructor 1\n",
};

describe("definition prefers interfaces", () => {
  let directory: string;
  let server: LanguageServer.LanguageServer;
  let documents: Map<string, string>;

  beforeEach(async () => {
    directory = fs.mkdtempSync(
      path.join(os.tmpdir(), "ocamllsp-interface-navigation-"),
    );
    fs.writeFileSync(path.join(directory, "dune-project"), "(lang dune 3.0)\n");
    fs.writeFileSync(
      path.join(directory, "dune"),
      "(library (name interface_navigation) (wrapped false))\n",
    );
    for (const [name, text] of Object.entries(sources))
      fs.writeFileSync(path.join(directory, name), text);
    execFileSync("dune", ["build", "--root", directory, "@check"], {
      stdio: "pipe",
    });
    documents = new Map();
    server = await LanguageServer.startAndInitialize({
      rootUri: URI.file(directory).toString(),
      workspaceFolders: [
        { name: "project", uri: URI.file(directory).toString() },
      ],
    });
  });

  afterEach(async () => {
    if (server) await LanguageServer.exit(server);
    fs.rmSync(directory, { recursive: true, force: true });
  });

  function uri(name: string) {
    return URI.file(path.join(directory, name)).toString();
  }

  function open(name: string, text = sources[name]) {
    documents.set(name, text);
    server.sendNotification(Protocol.DidOpenTextDocumentNotification.type, {
      textDocument: Types.TextDocumentItem.create(uri(name), "ocaml", 0, text),
    });
  }

  async function query(
    name: string,
    needle: string,
    inside = 0,
    method = "textDocument/definition",
  ) {
    if (!documents.has(name)) open(name);
    const text = documents.get(name)!;
    const offset = text.indexOf(needle);
    expect(offset).toBeGreaterThanOrEqual(0);
    const lines = text.slice(0, offset + inside).split("\n");
    const result = await server.sendRequest<
      Types.Location[] | Types.Location | null
    >(method, {
      textDocument: { uri: uri(name) },
      position: Types.Position.create(
        lines.length - 1,
        lines[lines.length - 1].length,
      ),
    });
    return result === null ? [] : Array.isArray(result) ? result : [result];
  }

  it("prefers the interface and uses its declaration position", async () => {
    const result = await query("main.ml", "Lib.exported", 5);
    expect(result).toHaveLength(1);
    expect(result[0].uri).toBe(uri("lib.mli"));
    expect(result[0].range.start).toEqual(Types.Position.create(5, 0));
  });

  it("follows module aliases to the interface", async () => {
    const result = await query("main.ml", "Alias.exported", 7);
    expect(result.map((location) => location.uri)).toEqual([uri("lib.mli")]);
    expect(result[0].range.start.line).toBe(5);
  });

  it("uses the implementation when a module has no interface", async () => {
    const result = await query("main.ml", "Plain.value", 7);
    expect(result.map((location) => location.uri)).toEqual([uri("plain.ml")]);
  });

  it("falls back when an interface source was removed after the build", async () => {
    fs.unlinkSync(path.join(directory, "lib.mli"));
    const result = await query("main.ml", "Lib.exported", 5);
    expect(result.map((location) => location.uri)).toEqual([uri("lib.ml")]);
    expect(result[0].range.start.line).toBe(2);
  });

  it("keeps private helpers in their implementation", async () => {
    const result = await query("lib.ml", "hidden 2", 2);
    expect(result).toEqual([
      Types.Location.create(uri("lib.ml"), Types.Range.create(1, 4, 1, 10)),
    ]);
  });

  it("keeps the nearest local binding when names are shadowed", async () => {
    open("main.ml", "let f value =\n  let value = value + 1 in\n  value\n");
    const result = await query("main.ml", "\n  value\n", 4);
    expect(result).toEqual([
      Types.Location.create(uri("main.ml"), Types.Range.create(1, 6, 1, 11)),
    ]);
  });

  it("refines the name range in an open, modified interface", async () => {
    open("lib.mli");
    const text = sources["lib.mli"] + "\n(* unsaved documentation *)\n";
    documents.set("lib.mli", text);
    server.sendNotification(Protocol.DidChangeTextDocumentNotification.type, {
      textDocument: { uri: uri("lib.mli"), version: 1 },
      contentChanges: [{ text }],
    });
    const result = await query("main.ml", "Lib.exported", 5);
    expect(result).toEqual([
      Types.Location.create(uri("lib.mli"), Types.Range.create(5, 4, 5, 12)),
    ]);
  });

  it("prefers interface declarations for constructors", async () => {
    const result = await query("main.ml", "Lib.Constructor", 6);
    expect(result.map((location) => location.uri)).toEqual([uri("lib.mli")]);
    expect(result[0].range.start.line).toBe(1);
  });

  it("keeps the declaration request directed at the interface", async () => {
    const result = await query(
      "main.ml",
      "Lib.exported",
      5,
      "textDocument/declaration",
    );
    expect(result.map((location) => location.uri)).toEqual([uri("lib.mli")]);
  });
});
