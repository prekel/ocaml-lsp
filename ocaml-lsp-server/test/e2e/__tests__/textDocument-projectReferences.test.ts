import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { execFileSync } from "child_process";
import { CancellationTokenSource } from "vscode-jsonrpc/node";
import { URI } from "vscode-uri";

jest.setTimeout(30000);
const maybeDescribe = os.type() === "Windows_NT" ? describe.skip : describe;
const sources: Record<string, string> = {
  "a.mli":
    "val shared : int\ntype t = A | B of int\ntype r = { field : int }\nmodule Nested : sig val x : int end\n",
  "a.ml":
    "let hidden = 2\nlet shared = hidden\ntype t = A | B of int\ntype r = { field : int }\nmodule Nested = struct let x = shared end\n",
  "b.ml":
    "module Alias = A\nlet use = A.shared + Alias.shared\nlet constr = A.B 1\nlet record = { A.field = 3 }\nlet nested = Alias.Nested.x\n",
  "c.ml":
    "open A\nlet use = shared\nlet value : t = B 2\nlet get (r : r) = r.field\n",
};

maybeDescribe("project references on OCaml 4.14", () => {
  let directory: string;
  let server: LanguageServer.LanguageServer | undefined;
  let openFiles: Map<string, { text: string; version: number }>;
  let logs: string[];

  function project(
    root: string,
    dune = "(library (name references) (wrapped false))",
  ) {
    fs.mkdirSync(root, { recursive: true });
    fs.writeFileSync(path.join(root, "dune-project"), "(lang dune 3.0)\n");
    fs.writeFileSync(path.join(root, "dune"), dune + "\n");
    for (const [name, text] of Object.entries(sources))
      fs.writeFileSync(path.join(root, name), text);
  }
  function build(root = directory) {
    execFileSync("dune", ["build", "--root", root, "@check"], {
      cwd: root,
      stdio: "pipe",
    });
  }
  async function start(roots = [directory], encoding?: "utf-8" | "utf-16") {
    server = await LanguageServer.startAndInitialize({
      rootUri: URI.file(directory).toString(),
      workspaceFolders: roots.map((root) => ({
        name: path.basename(root),
        uri: URI.file(root).toString(),
      })),
      capabilities: encoding
        ? { general: { positionEncodings: [encoding] } }
        : {},
    });
    server.onNotification(Protocol.LogMessageNotification.type, (params) =>
      logs.push(params.message),
    );
  }
  function open(name: string, text?: string) {
    const filename = path.join(directory, name);
    const uri = URI.file(filename).toString();
    if (openFiles.has(name)) return;
    text = text ?? fs.readFileSync(filename, "utf8");
    openFiles.set(name, { text, version: 0 });
    server!.sendNotification(Protocol.DidOpenTextDocumentNotification.type, {
      textDocument: Types.TextDocumentItem.create(uri, "ocaml", 0, text),
    });
  }
  function change(name: string, text: string) {
    open(name);
    const version = openFiles.get(name)!.version + 1;
    openFiles.set(name, { text, version });
    server!.sendNotification(Protocol.DidChangeTextDocumentNotification.type, {
      textDocument: {
        uri: URI.file(path.join(directory, name)).toString(),
        version,
      },
      contentChanges: [{ text }],
    });
  }
  function close(name: string) {
    openFiles.delete(name);
    server!.sendNotification(Protocol.DidCloseTextDocumentNotification.type, {
      textDocument: { uri: URI.file(path.join(directory, name)).toString() },
    });
  }
  async function query(
    name: string,
    needle: string,
    includeDeclaration = false,
    inside = 0,
  ) {
    open(name);
    const text = openFiles.get(name)!.text;
    const offset = text.indexOf(needle) + inside;
    expect(offset).toBeGreaterThanOrEqual(0);
    const lines = text.slice(0, offset).split("\n");
    const locations = await server!.sendRequest(
      Protocol.ReferencesRequest.type,
      {
        textDocument: { uri: URI.file(path.join(directory, name)).toString() },
        position: Types.Position.create(
          lines.length - 1,
          lines[lines.length - 1].length,
        ),
        context: { includeDeclaration },
      },
    );
    expect(locations).not.toBeNull();
    return locations!.map((location) => ({
      ...location,
      name: path.relative(directory, URI.parse(location.uri).fsPath),
    }));
  }
  function counts(locations: Array<{ name: string }>) {
    const result: Record<string, number> = {};
    for (const location of locations)
      result[location.name] = (result[location.name] ?? 0) + 1;
    return result;
  }

  beforeEach(() => {
    directory = fs.mkdtempSync(
      path.join(os.tmpdir(), "ocamllsp-project-references-"),
    );
    project(directory);
    server = undefined;
    openFiles = new Map();
    logs = [];
  });
  afterEach(async () => {
    if (server) await LanguageServer.exit(server);
    fs.rmSync(directory, { recursive: true, force: true });
  });

  it("finds closed-file usages from either side of an implementation/interface pair", async () => {
    build();
    await start();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
    expect(counts(await query("a.mli", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
    expect(counts(await query("a.ml", "shared", true))).toEqual({
      "a.ml": 2,
      "a.mli": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
    expect(counts(await query("b.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("distinguishes private implementation UIDs from colliding interface UIDs", async () => {
    build();
    await start();
    expect(counts(await query("a.ml", "hidden"))).toEqual({ "a.ml": 1 });
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("indexes types, constructors, record fields and nested modules", async () => {
    build();
    await start();
    expect(counts(await query("a.ml", "t ="))).toEqual({ "c.ml": 1 });
    expect(counts(await query("a.ml", "B of"))).toEqual({
      "b.ml": 1,
      "c.ml": 1,
    });
    expect(counts(await query("a.mli", "field"))).toEqual({
      "b.ml": 1,
      "c.ml": 1,
    });
    expect(counts(await query("a.ml", "Nested"))).toEqual({ "b.ml": 1 });
  });

  it("includes module prefixes and keeps the alias binding distinct", async () => {
    build();
    await start();
    expect(counts(await query("b.ml", "A.shared"))).toEqual({
      "b.ml": 4,
      "c.ml": 1,
    });
    expect(counts(await query("b.ml", "Alias"))).toEqual({ "b.ml": 2 });
  });

  it("does not merge shadowed values or same-named fields", async () => {
    fs.writeFileSync(
      path.join(directory, "d.ml"),
      "let shared = 0\nlet shared = shared + 1\nlet use = shared\ntype r = { field : int }\nlet get r = r.field\n",
    );
    build();
    await start();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
    expect(counts(await query("a.ml", "field"))).toEqual({
      "b.ml": 1,
      "c.ml": 1,
    });
    expect(
      (await query("d.ml", "shared", false)).map(
        (location) => location.range.start.line,
      ),
    ).toEqual([1]);
  });

  it("replaces a closed-file index with the unsaved buffer and restores it on close", async () => {
    build();
    await start();
    change("b.ml", "let use = A.shared + A.shared + A.shared\n");
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 3,
      "c.ml": 1,
    });
    close("b.ml");
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("keeps exported identity when unsaved edits move the definition", async () => {
    build();
    await start();
    change("a.ml", "(* inserted before the definition *)\n" + sources["a.ml"]);
    const result = await query("a.ml", "shared");
    expect(counts(result)).toEqual({ "a.ml": 1, "b.ml": 2, "c.ml": 1 });
    expect(
      result.find((location) => location.name === "a.ml")!.range.start.line,
    ).toBe(5);
  });

  it("skips stale closed-file positions and refreshes after manual dune build", async () => {
    build();
    await start();
    await query("a.ml", "shared");
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      sources["b.ml"] + "let added = A.shared\n",
    );
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "c.ml": 1,
    });
    expect(logs.some((message) => message.includes("Run dune build"))).toBe(
      true,
    );
    const logCount = logs.length;
    await query("a.ml", "shared");
    expect(logs.length).toBe(logCount);
    build();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 3,
      "c.ml": 1,
    });
  });

  it("uses local references without building the project automatically", async () => {
    await start();
    expect(counts(await query("a.ml", "shared"))).toEqual({ "a.ml": 1 });
    expect(
      fs.existsSync(
        path.join(
          directory,
          "_build",
          "default",
          ".references.objs",
          "byte",
          "a.cmt",
        ),
      ),
    ).toBe(false);
    expect(logs.some((message) => message.includes("Run dune build"))).toBe(
      true,
    );
  });

  it("does not merge identical unit names from separate workspaces", async () => {
    const second = path.join(directory, "second");
    project(second);
    build();
    build(second);
    await start([directory, second]);
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("finds project references to installed library declarations and implementations", async () => {
    fs.writeFileSync(
      path.join(directory, "dune"),
      "(library (name references) (wrapped false) (libraries stdune))\n",
    );
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      'let fail () = Stdune.Code_error.raise "boom" []\n',
    );
    build();
    await start();
    const fromUse = await query("b.ml", "Code_error.raise", false, 11);
    expect(counts(fromUse)).toEqual({ "b.ml": 1 });
    const stdune = execFileSync("ocamlfind", ["query", "stdune"], {
      encoding: "utf8",
    }).trim();
    for (const ext of ["ml", "mli"]) {
      const name = path.relative(
        directory,
        path.join(stdune, "code_error." + ext),
      );
      const found = await query(name, "raise", false);
      expect(found.some((loc) => loc.name === "b.ml")).toBe(true);
    }
  });

  it("indexes wrapped libraries using the compiler unit identity", async () => {
    fs.writeFileSync(
      path.join(directory, "dune"),
      "(library (name references))\n",
    );
    build();
    await start();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("does not merge implementations that share a module type", async () => {
    fs.writeFileSync(
      path.join(directory, "a.ml"),
      "module type S = sig val value : int end\nmodule F : S = struct let value = 1 end\nmodule G : S = struct let value = 2 end\n",
    );
    fs.writeFileSync(
      path.join(directory, "a.mli"),
      "module type S = sig val value : int end\nmodule F : S\nmodule G : S\n",
    );
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      "let first = A.F.value\nlet second = A.G.value\n",
    );
    fs.unlinkSync(path.join(directory, "c.ml"));
    build();
    await start();
    const result = await query("a.ml", "value = 1");
    expect(counts(result)).toEqual({ "b.ml": 1 });
    expect(result[0].range.start.line).toBe(0);
    expect(
      (await query("b.ml", "F.value", false, 2)).map(
        (location) => location.range.start.line,
      ),
    ).toEqual([0]);
  });

  it("finds exception, module-type and class usages", async () => {
    fs.writeFileSync(
      path.join(directory, "a.ml"),
      "exception Error\nmodule type S = sig end\nclass counter = object end\n",
    );
    fs.writeFileSync(
      path.join(directory, "a.mli"),
      "exception Error\nmodule type S = sig end\nclass counter : object end\n",
    );
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      "let error = A.Error\nmodule M : A.S = struct end\nlet counter = new A.counter\n",
    );
    fs.unlinkSync(path.join(directory, "c.ml"));
    build();
    await start();
    expect(counts(await query("a.ml", "Error"))).toEqual({ "b.ml": 1 });
    expect(counts(await query("a.mli", "S ="))).toEqual({ "b.ml": 1 });
    expect(counts(await query("a.ml", "counter"))).toEqual({ "b.ml": 1 });
  });

  it("keeps record fields from separate constrained modules distinct", async () => {
    fs.writeFileSync(
      path.join(directory, "a.ml"),
      "module type S = sig type t = { field : int } end\nmodule F : S = struct type t = { field : int } end\nmodule G : S = struct type t = { field : int } end\n",
    );
    fs.writeFileSync(
      path.join(directory, "a.mli"),
      "module type S = sig type t = { field : int } end\nmodule F : S\nmodule G : S\n",
    );
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      "let first = { A.F.field = 1 }\nlet second = { A.G.field = 2 }\n",
    );
    fs.writeFileSync(
      path.join(directory, "c.ml"),
      "let use (x : A.F.t) = x.field\nlet other (x : A.G.t) = x.field\n",
    );
    build();
    await start();
    expect(counts(await query("a.ml", "field : int } end\nmodule G"))).toEqual({
      "b.ml": 1,
      "c.ml": 1,
    });
    const result = await query("b.ml", "F.field", false, 2);
    expect(counts(result)).toEqual({ "b.ml": 1, "c.ml": 1 });
    expect(result[0].range.start.line).toBe(0);
  });

  it("indexes original source positions when Dune runs a PPX", async () => {
    fs.writeFileSync(
      path.join(directory, "dune"),
      "(library (name references) (wrapped false) (preprocess (pps ppx_yojson_conv)))\n",
    );
    build();
    await start();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("handles unreadable artifacts and removed source files", async () => {
    build();
    await start();
    const artifact = path.join(
      directory,
      "_build",
      "default",
      ".references.objs",
      "byte",
      "b.cmt",
    );
    fs.unlinkSync(artifact);
    fs.writeFileSync(artifact, "invalid cmt");
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "c.ml": 1,
    });
    fs.unlinkSync(path.join(directory, "c.ml"));
    expect(counts(await query("a.ml", "shared"))).toEqual({ "a.ml": 1 });
  });

  it("returns UTF-16 ranges in closed files containing Unicode", async () => {
    // Keep a valid top-level expression while putting a non-BMP character before the reference.
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      'let value = let text = "😀" in ignore text; A.shared\n',
    );
    build();
    await start();
    const result = await query("a.ml", "shared");
    const location = result.find((location) => location.name === "b.ml")!;
    const text = fs.readFileSync(path.join(directory, "b.ml"), "utf8");
    expect(location.range).toEqual(
      Types.Range.create(
        0,
        text.indexOf("shared"),
        0,
        text.indexOf("shared") + 6,
      ),
    );
  });
  it("refreshes newly created and deleted compilation units", async () => {
    build();
    await start();
    await query("a.ml", "shared");
    fs.writeFileSync(path.join(directory, "d.ml"), "let extra = A.shared\n");
    build();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
      "d.ml": 1,
    });
    fs.unlinkSync(path.join(directory, "d.ml"));
    build();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("handles comments inside qualified names and a caret at the identifier end", async () => {
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      "let use = A.(* outer (* nested *) *)shared\n",
    );
    build();
    await start();
    const result = await query("a.ml", "shared", false, 6);
    expect(counts(result)).toEqual({ "a.ml": 1, "b.ml": 1, "c.ml": 1 });
    expect(
      result.find((location) => location.name === "b.ml")!.range.start
        .character,
    ).toBe("let use = A.(* outer (* nested *) *)shared".indexOf("shared"));
  });

  it("honors UTF-8 position encoding", async () => {
    const text = 'let value = let text = "😀" in ignore text; A.shared\n';
    fs.writeFileSync(path.join(directory, "b.ml"), text);
    build();
    await start([directory], "utf-8");
    const result = await query("a.ml", "shared");
    expect(
      result.find((location) => location.name === "b.ml")!.range.start
        .character,
    ).toBe(Buffer.byteLength(text.slice(0, text.indexOf("shared"))));
  });

  it("evicts a removed workspace and indexes an added workspace", async () => {
    const first = directory;
    const second = path.join(directory, "second");
    project(second);
    build();
    build(second);
    await start();
    await query("a.ml", "shared");
    server!.sendNotification(
      Protocol.DidChangeWorkspaceFoldersNotification.type,
      {
        event: {
          removed: [
            { name: path.basename(first), uri: URI.file(first).toString() },
          ],
          added: [{ name: "second", uri: URI.file(second).toString() }],
        },
      },
    );
    directory = second;
    openFiles = new Map();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
    directory = first;
    expect(counts(await query("a.ml", "shared"))).toEqual({ "a.ml": 1 });
  });

  it("cancels a reference request and remains usable", async () => {
    build();
    await start();
    open("a.ml");
    const token = new CancellationTokenSource();
    const request = server!.sendRequest(
      Protocol.ReferencesRequest.type,
      {
        textDocument: {
          uri: URI.file(path.join(directory, "a.ml")).toString(),
        },
        position: Types.Position.create(1, 4),
        context: { includeDeclaration: false },
      },
      token.token,
    );
    token.cancel();
    await expect(request).rejects.toMatchObject({
      code: Protocol.LSPErrorCodes.RequestCancelled,
    });
    token.dispose();
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("rejects inconsistent positions when a document changes during the search", async () => {
    build();
    await start();
    open("a.ml");
    const request = server!.sendRequest(Protocol.ReferencesRequest.type, {
      textDocument: { uri: URI.file(path.join(directory, "a.ml")).toString() },
      position: Types.Position.create(1, 4),
      context: { includeDeclaration: false },
    });
    change("a.ml", "(* moved *)\n" + sources["a.ml"]);
    await expect(request).rejects.toMatchObject({
      code: Protocol.LSPErrorCodes.ContentModified,
    });
    expect(counts(await query("a.ml", "shared"))).toEqual({
      "a.ml": 1,
      "b.ml": 2,
      "c.ml": 1,
    });
  });

  it("keeps constructors from separate constrained modules distinct", async () => {
    fs.writeFileSync(
      path.join(directory, "a.ml"),
      "module type S = sig type t = C end\nmodule F : S = struct type t = C end\nmodule G : S = struct type t = C end\n",
    );
    fs.writeFileSync(
      path.join(directory, "a.mli"),
      "module type S = sig type t = C end\nmodule F : S\nmodule G : S\n",
    );
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      "let first = A.F.C\nlet second = A.G.C\n",
    );
    fs.writeFileSync(
      path.join(directory, "c.ml"),
      "let use = function A.F.C -> ()\nlet other = function A.G.C -> ()\n",
    );
    build();
    await start();
    const result = await query("a.ml", "C end\nmodule G");
    expect(counts(result)).toEqual({ "b.ml": 1, "c.ml": 1 });
    expect(result.every((location) => location.range.start.line === 0)).toBe(
      true,
    );
  });

  it("does not merge ambiguous functor applications", async () => {
    fs.writeFileSync(
      path.join(directory, "a.ml"),
      "module Make () = struct let value = 1 end\n",
    );
    fs.writeFileSync(
      path.join(directory, "a.mli"),
      "module Make () : sig val value : int end\n",
    );
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      "module F = A.Make ()\nmodule G = A.Make ()\nlet first = F.value\nlet second = G.value\n",
    );
    fs.unlinkSync(path.join(directory, "c.ml"));
    build();
    await start();
    expect(await query("b.ml", "F.value", false, 2)).toEqual([]);
  });

  it("uses a custom Dune build directory from the active Merlin configuration", async () => {
    const previous = process.env.DUNE_BUILD_DIR;
    process.env.DUNE_BUILD_DIR = path.join(directory, "artifacts");
    try {
      build();
      await start();
      expect(counts(await query("a.ml", "shared"))).toEqual({
        "a.ml": 1,
        "b.ml": 2,
        "c.ml": 1,
      });
    } finally {
      if (previous === undefined) delete process.env.DUNE_BUILD_DIR;
      else process.env.DUNE_BUILD_DIR = previous;
    }
  });
});
