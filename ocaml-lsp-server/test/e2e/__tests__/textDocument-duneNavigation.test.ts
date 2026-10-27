import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { URI } from "vscode-uri";

describe("Dune navigation without client extensions", () => {
  let directory: string;
  let server: LanguageServer.LanguageServer | undefined;
  let texts: Record<string, string>;
  let versions: Map<string, number>;
  let registrations: Protocol.Registration[];

  function uri(name: string) {
    return URI.file(path.join(directory, name)).toString();
  }
  function write(name: string, text: string) {
    texts[name] = text;
    fs.mkdirSync(path.dirname(path.join(directory, name)), { recursive: true });
    fs.writeFileSync(path.join(directory, name), text);
  }
  async function setup(
    files: Record<string, string> = {},
    capabilities: Protocol.ClientCapabilities = {},
  ) {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), "ocamllsp-dune-nav-"));
    texts = {};
    versions = new Map();
    registrations = [];
    for (const [name, text] of Object.entries({
      "dune-project": "(lang dune 3.0)\n",
      "lib/dune": "(library (name local_lib) (public_name example.api))\n",
      "bin/dune":
        "(executable (name main) (libraries local_lib example.api))\n",
      "bin/main.ml": "let () = ()\n",
      ...files,
    }))
      write(name, text);
    server = LanguageServer.start();
    server.onRequest(
      "client/registerCapability",
      (params: Protocol.RegistrationParams) => {
        registrations.push(...params.registrations);
        return null;
      },
    );
    await server.sendRequest(Protocol.InitializeRequest.type, {
      processId: process.pid,
      rootUri: URI.file(directory).toString(),
      workspaceFolders: [
        { name: "project", uri: URI.file(directory).toString() },
      ],
      capabilities,
    });
    server.sendNotification(Protocol.InitializedNotification.type, {});
  }
  function open(name: string) {
    if (versions.has(name)) return;
    versions.set(name, 0);
    server!.sendNotification(Protocol.DidOpenTextDocumentNotification.type, {
      textDocument: Types.TextDocumentItem.create(
        uri(name),
        "dune",
        0,
        texts[name],
      ),
    });
  }
  function change(name: string, text: string) {
    open(name);
    texts[name] = text;
    const version = versions.get(name)! + 1;
    versions.set(name, version);
    server!.sendNotification(Protocol.DidChangeTextDocumentNotification.type, {
      textDocument: { uri: uri(name), version },
      contentChanges: [{ text }],
    });
  }
  function position(name: string, needle: string, inside = 0) {
    const index = texts[name].indexOf(needle);
    expect(index).toBeGreaterThanOrEqual(0);
    const prefix = texts[name].slice(0, index + inside).split("\n");
    const last = prefix[prefix.length - 1];
    return Types.Position.create(prefix.length - 1, last.length);
  }
  async function query(
    name: string,
    needle: string,
    inside = 0,
    method = "definition",
  ) {
    open(name);
    return (
      (await server!.sendRequest<Types.Location[] | null>(
        "textDocument/" + method,
        {
          textDocument: { uri: uri(name) },
          position: position(name, needle, inside),
        },
      )) || []
    );
  }
  function target(
    name: string,
    needle?: string,
    inside = 0,
    length = needle?.length || 0,
  ) {
    const start = needle
      ? position(name, needle, inside)
      : Types.Position.create(0, 0);
    const end = needle ? position(name, needle, inside + length) : start;
    return Types.Location.create(uri(name), Types.Range.create(start, end));
  }
  function watched(name: string, type: Protocol.FileChangeType) {
    server!.sendNotification(Protocol.DidChangeWatchedFilesNotification.type, {
      changes: [{ uri: uri(name), type }],
    });
  }
  afterEach(async () => {
    if (server) await LanguageServer.exit(server);
    server = undefined;
    if (directory) fs.rmSync(directory, { recursive: true, force: true });
  });

  it("registers Dune definitions and incremental synchronization through standard LSP", async () => {
    await setup(
      {},
      {
        textDocument: {
          synchronization: { dynamicRegistration: true },
          definition: { dynamicRegistration: true },
          declaration: { dynamicRegistration: true },
        },
        workspace: { didChangeWatchedFiles: { dynamicRegistration: true } },
      },
    );
    await query("bin/dune", "local_lib");
    expect(registrations.map((r) => r.method).sort()).toEqual([
      "textDocument/declaration",
      "textDocument/definition",
      "textDocument/didChange",
      "textDocument/didClose",
      "textDocument/didOpen",
      "workspace/didChangeWatchedFiles",
    ]);
    expect(
      registrations
        .find((r) => r.method === "textDocument/definition")!
        .registerOptions.documentSelector.map(
          (filter: Protocol.DocumentFilter) => filter.language,
        ),
    ).toEqual(["dune", "dune-project", "dune-workspace"]);
    expect(
      registrations.find((r) => r.method === "textDocument/didChange")!
        .registerOptions.syncKind,
    ).toBe(Protocol.TextDocumentSyncKind.Incremental);
    expect(fs.existsSync(path.join(directory, "_build"))).toBe(false);
  });
  it("does not request unsupported dynamic capabilities", async () => {
    await setup(
      {},
      { textDocument: { definition: { dynamicRegistration: true } } },
    );
    await query("bin/dune", "local_lib");
    expect(registrations.map((r) => r.method)).toEqual([
      "textDocument/definition",
    ]);
  });
  it("finds local and public library declarations without a build", async () => {
    await setup();
    expect(await query("bin/dune", "local_lib", 3)).toEqual([
      target("lib/dune", "local_lib"),
    ]);
    expect(await query("bin/dune", "example.api", 4, "declaration")).toEqual([
      target("lib/dune", "example.api"),
    ]);
    expect(registrations).toEqual([]);
    expect(fs.existsSync(path.join(directory, "_build"))).toBe(false);
  });
  it("does not duplicate a library whose private and public names coincide", async () => {
    await setup({
      "lib/dune": "(library (name local_lib) (public_name local_lib))\n",
    });
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
  });
  it("respects private library names at nested dune-project boundaries", async () => {
    await setup({
      "nested/dune-project": "(lang dune 3.0)\n",
      "nested/lib/dune": "(library (name local_lib))\n",
      "nested/bin/dune":
        "(executable (name main) (libraries local_lib example.api))\n",
    });
    expect(await query("nested/bin/dune", "local_lib")).toEqual([
      target("nested/lib/dune", "local_lib"),
    ]);
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
    expect(await query("nested/bin/dune", "example.api")).toEqual([
      target("lib/dune", "example.api"),
    ]);
  });
  it("updates workspace membership even when a removed target remains open", async () => {
    await setup({
      "left/dune": "(executable (name app) (libraries exported))\n",
      "right/dune": "(library (name hidden) (public_name exported))\n",
    });
    open("right/dune");
    expect(await query("left/dune", "exported")).toEqual([
      target("right/dune", "exported"),
    ]);
    server!.sendNotification(
      Protocol.DidChangeWorkspaceFoldersNotification.type,
      {
        event: {
          removed: [{ name: "project", uri: URI.file(directory).toString() }],
          added: [{ name: "left", uri: uri("left") }],
        },
      },
    );
    expect(await query("left/dune", "exported")).toEqual([]);
    server!.sendNotification(
      Protocol.DidChangeWorkspaceFoldersNotification.type,
      {
        event: { removed: [], added: [{ name: "right", uri: uri("right") }] },
      },
    );
    expect(await query("left/dune", "exported")).toEqual([
      target("right/dune", "exported"),
    ]);
  });
  it("returns all ambiguous declarations and skips build/switch/dependency directories", async () => {
    await setup({
      "other/dune": "(library (name local_lib))\n",
      "_build/default/dune": "(library (name local_lib))\n",
      "_opam/lib/dune": "(library (name local_lib))\n",
      "node_modules/pkg/dune": "(library (name local_lib))\n",
      ".hidden/dune": "(library (name local_lib))\n",
    });
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
      target("other/dune", "local_lib"),
    ]);
  });
  it("updates both source and target using unsaved buffers and incremental edits", async () => {
    await setup();
    await query("bin/dune", "local_lib");
    change("lib/dune", "; 😀\n(library (name renamed))\n");
    expect(await query("bin/dune", "local_lib")).toEqual([]);
    change("bin/dune", "(executable (name main) (libraries renamed))\n");
    expect(await query("bin/dune", "renamed")).toEqual([
      target("lib/dune", "renamed"),
    ]);
    server!.sendNotification(Protocol.DidChangeTextDocumentNotification.type, {
      textDocument: { uri: uri("lib/dune"), version: 2 },
      contentChanges: [{ range: Types.Range.create(1, 0, 1, 0), text: "  " }],
    });
    texts["lib/dune"] = "; 😀\n  (library (name renamed))\n";
    expect(await query("bin/dune", "renamed")).toEqual([
      target("lib/dune", "renamed"),
    ]);
  });
  it("reverts an unsaved target when its buffer closes", async () => {
    await setup();
    change("lib/dune", "(library (name temporary))\n");
    expect(await query("bin/dune", "local_lib")).toEqual([]);
    server!.sendNotification(Protocol.DidCloseTextDocumentNotification.type, {
      textDocument: { uri: uri("lib/dune") },
    });
    texts["lib/dune"] = fs.readFileSync(
      path.join(directory, "lib/dune"),
      "utf8",
    );
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
  });
  it("handles closed-file changes, additions and removals from watched events", async () => {
    await setup();
    await query("bin/dune", "local_lib");
    write("lib/dune", "; moved\n(library (name local_lib))\n");
    watched("lib/dune", Protocol.FileChangeType.Changed);
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
    fs.unlinkSync(path.join(directory, "lib/dune"));
    watched("lib/dune", Protocol.FileChangeType.Deleted);
    expect(await query("bin/dune", "local_lib")).toEqual([]);
    write("new/dune", "(library (name local_lib))\n");
    watched("new/dune", Protocol.FileChangeType.Created);
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("new/dune", "local_lib"),
    ]);
  });
  it("keeps watched generated Dune copies out of the source index", async () => {
    await setup();
    await query("bin/dune", "local_lib");
    write("_build/default/dune", "(library (name local_lib))\n");
    watched("_build/default/dune", Protocol.FileChangeType.Created);
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
  });
  it("validates closed targets by content even with equal timestamps and sizes", async () => {
    await setup();
    await query("bin/dune", "local_lib");
    const stamp = fs.statSync(path.join(directory, "lib/dune"));
    write("lib/dune", texts["lib/dune"].replace("local_lib", "other_lib"));
    fs.utimesSync(path.join(directory, "lib/dune"), stamp.atime, stamp.mtime);
    expect(await query("bin/dune", "local_lib")).toEqual([]);
  });
  it("prefers interfaces for module lists and implementations for executable names", async () => {
    await setup({
      "bin/dune":
        "(executable (name main) (modules Main (:standard \\ Util)))\n",
      "bin/main.mli": "val main : unit\n",
      "bin/util.ml": "let value = 1\n",
    });
    expect(await query("bin/dune", "Main")).toEqual([target("bin/main.mli")]);
    expect(await query("bin/dune", "Util")).toEqual([target("bin/util.ml")]);
    expect(await query("bin/dune", "main")).toEqual([target("bin/main.ml")]);
    expect(await query("bin/dune", ":standard")).toEqual([]);
  });
  it("can open a new interface that only exists in an unsaved OCaml buffer", async () => {
    await setup({ "bin/dune": "(executable (name main) (modules Main))\n" });
    server!.sendNotification(Protocol.DidOpenTextDocumentNotification.type, {
      textDocument: Types.TextDocumentItem.create(
        uri("bin/main.mli"),
        "ocaml.interface",
        0,
        "",
      ),
    });
    expect(await query("bin/dune", "Main")).toEqual([target("bin/main.mli")]);
    server!.sendNotification(Protocol.DidCloseTextDocumentNotification.type, {
      textDocument: { uri: uri("bin/main.mli") },
    });
    expect(await query("bin/dune", "Main")).toEqual([target("bin/main.ml")]);
  });
  it("opens literal include, dependency and ordered-set include paths", async () => {
    await setup({
      "bin/dune":
        '(include "common.inc")\n(executable (name main) (modules (:include modules.list)) (deps "data file.txt" (file ../input.txt) (:named named.txt)))\n',
      "bin/common.inc": "; shared stanzas\n",
      "bin/modules.list": "Main\n",
      "bin/data file.txt": "data\n",
      "input.txt": "input\n",
      "bin/named.txt": "input\n",
    });
    for (const [needle, name] of [
      ["common.inc", "bin/common.inc"],
      ["modules.list", "bin/modules.list"],
      ["data file.txt", "bin/data file.txt"],
      ["../input.txt", "input.txt"],
      ["named.txt", "bin/named.txt"],
    ])
      expect(await query("bin/dune", needle)).toEqual([target(name)]);
  });
  it("finds PPX and re-exported libraries without treating PPX arguments as libraries", async () => {
    await setup({
      "bin/dune":
        "(executable (name main) (libraries (re_export local_lib)) (preprocess (pps example.api -- local_lib)))\n",
    });
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
    expect(await query("bin/dune", "example.api")).toEqual([
      target("lib/dune", "example.api"),
    ]);
    expect(await query("bin/dune", "-- local_lib", 3)).toEqual([]);
  });
  it("navigates PPX and module names inside per_module specifications", async () => {
    await setup({
      "bin/dune":
        "(executable (name main) (preprocess (per_module ((pps example.api) Main))))\n",
      "bin/main.mli": "",
    });
    expect(await query("bin/dune", "example.api")).toEqual([
      target("lib/dune", "example.api"),
    ]);
    expect(await query("bin/dune", "Main")).toEqual([target("bin/main.mli")]);
  });
  it("finds alias and rule declarations with relative and recursive alias dependencies", async () => {
    await setup({
      "bin/dune":
        "(rule (deps (alias check) (alias ../lib/check) (alias_rec ../lib/check)))\n(alias (name check))\n",
      "lib/dune": "(rule (alias check) (action (echo ok)))\n",
      "lib/nested/dune": "(alias (name check))\n",
    });
    expect(await query("bin/dune", "alias check", 6)).toEqual([
      target("bin/dune", "name check", 5, 5),
    ]);
    expect(await query("bin/dune", "alias ../lib/check", 6)).toEqual([
      target("lib/dune", "check"),
    ]);
    expect(await query("bin/dune", "alias_rec ../lib/check", 10)).toEqual([
      target("lib/dune", "check"),
      target("lib/nested/dune", "check"),
    ]);
  });
  it("resolves nested subdir module paths and declarations", async () => {
    await setup({
      dune: "(subdir inner (library (name inner_lib)) (executable (name app) (modules App (:include modules.list)) (libraries inner_lib)))\n",
      "inner/app.ml": "let () = ()\n",
      "inner/app.mli": "",
      "inner/modules.list": "App\n",
    });
    expect(await query("dune", "App")).toEqual([target("inner/app.mli")]);
    expect(await query("dune", "app")).toEqual([target("inner/app.ml")]);
    expect(await query("dune", "modules.list")).toEqual([
      target("inner/modules.list"),
    ]);
    expect(await query("dune", "libraries inner_lib", 10)).toEqual([
      target("dune", "inner_lib"),
    ]);
  });
  it("ignores comments, action strings, variables, globs and missing files", async () => {
    await setup({
      "bin/dune":
        '; (libraries local_lib)\n(rule (action (echo "(libraries local_lib)")) (deps %{dep:input.txt} *.txt absent.txt))\n',
    });
    expect(await query("bin/dune", "local_lib")).toEqual([]);
    expect(await query("bin/dune", '"(libraries local_lib)', 12)).toEqual([]);
    expect(await query("bin/dune", "%{dep:input.txt}")).toEqual([]);
    expect(await query("bin/dune", "*.txt")).toEqual([]);
    expect(await query("bin/dune", "absent.txt")).toEqual([]);
  });
  it("keeps navigation useful in incomplete source lists without indexing incomplete declarations", async () => {
    await setup({
      "bad/dune": "(library (name local_lib)",
      "bin/dune": "(executable (name main) (libraries local_lib",
    });
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
  });
  it("keeps end-of-line strings opaque and decodes escaped literal paths", async () => {
    await setup({
      "lib/dune":
        '(rule (action (echo\n "\\> (library (name local_lib))\n "\\| (library (name local_lib))\n )))\n(library (name real_lib))\n',
      "bin/dune":
        '(executable (name main) (libraries local_lib real_lib) (deps "data\\032file.txt"))\n',
      "bin/data file.txt": "data\n",
    });
    expect(await query("bin/dune", "local_lib")).toEqual([]);
    expect(await query("bin/dune", "real_lib")).toEqual([
      target("lib/dune", "real_lib"),
    ]);
    expect(await query("bin/dune", "data\\032file.txt")).toEqual([
      target("bin/data file.txt"),
    ]);
  });
  it.each(["utf-8", "utf-16"])(
    "returns encoded ranges for quoted names with non-ASCII prefixes (%s)",
    async (encoding) => {
      await setup(
        { "lib/dune": '(library (synopsis "😀") (name "local_lib"))\n' },
        {
          general: { positionEncodings: [encoding] },
        },
      );
      const result = await query("bin/dune", "local_lib");
      const expected = target("lib/dune", "local_lib");
      if (encoding === "utf-8") {
        expected.range.start.character += 2;
        expected.range.end.character += 2;
      }
      expect(result).toEqual([expected]);
    },
  );
});
