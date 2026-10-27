import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { URI } from "vscode-uri";
import { CancellationTokenSource } from "vscode-jsonrpc";

describe("Dune navigation without client extensions", () => {
  let directory: string;
  let server: LanguageServer.LanguageServer | undefined;
  let texts: Record<string, string>;
  let versions: Map<string, number>;
  let registrations: Protocol.Registration[];
  let initialized: Protocol.InitializeResult;
  let refreshes: number;

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
    environment: NodeJS.ProcessEnv = {},
  ) {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), "ocamllsp-dune-nav-"));
    texts = {};
    versions = new Map();
    registrations = [];
    refreshes = 0;
    for (const [name, text] of Object.entries({
      "dune-project": "(lang dune 3.0)\n",
      "lib/dune": "(library (name local_lib) (public_name example.api))\n",
      "bin/dune":
        "(executable (name main) (libraries local_lib example.api))\n",
      "bin/main.ml": "let () = ()\n",
      ...files,
    }))
      write(name, text);
    if (files["tools/ocamlfind"])
      fs.chmodSync(path.join(directory, "tools/ocamlfind"), 0o755);
    server = LanguageServer.start({
      env: {
        ...process.env,
        OCAMLLSP_TEST: "true",
        DUNE_TEST_ROOT: directory,
        PATH: path.join(directory, "tools") + path.delimiter + process.env.PATH,
        ...environment,
      },
    });
    server.onRequest("workspace/semanticTokens/refresh", () => {
      refreshes++;
      return null;
    });
    server.onRequest(
      "client/registerCapability",
      (params: Protocol.RegistrationParams) => {
        registrations.push(...params.registrations);
        return null;
      },
    );
    initialized = await server.sendRequest(Protocol.InitializeRequest.type, {
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
  const semanticCapabilities: Protocol.ClientCapabilities = {
    textDocument: {
      semanticTokens: {
        dynamicRegistration: true,
        requests: { full: { delta: true } },
        tokenTypes: ["namespace", "macro"],
        tokenModifiers: [],
        formats: ["relative"],
      },
    },
    workspace: { semanticTokens: { refreshSupport: true } },
  };
  async function semantic(name = "bin/dune") {
    open(name);
    const result = await server!.sendRequest<Protocol.SemanticTokens>(
      "textDocument/semanticTokens/full",
      { textDocument: { uri: uri(name) } },
    );
    return result;
  }
  function decode(name: string, result: Protocol.SemanticTokens) {
    const legend = initialized.capabilities.semanticTokensProvider!.legend;
    let line = 0,
      character = 0;
    const tokens = [];
    for (let i = 0; i < result.data.length; i += 5) {
      const [delta, column, length, type, modifiers] = result.data.slice(
        i,
        i + 5,
      );
      line += delta;
      character = delta ? column : character + column;
      tokens.push({
        name:
          initialized.capabilities.positionEncoding === "utf-8"
            ? Buffer.from(texts[name].split("\n")[line])
                .subarray(character, character + length)
                .toString("utf8")
            : texts[name]
                .split("\n")
                [line].slice(character, character + length),
        type: legend.tokenTypes[type],
        modifiers: legend.tokenModifiers.filter(
          (_, bit) => (modifiers & (1 << bit)) !== 0,
        ),
      });
    }
    return tokens;
  }
  async function until(predicate: () => boolean, timeout = 4000) {
    const deadline = Date.now() + timeout;
    while (!predicate()) {
      if (Date.now() > deadline)
        throw Error("Timed out waiting for external resolver");
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
  }
  function externalFiles() {
    return {
      "tools/ocamlfind": `#!/bin/sh
printf '%s\\n' "$4" >> "$DUNE_TEST_ROOT/queries.log"
printf '%s\\n' "$$" > "$DUNE_TEST_ROOT/query.pid"
case "$4" in
  slow.pkg|aaa.slow) sleep 5; exit 2 ;;
  external.api)
    subdir=sub
    if [ -f "$DUNE_TEST_ROOT/installed/pkg/relocated" ]; then subdir=moved; fi
    printf 'external.api\\n%s\\n%s\\n' "$DUNE_TEST_ROOT/installed/pkg/$subdir" "$DUNE_TEST_ROOT/installed/pkg/META" ;;
  external.ppx) printf 'external.ppx\\n%s\\n%s\\n' "$DUNE_TEST_ROOT/installed/pkg" "$DUNE_TEST_ROOT/installed/pkg/META" ;;
  meta.only) printf 'meta.only\\n%s\\n%s\\n' "$DUNE_TEST_ROOT/installed/legacy" "$DUNE_TEST_ROOT/installed/legacy/META" ;;
  *) exit 2 ;;
esac
`,
      "installed/pkg/META":
        'package "api" (directory = "sub")\npackage "ppx" ()\n',
      "installed/pkg/dune-package": `(lang dune 3.0)
(library (name external.api) (main_module_name Api)
 (modules (module (source (path Api) (intf (path sub/api.mli)) (impl (path sub/api.ml))))))
(library (name external.ppx) (kind ppx_rewriter))
`,
      "installed/pkg/sub/api.mli": "val value : int\n",
      "installed/pkg/sub/api.ml": "let value = 1\n",
      "installed/legacy/META": 'version = "1"\n',
    };
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
  it("registers semantic tokens dynamically with an append-only shared legend", async () => {
    await setup({}, semanticCapabilities);
    await semantic();
    const registration = registrations.find(
      (r) => r.method === "textDocument/semanticTokens",
    )!;
    expect(registration.registerOptions.full).toEqual({ delta: true });
    expect(registration.registerOptions.legend).toEqual(
      initialized.capabilities.semanticTokensProvider!.legend,
    );
    expect(
      registration.registerOptions.legend.tokenModifiers.slice(-2),
    ).toEqual(["duneLocal", "duneExternal"]);
  });
  it("underlines only resolvable local libraries and PPX, without highlighting PPX flags", async () => {
    await setup(
      {
        ...externalFiles(),
        "bin/dune":
          "(executable (name main) (libraries local_lib missing.lib) (preprocess (pps example.api -- local_lib)))\n",
      },
      semanticCapabilities,
    );
    expect(decode("bin/dune", await semantic())).toEqual([
      { name: "local_lib", type: "namespace", modifiers: ["duneLocal"] },
      { name: "example.api", type: "macro", modifiers: ["duneLocal"] },
    ]);
  });
  it("resolves an external subpackage to its declared interface and caches the package lookup", async () => {
    await setup({
      ...externalFiles(),
      "bin/dune": "(executable (name main) (libraries external.api))\n",
    });
    for (let i = 0; i < 4; i++) {
      expect(await query("bin/dune", "external.api")).toEqual([
        target("installed/pkg/sub/api.mli"),
      ]);
    }
    expect(
      fs
        .readFileSync(path.join(directory, "queries.log"), "utf8")
        .trim()
        .split("\n"),
    ).toEqual(["external.api"]);
    fs.unlinkSync(path.join(directory, "installed/pkg/sub/api.mli"));
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/sub/api.ml"),
    ]);
    fs.unlinkSync(path.join(directory, "installed/pkg/sub/api.ml"));
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/dune-package", "external.api"),
    ]);
  });
  it("prefers a local public name over an installed package", async () => {
    await setup({
      ...externalFiles(),
      "lib/dune": "(library (name local_lib) (public_name external.api))\n",
      "bin/dune": "(executable (name main) (libraries external.api))\n",
    });
    expect(await query("bin/dune", "external.api")).toEqual([
      target("lib/dune", "external.api"),
    ]);
    expect(decode("bin/dune", await semantic())[0].modifiers).toEqual([
      "duneLocal",
    ]);
    expect(fs.existsSync(path.join(directory, "queries.log"))).toBe(false);
  });
  it("falls back to PPX metadata and to META for legacy packages", async () => {
    await setup({
      ...externalFiles(),
      "bin/dune":
        "(executable (name main) (libraries meta.only) (preprocess (pps external.ppx)))\n",
    });
    expect(await query("bin/dune", "external.ppx")).toEqual([
      target("installed/pkg/dune-package", "external.ppx"),
    ]);
    expect(await query("bin/dune", "meta.only")).toEqual([
      target("installed/legacy/META"),
    ]);
    expect(decode("bin/dune", await semantic())).toEqual([
      { name: "meta.only", type: "namespace", modifiers: ["duneExternal"] },
      { name: "external.ppx", type: "macro", modifiers: ["duneExternal"] },
    ]);
  });
  it("refreshes external highlighting after background resolution and shares the query with a click", async () => {
    await setup(
      {
        ...externalFiles(),
        "bin/dune": "(executable (name main) (libraries external.api))\n",
      },
      semanticCapabilities,
    );
    await semantic();
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/sub/api.mli"),
    ]);
    await until(() => refreshes > 0);
    expect(decode("bin/dune", await semantic())).toEqual([
      { name: "external.api", type: "namespace", modifiers: ["duneExternal"] },
    ]);
    expect(
      fs
        .readFileSync(path.join(directory, "queries.log"), "utf8")
        .trim()
        .split("\n"),
    ).toEqual(["external.api"]);
  });
  it("updates semantic token deltas when an unsaved local declaration disappears", async () => {
    await setup({}, semanticCapabilities);
    const before = await semantic();
    expect(decode("bin/dune", before)).toHaveLength(2);
    change("lib/dune", "(library (name renamed))\n");
    const delta = await server!.sendRequest<Protocol.SemanticTokensDelta>(
      "textDocument/semanticTokens/full/delta",
      {
        textDocument: { uri: uri("bin/dune") },
        previousResultId: before.resultId,
      },
    );
    expect(delta.edits).toEqual([
      { start: 0, deleteCount: before.data.length },
    ]);
    expect(decode("bin/dune", await semantic())).toEqual([]);
  });
  it("keeps local navigation working without ocamlfind and caches missing packages", async () => {
    await setup(
      {
        "bin/dune":
          "(executable (name main) (libraries local_lib external.api))\n",
      },
      {},
      { PATH: "/nonexistent" },
    );
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
    expect(await query("bin/dune", "external.api")).toEqual([]);
    expect(decode("bin/dune", await semantic())).toEqual([
      { name: "local_lib", type: "namespace", modifiers: ["duneLocal"] },
    ]);
  });
  it("does not block a local click behind a stalled external query and supports cancellation", async () => {
    await setup({
      ...externalFiles(),
      "bin/dune": "(executable (name main) (libraries slow.pkg local_lib))\n",
    });
    open("bin/dune");
    const cancellation = new CancellationTokenSource();
    const slow = server!.sendRequest(
      "textDocument/definition",
      {
        textDocument: { uri: uri("bin/dune") },
        position: position("bin/dune", "slow.pkg"),
      },
      cancellation.token,
    );
    const rejected = expect(slow).rejects.toMatchObject({ code: -32800 });
    await until(() => fs.existsSync(path.join(directory, "queries.log")));
    const start = Date.now();
    expect(await query("bin/dune", "local_lib")).toEqual([
      target("lib/dune", "local_lib"),
    ]);
    cancellation.cancel();
    await rejected;
    expect(Date.now() - start).toBeLessThan(1000);
    cancellation.dispose();
  });
  it("bounds a stalled ocamlfind query to two seconds", async () => {
    await setup({
      ...externalFiles(),
      "bin/dune": "(executable (name main) (libraries slow.pkg))\n",
    });
    const start = Date.now();
    expect(await query("bin/dune", "slow.pkg")).toEqual([]);
    expect(Date.now() - start).toBeLessThan(3500);
  });
  it("terminates and reaps background package queries on server shutdown", async () => {
    await setup(
      {
        ...externalFiles(),
        "bin/dune": "(executable (name main) (libraries slow.pkg))\n",
      },
      semanticCapabilities,
    );
    await semantic();
    await until(() => fs.existsSync(path.join(directory, "query.pid")));
    const pid = Number(
      fs.readFileSync(path.join(directory, "query.pid"), "utf8"),
    );
    await LanguageServer.exit(server!);
    server = undefined;
    await until(() => {
      try {
        process.kill(pid, 0);
        return false;
      } catch (error) {
        return (error as NodeJS.ErrnoException).code === "ESRCH";
      }
    }, 1000);
  });
  it("does not queue an interactive external click behind background prefetch", async () => {
    await setup(
      {
        ...externalFiles(),
        "bin/dune":
          "(executable (name main) (libraries aaa.slow external.api))\n",
      },
      semanticCapabilities,
    );
    await semantic();
    await until(() => fs.existsSync(path.join(directory, "queries.log")));
    const start = Date.now();
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/sub/api.mli"),
    ]);
    expect(Date.now() - start).toBeLessThan(1000);
  });
  it("retries negative entries and reloads changed package metadata without requerying a positive package", async () => {
    await setup({
      ...externalFiles(),
      "bin/dune":
        "(executable (name main) (libraries absent.pkg external.api))\n",
    });
    expect(await query("bin/dune", "absent.pkg")).toEqual([]);
    expect(await query("bin/dune", "absent.pkg")).toEqual([]);
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/sub/api.mli"),
    ]);
    write("installed/pkg/new.mli", "val changed : bool\n");
    write(
      "installed/pkg/dune-package",
      texts["installed/pkg/dune-package"].replace("sub/api.mli", "new.mli"),
    );
    await new Promise((resolve) => setTimeout(resolve, 2100));
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/new.mli"),
    ]);
    expect(await query("bin/dune", "absent.pkg")).toEqual([]);
    const calls = fs
      .readFileSync(path.join(directory, "queries.log"), "utf8")
      .trim()
      .split("\n");
    expect(calls.filter((name) => name === "external.api")).toHaveLength(1);
    expect(calls.filter((name) => name === "absent.pkg")).toHaveLength(2);
  });
  it("requeries findlib when META changes the installation directory", async () => {
    await setup({
      ...externalFiles(),
      "installed/pkg/dune-package":
        "(library (name external.api) (main_module_name Api))\n",
      "bin/dune": "(executable (name main) (libraries external.api))\n",
    });
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/sub/api.mli"),
    ]);
    write("installed/pkg/relocated", "");
    write("installed/pkg/moved/api.mli", "val relocated : bool\n");
    write("installed/pkg/META", 'package "api" (directory = "moved")\n');
    // A click inside the TTL must not replace the metadata fingerprint before
    // the next findlib query has observed the changed directory.
    await query("bin/dune", "external.api");
    await new Promise((resolve) => setTimeout(resolve, 2100));
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/moved/api.mli"),
    ]);
    expect(
      fs
        .readFileSync(path.join(directory, "queries.log"), "utf8")
        .trim()
        .split("\n"),
    ).toEqual(["external.api", "external.api"]);
  });
  it("refreshes idle highlighting when an installed target disappears", async () => {
    await setup(
      {
        ...externalFiles(),
        "bin/dune": "(executable (name main) (libraries external.api))\n",
      },
      semanticCapabilities,
    );
    await query("bin/dune", "external.api");
    await semantic();
    await until(() => refreshes > 0);
    const before = refreshes;
    fs.unlinkSync(path.join(directory, "installed/pkg/sub/api.mli"));
    await until(() => refreshes > before, 6000);
    expect(await query("bin/dune", "external.api")).toEqual([
      target("installed/pkg/sub/api.ml"),
    ]);
  }, 7000);
  it.each(["utf-8", "utf-16"])(
    "uses negotiated encoding for highlighting and external metadata (%s)",
    async (encoding) => {
      await setup(
        {
          ...externalFiles(),
          "bin/dune":
            '(executable (name main) (synopsis "😀") (libraries local_lib) (preprocess (pps external.ppx)))\n',
          "installed/pkg/dune-package":
            '(library (synopsis "😀") (name external.ppx) (kind ppx_rewriter))\n',
        },
        { general: { positionEncodings: [encoding] } },
      );
      const expected = target("installed/pkg/dune-package", "external.ppx");
      if (encoding === "utf-8") {
        expected.range.start.character += 2;
        expected.range.end.character += 2;
      }
      // An ASCII source position avoids mixing JS UTF-16 offsets into a UTF-8 request.
      change(
        "bin/dune",
        "(executable (name main) (libraries local_lib) (preprocess (pps external.ppx)))\n",
      );
      expect(await query("bin/dune", "external.ppx")).toEqual([expected]);
      change(
        "bin/dune",
        '(executable (name main) (synopsis "😀") (libraries local_lib) (preprocess (pps external.ppx)))\n',
      );
      expect(decode("bin/dune", await semantic())).toEqual([
        { name: "local_lib", type: "namespace", modifiers: ["duneLocal"] },
        { name: "external.ppx", type: "macro", modifiers: ["duneExternal"] },
      ]);
    },
  );
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
  it("opens explicit rule targets and dependency/module references in _build", async () => {
    await setup({
      "bin/dune":
        "(rule (targets generated.ml generated.mli) (action (run generator)))\n(executable (name main) (modules Generated) (deps generated.ml))\n",
      "_build/default/bin/generated.ml": "let value = 1\n",
      "_build/default/bin/generated.mli": "val value : int\n",
    });
    expect(await query("bin/dune", "generated.ml")).toEqual([
      target("_build/default/bin/generated.ml"),
    ]);
    expect(await query("bin/dune", "generated.mli")).toEqual([
      target("_build/default/bin/generated.mli"),
    ]);
    expect(await query("bin/dune", "Generated")).toEqual([
      target("_build/default/bin/generated.mli"),
    ]);
    expect(await query("bin/dune", "deps generated.ml", 5)).toEqual([
      target("_build/default/bin/generated.ml"),
    ]);
  });
  it("switches an unsaved rule to promoted source targets", async () => {
    await setup({
      "bin/dune": "(rule (target generated.ml) (action (run generator)))\n",
      "bin/generated.ml": "let source = 1\n",
      "_build/default/bin/generated.ml": "let built = 2\n",
    });
    expect(await query("bin/dune", "generated.ml")).toEqual([
      target("_build/default/bin/generated.ml"),
    ]);
    change(
      "bin/dune",
      "(rule (mode promote) (target generated.ml) (action (run generator)))\n",
    );
    expect(await query("bin/dune", "generated.ml")).toEqual([
      target("bin/generated.ml"),
    ]);
    change(
      "bin/dune",
      "(rule (target generated.ml) (action (run generator)))\n",
    );
    expect(await query("bin/dune", "generated.ml")).toEqual([
      target("_build/default/bin/generated.ml"),
    ]);
  });
  it("supports promote into, until-clean and only predicates", async () => {
    await setup({
      "bin/dune":
        "(rule (mode (promote (until-clean) (into kept) (only *.mli))) (targets generated.ml generated.mli) (action (run generator)))\n",
      "bin/kept/generated.ml": "let source = 1\n",
      "bin/kept/generated.mli": "val source : int\n",
      "_build/default/bin/generated.ml": "let built = 2\n",
      "_build/default/bin/generated.mli": "val built : int\n",
    });
    expect(await query("bin/dune", "generated.mli")).toEqual([
      target("bin/kept/generated.mli"),
    ]);
    expect(await query("bin/dune", "generated.ml")).toEqual([
      target("_build/default/bin/generated.ml"),
    ]);
  });
  it("infers output files from literal Dune actions without executing them", async () => {
    await setup({
      "bin/dune":
        '(rule (with-stdout-to stdout.ml (echo "let x = 1")))\n(rule (action (progn (write-file written.ml "let x = 2") (copy input.txt copied.ml))))\n',
      "bin/input.txt": "input\n",
      "_build/default/bin/stdout.ml": "let x = 1\n",
      "_build/default/bin/written.ml": "let x = 2\n",
      "_build/default/bin/copied.ml": "input\n",
    });
    for (const name of ["stdout.ml", "written.ml", "copied.ml"]) {
      expect(await query("bin/dune", name)).toEqual([
        target("_build/default/bin/" + name),
      ]);
    }
    expect(await query("bin/dune", "input.txt")).toEqual([
      target("bin/input.txt"),
    ]);
  });
  it("resolves generated dependencies by full relative path and handles subdir stanzas", async () => {
    await setup({
      "producer/dune":
        "(subdir nested (rule (target generated.ml) (action (run generator))))\n",
      "other/dune": "(rule (target generated.ml) (action (run generator)))\n",
      "bin/dune":
        "(rule (target output) (deps ../producer/nested/generated.ml) (action (run generator)))\n",
      "_build/default/producer/nested/generated.ml": "let right = 1\n",
      "_build/default/other/generated.ml": "let wrong = 1\n",
    });
    expect(await query("bin/dune", "../producer/nested/generated.ml")).toEqual([
      target("_build/default/producer/nested/generated.ml"),
    ]);
    expect(await query("producer/dune", "generated.ml")).toEqual([
      target("_build/default/producer/nested/generated.ml"),
    ]);
  });
  it("uses the selected Merlin build context and preserves nested project paths", async () => {
    await setup({
      "dune-workspace":
        "(lang dune 3.0)\n(context (default (name chosen) (merlin)))\n",
      "nested/dune-project": "(lang dune 3.0)\n",
      "nested/lib/dune":
        "(rule (target generated.ml) (action (run generator)))\n",
      "_build/chosen/nested/lib/generated.ml": "let chosen = 1\n",
      "_build/default/nested/lib/generated.ml": "let default = 1\n",
    });
    expect(await query("nested/lib/dune", "generated.ml")).toEqual([
      target("_build/chosen/nested/lib/generated.ml"),
    ]);
  });
  it("returns existing alternative contexts without guessing between them", async () => {
    await setup({
      "bin/dune": "(rule (target generated.ml) (action (run generator)))\n",
      "_build/debug/bin/generated.ml": "let debug = 1\n",
      "_build/release/bin/generated.ml": "let release = 1\n",
    });
    expect(await query("bin/dune", "generated.ml")).toEqual([
      target("_build/debug/bin/generated.ml"),
      target("_build/release/bin/generated.ml"),
    ]);
  });
  it("opens existing files inside directory targets and promotes directory contents", async () => {
    await setup({
      "bin/dune":
        "(rule (targets (dir generated)) (action (run generator)))\n(rule (deps generated/part.ml) (action (run consumer)))\n",
      "_build/default/bin/generated/part.ml": "let part = 1\n",
      "bin/generated/part.ml": "let promoted = 2\n",
    });
    expect(await query("bin/dune", "generated/part.ml")).toEqual([
      target("_build/default/bin/generated/part.ml"),
    ]);
    change(
      "bin/dune",
      "(rule (mode promote) (targets (dir generated)) (action (run generator)))\n(rule (deps generated/part.ml) (action (run consumer)))\n",
    );
    expect(await query("bin/dune", "generated/part.ml")).toEqual([
      target("bin/generated/part.ml"),
    ]);
  });
  it("leaves unbuilt targets unresolved and uses a build copy until promotion exists", async () => {
    await setup({
      "bin/dune": "(rule (target generated.ml) (action (run generator)))\n",
    });
    expect(await query("bin/dune", "generated.ml")).toEqual([]);
    expect(fs.existsSync(path.join(directory, "_build"))).toBe(false);
    write("_build/default/bin/generated.ml", "let built = 1\n");
    change(
      "bin/dune",
      "(rule (mode promote) (target generated.ml) (action (run generator)))\n",
    );
    expect(await query("bin/dune", "generated.ml")).toEqual([
      target("_build/default/bin/generated.ml"),
    ]);
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
