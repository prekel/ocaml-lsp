import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import { execFileSync } from "child_process";
import { CancellationTokenSource } from "vscode-jsonrpc/node";
import { URI } from "vscode-uri";

jest.setTimeout(30000);
const sources: Record<string, string> = {
  "a.mli": "val target : int -> int\nval recursor : int -> int\n",
  "a.ml":
    "let target x = x + 1\nlet rec recursor n = if n = 0 then target n else recursor (n - 1)\n",
  "b.ml":
    "module Alias = A\nlet caller x = A.target x + Alias.target x\nlet passed = List.map A.target [1;2]\nlet piped x = x |> A.target\nlet applied x = A.target @@ x\nlet init = A.target 42\n",
  "c.ml":
    "let outer x =\n  let local y = A.target y in\n  let closure = fun z -> A.target z in\n  local x + closure x\nlet callback f x = f x\nlet shadowed x = let target y = y in target x\n",
};

describe("call hierarchy on OCaml 4.14", () => {
  let directory: string;
  let server: LanguageServer.LanguageServer;
  let opened: Map<string, string>;

  beforeEach(async () => {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), "ocamllsp-calls-"));
    fs.writeFileSync(path.join(directory, "dune-project"), "(lang dune 3.0)\n");
    fs.writeFileSync(
      path.join(directory, "dune"),
      "(library (name calls) (wrapped false) (libraries stdune))\n",
    );
    for (const [name, text] of Object.entries(sources))
      fs.writeFileSync(path.join(directory, name), text);
    build();
    opened = new Map();
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
  function build() {
    execFileSync("dune", ["build", "--root", directory, "@check"], {
      stdio: "pipe",
    });
  }
  function uri(name: string) {
    return URI.file(path.resolve(directory, name)).toString();
  }
  function open(name: string, text?: string) {
    if (opened.has(name)) return;
    text = text ?? fs.readFileSync(path.resolve(directory, name), "utf8");
    opened.set(name, text);
    server.sendNotification(Protocol.DidOpenTextDocumentNotification.type, {
      textDocument: Types.TextDocumentItem.create(uri(name), "ocaml", 0, text),
    });
  }
  function change(name: string, text: string) {
    open(name);
    opened.set(name, text);
    server.sendNotification(Protocol.DidChangeTextDocumentNotification.type, {
      textDocument: { uri: uri(name), version: 1 },
      contentChanges: [{ text }],
    });
  }
  async function prepare(name: string, needle: string, inside = 0) {
    open(name);
    const text = opened.get(name)!;
    const index = text.indexOf(needle);
    expect(index).toBeGreaterThanOrEqual(0);
    const lines = text.slice(0, index + inside).split("\n");
    return (
      (await server.sendRequest<Protocol.CallHierarchyItem[] | null>(
        "textDocument/prepareCallHierarchy",
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
  function incoming(item: Protocol.CallHierarchyItem) {
    return server.sendRequest<Protocol.CallHierarchyIncomingCall[] | null>(
      "callHierarchy/incomingCalls",
      { item },
    );
  }
  function outgoing(item: Protocol.CallHierarchyItem) {
    return server.sendRequest<Protocol.CallHierarchyOutgoingCall[] | null>(
      "callHierarchy/outgoingCalls",
      { item },
    );
  }

  it("groups incoming calls across closed files and excludes function values", async () => {
    const [item] = await prepare("a.mli", "target");
    expect(item.name).toBe("target");
    expect(item.uri).toBe(uri("a.ml"));
    const calls = (await incoming(item))!;
    const names = calls.map((call) => call.from.name);
    expect(names).toEqual(
      expect.arrayContaining([
        "caller",
        "piped",
        "applied",
        "recursor",
        "local",
        "closure",
        "B.<init>",
      ]),
    );
    expect(names).not.toContain("passed");
    expect(names).not.toContain("outer");
    const caller = calls.find((call) => call.from.name === "caller")!;
    expect(caller.fromRanges).toEqual([
      Types.Range.create(1, 17, 1, 23),
      Types.Range.create(1, 34, 1, 40),
    ]);
    expect(caller.from.uri).toBe(uri("b.ml"));
  });

  it("expands outgoing items into a closed callee and handles recursion", async () => {
    const [item] = await prepare("b.ml", "caller");
    const calls = (await outgoing(item))!;
    expect(calls.map((call) => call.to.name)).toContain("target");
    const target = calls.find((call) => call.to.name === "target")!;
    expect(target.fromRanges).toHaveLength(2);
    expect(
      (await incoming(target.to))!.some((call) => call.from.name === "caller"),
    ).toBe(true);
    const [recursor] = await prepare("a.ml", "recursor");
    const recursive = (await outgoing(recursor))!.find(
      (call) => call.to.name === "recursor",
    )!;
    expect(recursive.fromRanges).toHaveLength(1);
  });

  it("keeps local functions and callbacks separate", async () => {
    const [outer] = await prepare("c.ml", "outer");
    const outerCalls = (await outgoing(outer))!;
    expect(
      outerCalls
        .filter((call) => call.to.uri === uri("c.ml"))
        .map((call) => call.to.name),
    ).toEqual(["local", "closure"]);
    expect(outerCalls.map((call) => call.to.name)).not.toContain("target");
    const [callback] = await prepare("c.ml", "callback");
    expect(await outgoing(callback)).toEqual([]);
    const [shadowed] = await prepare("c.ml", "shadowed");
    const calls = (await outgoing(shadowed))!;
    expect(calls.map((call) => call.to.name)).toEqual(["target"]);
    expect(calls[0].to.uri).toBe(uri("c.ml"));
  });

  it("replaces disk calls with unsaved buffers and rejects stale items", async () => {
    const [old] = await prepare("a.ml", "target");
    change("b.ml", "let replacement x = A.target x\n");
    await expect(incoming(old)).rejects.toMatchObject({ code: -32801 });
    const [fresh] = await prepare("a.ml", "target");
    const names = (await incoming(fresh))!.map((call) => call.from.name);
    expect(names).toContain("replacement");
    expect(names).not.toContain("caller");
  });

  it("does not expand dependency bodies, but finds project callers of library functions", async () => {
    change("b.ml", 'let fail () = Stdune.Code_error.raise "boom" []\n');
    const [fail] = await prepare("b.ml", "fail");
    const calls = (await outgoing(fail))!;
    const raise = calls.find((call) => call.to.name === "raise")!;
    expect(raise).toBeDefined();
    expect(raise.to.uri).toContain("/lib/stdune/code_error.");
    expect(await outgoing(raise.to)).toEqual([]);
    expect((await incoming(raise.to))!.map((call) => call.from.name)).toContain(
      "fail",
    );
  });

  it("rejects invalid data", async () => {
    const [item] = await prepare("a.ml", "target");
    await expect(incoming({ ...item, data: {} })).rejects.toMatchObject({
      code: -32602,
    });
  });
  it("recognizes partial calls and standard operators without guessing shadowed operators", async () => {
    change(
      "b.ml",
      "let partial x = List.fold_left ( + ) x\nlet ( |> ) x f = ignore f; x\nlet custom x = x |> A.target\nlet standard x = Stdlib.( |> ) x A.target\n",
    );
    const [target] = await prepare("a.ml", "target");
    const names = (await incoming(target))!.map((call) => call.from.name);
    expect(names).toContain("standard");
    expect(names).not.toContain("custom");
    const [partial] = await prepare("b.ml", "partial");
    expect((await outgoing(partial))!.map((call) => call.to.name)).toContain(
      "fold_left",
    );
  });

  it("attributes anonymous callback bodies to their own callable", async () => {
    change("b.ml", "let map xs = List.map (fun x -> A.target x) xs\n");
    const [target] = await prepare("a.ml", "target");
    const calls = (await incoming(target))!;
    expect(calls.some((call) => call.from.name.startsWith("<fun at"))).toBe(
      true,
    );
    expect(calls.map((call) => call.from.name)).not.toContain("map");
  });

  it("uses UTF-16 ranges for closed files containing Unicode", async () => {
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      'let caller x = let _ = "😀" in A.target x\n',
    );
    build();
    const [target] = await prepare("a.ml", "target");
    const caller = (await incoming(target))!.find(
      (call) => call.from.name === "caller",
    )!;
    const text = fs.readFileSync(path.join(directory, "b.ml"), "utf8");
    expect(caller.fromRanges[0]).toEqual(
      Types.Range.create(
        0,
        text.indexOf("target"),
        0,
        text.indexOf("target") + 6,
      ),
    );
    expect(caller.from.range.start).toEqual(Types.Position.create(0, 0));
  });

  it("skips stale closed-file calls and refreshes after a manual build", async () => {
    const [old] = await prepare("a.ml", "target");
    fs.writeFileSync(
      path.join(directory, "b.ml"),
      "let rebuilt x = A.target x\n",
    );
    await expect(incoming(old)).rejects.toMatchObject({ code: -32801 });
    const [fresh] = await prepare("a.ml", "target");
    expect(
      (await incoming(fresh))!.map((call) => call.from.name),
    ).not.toContain("caller");
    build();
    const [rebuilt] = await prepare("a.ml", "target");
    expect((await incoming(rebuilt))!.map((call) => call.from.name)).toContain(
      "rebuilt",
    );
  });

  it("keeps same-named compilation units in different workspaces separate", async () => {
    const other = path.join(directory, "other");
    fs.mkdirSync(other);
    fs.writeFileSync(path.join(other, "dune-project"), "(lang dune 3.0)\n");
    fs.writeFileSync(
      path.join(other, "dune"),
      "(library (name other) (wrapped false))\n",
    );
    fs.writeFileSync(path.join(other, "a.ml"), "let target x = x\n");
    fs.writeFileSync(path.join(other, "b.ml"), "let stranger x = A.target x\n");
    execFileSync("dune", ["build", "--root", other, "@check"], {
      stdio: "pipe",
    });
    server.sendNotification(
      Protocol.DidChangeWorkspaceFoldersNotification.type,
      {
        event: {
          added: [{ name: "other", uri: URI.file(other).toString() }],
          removed: [],
        },
      },
    );
    const [target] = await prepare("a.ml", "target");
    expect(
      (await incoming(target))!.map((call) => call.from.name),
    ).not.toContain("stranger");
  });

  it("cancels prepare requests and remains usable", async () => {
    open("a.ml");
    const source = new CancellationTokenSource();
    const request = server.sendRequest(
      "textDocument/prepareCallHierarchy",
      {
        textDocument: { uri: uri("a.ml") },
        position: Types.Position.create(0, 4),
      },
      source.token,
    );
    source.cancel();
    await expect(request).rejects.toMatchObject({ code: -32800 });
    expect(await prepare("a.ml", "target")).toHaveLength(1);
    source.dispose();
  });
  it("keeps deferred lazy and object method bodies out of their enclosing function", async () => {
    change(
      "b.ml",
      "let make x = (lazy (A.target x), object method run = A.target x end)\nlet dynamic obj = obj#run\n",
    );
    const [target] = await prepare("a.ml", "target");
    const names = (await incoming(target))!.map((call) => call.from.name);
    expect(names.some((name) => name.startsWith("<lazy at"))).toBe(true);
    expect(names).toContain("run");
    expect(names).not.toContain("make");
    const [dynamic] = await prepare("b.ml", "dynamic");
    expect(await outgoing(dynamic)).toEqual([]);
  });
});
