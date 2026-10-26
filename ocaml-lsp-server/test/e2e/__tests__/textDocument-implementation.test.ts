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
  let opened: Set<string>;

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
    opened = new Set();
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
    if (opened.has(name)) return;
    opened.add(name);
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
  it.each([
    ["val", "val f : int -> int\n", "let f x = x + 1\n", "f :", 0, "f x"],
    [
      "nested val",
      "module M : sig val f : int -> int end\n",
      "module M = struct let f x = x end\n",
      "f :",
      0,
      "f x",
    ],
    [
      "module",
      "module M : sig val f : int end\n",
      "module M = struct let f = 1 end\n",
      "M :",
      0,
      "M =",
    ],
    [
      "functor value",
      "module F : functor (X : sig end) -> sig val f : int end\n",
      "module F (X : sig end) = struct let f = 1 end\n",
      "f :",
      0,
      "f =",
    ],
  ])(
    "resolves %s declarations",
    async (_title, intf, impl, needle, inside, target) => {
      await setup({ "lib.mli": intf as string, "lib.ml": impl as string });
      const result = await query("lib.mli", needle as string, inside as number);
      expect(result).toHaveLength(1);
      expect(result[0].uri).toBe(uri("lib.ml"));
      expect(result[0].range.start.character).toBe(
        (impl as string).indexOf(target as string),
      );
    },
  );

  it("resolves a use through its declaration into an unsaved implementation", async () => {
    await setup({
      "lib.mli": "val f : int -> int\n",
      "lib.ml": "let f x = x\n",
      "main.ml": "module Alias = Lib\nlet x = Alias.f 1\n",
    });
    open("lib.ml", "\n\nlet f x = x + 1\n");
    const result = await query("main.ml", "Alias.f", 6);
    expect(result).toEqual([
      Types.Location.create(uri("lib.ml"), Types.Range.create(2, 4, 2, 5)),
    ]);
  });

  it("resolves functions in a module without an interface", async () => {
    await setup({ "lib.ml": "let f x = x\n", "main.ml": "let x = Lib.f 1\n" });
    expect((await query("main.ml", "Lib.f", 4))[0].uri).toBe(uri("lib.ml"));
  });

  it("returns no implementation for val-only modules", async () => {
    await setup({
      "only.mli": "val f : int -> int\n",
      "main.ml": "let x = Only.f 1\n",
    });
    expect(await query("only.mli", "f :")).toEqual([]);
    expect(await query("main.ml", "Only.f", 5)).toEqual([]);
  });
  it("resolves a cross-file included type to its actual body", async () => {
    await setup({
      "base.mli": "type t\n",
      "base.ml": "type t = int\n",
      "lib.mli": "type t\n",
      "lib.ml": "include Base\n",
    });
    const result = await query("lib.mli", "type t", 5);
    expect(result).toHaveLength(1);
    expect(result[0].uri).toBe(uri("base.ml"));
  });

  it("does not mistake an interface-only include for an implementation", async () => {
    await setup({
      "only.mli": "type t\n",
      "lib.mli": "type t\n",
      "lib.ml": "include Only\n",
    });
    expect(await query("lib.mli", "type t", 5)).toEqual([]);
  });

  it("resolves a cross-file functor alias result", async () => {
    await setup({
      "base.mli": "module F : functor (X : sig end) -> sig type t end\n",
      "base.ml": "module F (X : sig end) = struct type t = int end\n",
      "lib.mli": "module F : functor (X : sig end) -> sig type t end\n",
      "lib.ml": "module F = Base.F\n",
    });
    const result = await query("lib.mli", "t end");
    expect(result).toHaveLength(1);
    expect(result[0].uri).toBe(uri("base.ml"));
  });

  it("does not find an outer type when an unsaved functor result no longer has it", async () => {
    await setup({
      "lib.mli": "type t\nmodule F : functor () -> sig type t end\n",
      "lib.ml": "type t = int\nmodule F () = struct type t = int end\n",
    });
    open("lib.ml", "type t = int\nmodule F () = struct let x = 1 end\n");
    expect(await query("lib.mli", "t end")).toEqual([]);
  });
  it("keeps bodies implementing the same signature distinct", async () => {
    await setup({
      "lib.mli":
        "module F : sig val f : int -> int end\nmodule G : sig val f : int -> int end\n",
      "lib.ml":
        "module type S = sig val f : int -> int end\nmodule F : S = struct let f x = x + 1 end\nmodule G : S = struct let f x = x + 2 end\n",
      "main.ml": "let a = Lib.F.f 1\nlet b = Lib.G.f 2\n",
    });
    const f = await query("main.ml", "Lib.F.f", 6);
    const g = await query("main.ml", "Lib.G.f", 6);
    expect(f[0].range.start.line).toBe(1);
    expect(g[0].range.start.line).toBe(2);
    const fromDeclaration = await query("lib.mli", "f :");
    expect(fromDeclaration[0].range.start.line).toBe(1);
  });
});
