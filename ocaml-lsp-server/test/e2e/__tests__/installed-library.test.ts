import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { execFileSync } from "child_process";
import { URI } from "vscode-uri";

jest.setTimeout(30000);
const maybeDescribe = os.type() === "Windows_NT" ? describe.skip : describe;

maybeDescribe("installed library sources", () => {
  let template: string;
  let directory: string;
  let prefix: string;
  let server: LanguageServer.LanguageServer;

  beforeAll(() => {
    template = fs.mkdtempSync(
      path.join(os.tmpdir(), "ocamllsp-installed-template-"),
    );
    const source = path.join(template, "source");
    fs.mkdirSync(path.join(source, "dep"), { recursive: true });
    fs.mkdirSync(path.join(source, "lib"));
    fs.writeFileSync(
      path.join(source, "dune-project"),
      "(lang dune 3.0)\n(package (name installed_reading))\n",
    );
    fs.writeFileSync(
      path.join(source, "dep/dune"),
      "(library (name dep) (public_name installed_reading.dep) (wrapped false))\n",
    );
    fs.writeFileSync(
      path.join(source, "dep/dep.ml"),
      'let label = "dependency"\n',
    );
    fs.writeFileSync(
      path.join(source, "pp.sh"),
      '#!/bin/sh\nprintf \'# 1 "%s"\\n\' "$1"\ncat "$1"\n',
      { mode: 0o755 },
    );
    fs.writeFileSync(
      path.join(source, "lib/dune"),
      `(library (name installed_reading) (public_name installed_reading) (libraries installed_reading.dep) (flags (:standard -pp ${JSON.stringify(
        path.join(source, "pp.sh"),
      )})))\n`,
    );
    fs.writeFileSync(
      path.join(source, "lib/helper.ml"),
      "type t = Number of int\nlet number = Number 42\n",
    );
    fs.writeFileSync(
      path.join(source, "lib/library.ml"),
      "let value = Helper.number\nlet label = Dep.label\n",
    );
    fs.writeFileSync(
      path.join(source, "lib/library.mli"),
      "val value : Helper.t\nval label : string\n",
    );
    execFileSync("dune", ["build", "--root", source, "@install"], {
      stdio: "pipe",
    });
    execFileSync(
      "dune",
      ["install", "--root", source, "--prefix", path.join(template, "switch")],
      { stdio: "pipe" },
    );
    // The installed environment must work without the original build tree.
    fs.rmSync(source, { recursive: true, force: true });
  });

  afterAll(() => fs.rmSync(template, { recursive: true, force: true }));

  beforeEach(async () => {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), "ocamllsp-installed-"));
    prefix = path.join(directory, "_opam");
    fs.cpSync(path.join(template, "switch"), prefix, { recursive: true });
    fs.mkdirSync(path.join(prefix, ".opam-switch"));
    // Reproduce a local switch nested inside an unrelated Dune workspace.
    fs.writeFileSync(path.join(directory, "dune-project"), "(lang dune 3.0)\n");
    server = await LanguageServer.startAndInitialize({
      rootUri: URI.file(directory).toString(),
      workspaceFolders: [
        { name: "workspace", uri: URI.file(directory).toString() },
      ],
    });
  });

  afterEach(async () => {
    if (server) await LanguageServer.exit(server);
    if (directory) fs.rmSync(directory, { recursive: true, force: true });
  });

  function filename(name = "library.ml") {
    return path.join(prefix, "lib/installed_reading", name);
  }

  function open(file = filename(), text = fs.readFileSync(file, "utf8")) {
    const uri = URI.file(file).toString();
    server.sendNotification(Protocol.DidOpenTextDocumentNotification.type, {
      textDocument: Types.TextDocumentItem.create(uri, "ocaml", 0, text),
    });
    return { uri, text };
  }

  async function config(uri: string) {
    const response = await server.sendRequest<{ result: string }>(
      "ocamllsp/merlinCallCompatible",
      { uri, resultAsSexp: false, command: "dump-configuration", args: [] },
    );
    const result = JSON.parse(response.result);
    expect(result.class).toBe("return");
    return result.value;
  }

  async function hover(uri: string, text: string, needle: string) {
    const offset = text.indexOf(needle);
    expect(offset).toBeGreaterThanOrEqual(0);
    const lines = text.slice(0, offset).split("\n");
    return server.sendRequest(Protocol.HoverRequest.type, {
      textDocument: { uri },
      position: Types.Position.create(
        lines.length - 1,
        lines[lines.length - 1].length,
      ),
    });
  }

  it("restores the wrapped unit, installed dependencies and typing options", async () => {
    const { uri, text } = open();
    const settings = await config(uri);
    expect(settings.merlin.unit_name).toBe("Installed_reading__Library");
    expect(settings.ocaml.open_modules).toContain("Installed_reading");
    expect(settings.ocaml.pp).toBeNull();
    expect(settings.ocaml.ppx).toEqual([]);
    expect(settings.merlin.build_path).toContain(
      path.join(prefix, "lib/installed_reading/dep"),
    );
    expect(settings.merlin.failures).toEqual([]);
    expect(JSON.stringify(await hover(uri, text, "number"))).toContain(
      "Helper.t",
    );
    expect(JSON.stringify(await hover(uri, text, "Dep.label"))).toContain(
      "label : string",
    );
  });

  it("navigates to installed sibling modules", async () => {
    const { uri } = open();
    const result = await server.sendRequest(Protocol.DefinitionRequest.type, {
      textDocument: { uri },
      position: Types.Position.create(0, 21),
    });
    expect(JSON.stringify(result)).toContain(
      URI.file(filename("helper.ml")).toString(),
    );
  });

  it("selects dependencies by interface digest when module names collide", async () => {
    const { uri, text } = open();
    await config(uri); // Populate the directory index before installing another module.
    const other = path.join(prefix, "lib/zzz-unrelated");
    fs.mkdirSync(other);
    fs.writeFileSync(path.join(other, "dep.ml"), "let label = 123\n");
    execFileSync("ocamlc", ["-c", "dep.ml"], { cwd: other, stdio: "pipe" });
    expect((await config(uri)).merlin.build_path).not.toContain(other);
    expect(JSON.stringify(await hover(uri, text, "Dep.label"))).toContain(
      "label : string",
    );
  });

  it("discovers newly installed dependencies after populating the cache", async () => {
    const dependency = path.join(prefix, "lib/installed_reading/dep");
    const saved = path.join(directory, "saved-dep");
    fs.renameSync(dependency, saved);
    const { uri, text } = open();
    expect((await config(uri)).merlin.build_path).not.toContain(dependency);
    const restored = path.join(prefix, "lib/reinstalled-dep");
    fs.renameSync(saved, restored);
    expect((await config(uri)).merlin.build_path).toContain(restored);
    expect(JSON.stringify(await hover(uri, text, "Dep.label"))).toContain(
      "label : string",
    );
  });

  it("restores the environment of the installed Stdune.List implementation", async () => {
    const stdune = execFileSync("ocamlfind", ["query", "stdune"], {
      encoding: "utf8",
    }).trim();
    const { uri, text } = open(path.join(stdune, "list.ml"));
    const settings = await config(uri);
    expect(settings.merlin.unit_name).toBe("Stdune__List");
    expect(settings.ocaml.open_modules).toContain("Stdune__");
    expect(settings.merlin.failures).toEqual([]);
    expect(await hover(uri, text, "Code_error.raise")).not.toBeNull();
  });

  it("configures installed interfaces", async () => {
    const { uri, text } = open(filename("library.mli"));
    const settings = await config(uri);
    expect(settings.merlin.unit_name).toBe("Installed_reading__Library");
    expect(JSON.stringify(await hover(uri, text, "Helper.t"))).toContain(
      "Number of int",
    );
  });

  it("recognizes a named switch outside the workspace's local _opam", async () => {
    const global = path.join(directory, ".opam/named-switch");
    fs.mkdirSync(path.dirname(global));
    fs.renameSync(prefix, global);
    prefix = global;
    expect((await config(open().uri)).merlin.unit_name).toBe(
      "Installed_reading__Library",
    );
  });

  it("recognizes symlinked installed sources", async () => {
    const linked = path.join(directory, "linked.ml");
    fs.symlinkSync(filename(), linked);
    const { uri, text } = open(linked);
    expect((await config(uri)).merlin.unit_name).toBe(
      "Installed_reading__Library",
    );
    expect(JSON.stringify(await hover(uri, text, "number"))).toContain(
      "Helper.t",
    );
  });

  it.each(["truncated", "incompatible", "invalid marshal"])(
    "invalidates cached metadata after an artifact becomes %s and is restored",
    async (failure) => {
      const artifact = filename("installed_reading__Library.cmt");
      const contents = fs.readFileSync(artifact);
      const { uri } = open();
      expect((await config(uri)).merlin.unit_name).toBe(
        "Installed_reading__Library",
      );
      const corrupted =
        failure === "truncated"
          ? Buffer.from("corrupt artifact")
          : failure === "incompatible"
          ? Buffer.concat([Buffer.from("Caml1999T000"), contents.subarray(12)])
          : Buffer.concat([
              contents.subarray(0, 12),
              Buffer.from("invalid marshal"),
            ]);
      fs.writeFileSync(artifact, corrupted);
      expect((await config(uri)).merlin.unit_name).toBeNull();
      fs.writeFileSync(artifact, contents);
      expect((await config(uri)).merlin.unit_name).toBe(
        "Installed_reading__Library",
      );
    },
  );

  it("falls back when annotations are missing", async () => {
    for (const name of fs.readdirSync(path.dirname(filename()))) {
      if (/\.cmti?$/.test(name)) fs.unlinkSync(filename(name));
    }
    const { uri } = open();
    expect((await config(uri)).merlin.failures).toEqual([]);
    const tokens = await server.sendRequest(
      Protocol.SemanticTokensRequest.type,
      { textDocument: { uri } },
    );
    expect(tokens!.data.length).toBeGreaterThan(0);
  });

  it("does not mistake a project directory named lib for an installed library", async () => {
    const projectLib = path.join(directory, "lib");
    fs.mkdirSync(projectLib);
    const { uri } = open(path.join(projectLib, "local.ml"), "let x = 1\n");
    expect((await config(uri)).merlin.failures.join("\n")).toContain(
      "No config found",
    );
  });
});
