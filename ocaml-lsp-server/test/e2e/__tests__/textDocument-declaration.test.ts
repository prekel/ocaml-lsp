import { promises as fs } from "fs";
import * as path from "path";
import * as os from "os";
import * as child_process from "child_process";
import * as LanguageServer from "./../src/LanguageServer";
import * as Types from "vscode-languageserver-types";
import { testUri } from "./../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import { isNotNullable } from "../src/utils";

describe("textDocument/declaration", () => {
  let languageServer: LanguageServer.LanguageServer;

  let testWorkspacePath: string;

  let createPathForFile = (filename: string) =>
    path.join(testWorkspacePath, filename);

  beforeEach(async () => {
    testWorkspacePath = await fs.mkdtemp(
      path.join(os.tmpdir(), "ocamllsp-declaration-"),
    );
    for (const name of ["dune-project", "dune", "main.ml", "lib.ml", "lib.mli"])
      await fs.copyFile(
        path.join(__dirname, "declaration_files", name),
        path.join(testWorkspacePath, name),
      );
    languageServer = await LanguageServer.startAndInitialize({
      rootUri: testUri(testWorkspacePath),
      workspaceFolders: [{ name: "project", uri: testUri(testWorkspacePath) }],
    });
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    await fs.rm(testWorkspacePath, { recursive: true, force: true });
  });

  async function openDocument(filepath: string) {
    let source = await fs.readFile(filepath);
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          testUri(filepath),
          "ocaml",
          0,
          source.toString(),
        ),
      },
    );
  }

  async function queryDeclaration(filepath: string, position: Types.Position) {
    let result = await languageServer.sendRequest(
      Protocol.DeclarationRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create(testUri(filepath)),
        position,
      },
    );

    if (result === null) return [];

    result = Array.isArray(result) ? result : [result];

    return result
      .map((location) => (Types.Location.is(location) ? location : null))
      .filter(isNotNullable);
  }

  it("returns location of a declaration", async () => {
    child_process.execFileSync("dune", ["build", "--root", testWorkspacePath]);

    await openDocument(createPathForFile("main.ml"));

    let result = await queryDeclaration(
      createPathForFile("main.ml"),
      Types.Position.create(0, 13),
    );

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchInlineSnapshot(`
Object {
  "end": Object {
    "character": 5,
    "line": 0,
  },
  "start": Object {
    "character": 4,
    "line": 0,
  },
}
`);
    expect(result[0].uri).toEqualUri(testUri(createPathForFile("lib.mli")));
  });
});
