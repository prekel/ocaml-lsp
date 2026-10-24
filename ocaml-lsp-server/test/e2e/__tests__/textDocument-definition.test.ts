import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";
import * as Types from "vscode-languageserver-types";
import { testUri } from "./../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import { isNotNullable } from "../src/utils";

describe("textDocument/definition", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(source: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          testUri("test.ml"),
          "ocaml",
          0,
          source,
        ),
      },
    );
  }

  async function queryDefinition(position: Types.Position) {
    let result = await languageServer.sendRequest(
      Protocol.DefinitionRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create(testUri("test.ml")),
        position,
      },
    );

    if (result === null) return [];

    result = Array.isArray(result) ? result : [result];

    return result
      .map((location) => (Types.Location.is(location) ? location : null))
      .filter(isNotNullable);
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("returns location of a definition", async () => {
    openDocument(outdent`
      let x = 43

      let () =
        print_int x
    `);

    let result = await queryDefinition(Types.Position.create(3, 12));

    expect(result.length).toBe(1);
    expect(result[0].range).toMatchObject({
      end: { character: 5, line: 0 },
      start: { character: 4, line: 0 },
    });
    expect(result[0].uri).toEqualUri(testUri("test.ml"));
  });

  it("selects the whole definition name", async () => {
    openDocument("let long_name = 43\nlet result = long_name");
    const result = await queryDefinition(Types.Position.create(1, 14));
    expect(result.map((location) => location.range)).toEqual([
      Types.Range.create(0, 4, 0, 13),
    ]);
  });

  it("selects a constructor name", async () => {
    openDocument("type t = Constructor\nlet result = Constructor");
    const result = await queryDefinition(Types.Position.create(1, 14));
    expect(result.map((location) => location.range)).toEqual([
      Types.Range.create(0, 9, 0, 20),
    ]);
  });

  it("selects a declaration name", async () => {
    openDocument("let long_name = 43\nlet result = long_name");
    const result = await languageServer.sendRequest(
      Protocol.DeclarationRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create(testUri("test.ml")),
        position: Types.Position.create(1, 14),
      },
    );
    expect(result).toEqual([
      Types.Location.create(
        testUri("test.ml"),
        Types.Range.create(0, 4, 0, 13),
      ),
    ]);
  });

  it.each([
    {
      name: "a builtin",
      source: "let f (x : int) = x",
      position: Types.Position.create(0, 12),
    },
    {
      name: "an unresolved identifier",
      source: "let x = missing",
      position: Types.Position.create(0, 10),
    },
    {
      name: "whitespace",
      source: "\n\nlet x = 1",
      position: Types.Position.create(0, 0),
    },
    {
      name: "a definition itself",
      source: "let x = 1",
      position: Types.Position.create(0, 4),
    },
  ])(
    "returns no target without an RPC error for $name",
    async ({ source, position }) => {
      openDocument(source);
      expect(await queryDefinition(position)).toEqual([]);
    },
  );
});
