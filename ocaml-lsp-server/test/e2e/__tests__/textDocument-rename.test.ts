import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/rename", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(source: string) {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(
          "file:///test.ml",
          "ocaml",
          0,
          source,
        ),
      },
    );
  }

  async function query(position: Types.Position, newNameOpt?: string) {
    let newName = newNameOpt ? newNameOpt : "new_num";
    return await languageServer.sendRequest(Protocol.RenameRequest.type, {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position,
      newName,
    });
  }

  async function query_prepare(position: Types.Position) {
    return await languageServer.sendRequest(
      Protocol.PrepareRenameRequest.type,
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        position,
      },
    );
  }

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it.each([0, 1, 2])(
    "renames an operator from line %i while preserving its parentheses",
    async (line) => {
      languageServer = await LanguageServer.startAndInitialize();
      const lines = [
        "let ( ++ ) a b = a + b",
        "let result = ( ++ ) 1 2",
        "let other = 1 ++ 2",
      ];
      const source = lines.join("\n");
      openDocument(source);
      const column = lines[line].indexOf("++");
      const position = Types.Position.create(line, column);
      expect(await query_prepare(position)).toEqual(
        Types.Range.create(line, column, line, column + 2),
      );
      const result = await query(position, "**");
      const edits = result?.changes?.["file:///test.ml"] ?? [];
      expect(edits).toHaveLength(3);
      for (const edit of edits) {
        expect(edit.range.end.line).toBe(edit.range.start.line);
        expect(edit.range.end.character - edit.range.start.character).toBe(2);
        const { line: editLine, character } = edit.range.start;
        lines[editLine] =
          lines[editLine].slice(0, character) +
          edit.newText +
          lines[editLine].slice(edit.range.end.character);
      }
      expect(lines.join("\n")).toBe(source.replace(/\+\+/g, "**"));
    },
  );

  it("renames a completed binding before an incomplete one", async () => {
    languageServer = await LanguageServer.startAndInitialize();
    openDocument("let rec ma x = ma x\nlet broken =");
    const result = await query(Types.Position.create(0, 9), "renamed");
    expect(result?.changes?.["file:///test.ml"]).toEqual([
      Types.TextEdit.replace(Types.Range.create(0, 8, 0, 10), "renamed"),
      Types.TextEdit.replace(Types.Range.create(0, 15, 0, 17), "renamed"),
    ]);
  });

  it("can reject invalid rename request", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query_prepare(Types.Position.create(0, 1));
    expect(result).toBeNull();
  });

  it("allows valid rename request", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query_prepare(Types.Position.create(0, 4));
    expect(result).toMatchInlineSnapshot(`
Object {
  "end": Object {
    "character": 7,
    "line": 0,
  },
  "start": Object {
    "character": 4,
    "line": 0,
  },
}
`);
  });

  it("rename value in a file without documentChanges capability", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query(Types.Position.create(0, 4));

    expect(result).toMatchInlineSnapshot(`
Object {
  "changes": Object {
    "file:///test.ml": Array [
      Object {
        "newText": "new_num",
        "range": Object {
          "end": Object {
            "character": 7,
            "line": 0,
          },
          "start": Object {
            "character": 4,
            "line": 0,
          },
        },
      },
      Object {
        "newText": "new_num",
        "range": Object {
          "end": Object {
            "character": 13,
            "line": 1,
          },
          "start": Object {
            "character": 10,
            "line": 1,
          },
        },
      },
    ],
  },
}
`);
  });

  it("rename value in a file with documentChanges capability", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: true } },
      },
    });

    openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query(Types.Position.create(0, 4));

    expect(result).toMatchInlineSnapshot(`
Object {
  "documentChanges": Array [
    Object {
      "edits": Array [
        Object {
          "newText": "new_num",
          "range": Object {
            "end": Object {
              "character": 7,
              "line": 0,
            },
            "start": Object {
              "character": 4,
              "line": 0,
            },
          },
        },
        Object {
          "newText": "new_num",
          "range": Object {
            "end": Object {
              "character": 13,
              "line": 1,
            },
            "start": Object {
              "character": 10,
              "line": 1,
            },
          },
        },
      ],
      "textDocument": Object {
        "uri": "file:///test.ml",
        "version": 0,
      },
    },
  ],
}
`);
  });

  it("rename a var used as a named argument", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
let foo x = x

let bar ~foo = foo ()

let () = bar ~foo
    `);

    let result = await query(Types.Position.create(0, 4), "ident");

    expect(result).toMatchInlineSnapshot(`
      Object {
        "changes": Object {
          "file:///test.ml": Array [
            Object {
              "newText": "ident",
              "range": Object {
                "end": Object {
                  "character": 7,
                  "line": 0,
                },
                "start": Object {
                  "character": 4,
                  "line": 0,
                },
              },
            },
            Object {
              "newText": ":ident",
              "range": Object {
                "end": Object {
                  "character": 17,
                  "line": 4,
                },
                "start": Object {
                  "character": 17,
                  "line": 4,
                },
              },
            },
          ],
        },
      }
    `);
  });

  it("rename a var used as a named argument", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      capabilities: {
        workspace: { workspaceEdit: { documentChanges: false } },
      },
    });

    openDocument(outdent`
let foo = Some ()

let bar ?foo () = foo

;;
ignore (bar ?foo ())
    `);

    let result = await query(Types.Position.create(0, 4), "sunit");

    expect(result).toMatchInlineSnapshot(`
      Object {
        "changes": Object {
          "file:///test.ml": Array [
            Object {
              "newText": "sunit",
              "range": Object {
                "end": Object {
                  "character": 7,
                  "line": 0,
                },
                "start": Object {
                  "character": 4,
                  "line": 0,
                },
              },
            },
            Object {
              "newText": ":sunit",
              "range": Object {
                "end": Object {
                  "character": 16,
                  "line": 5,
                },
                "start": Object {
                  "character": 16,
                  "line": 5,
                },
              },
            },
          ],
        },
      }
    `);
  });
});
