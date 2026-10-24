import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/references", () => {
  let languageServer: LanguageServer.LanguageServer;

  function openDocument(source: string, uri = "file:///test.ml") {
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(uri, "ocaml", 0, source),
      },
    );
  }

  async function query(
    position: Types.Position,
    includeDeclaration = false,
    uri = "file:///test.ml",
  ) {
    return await languageServer.sendRequest(Protocol.ReferencesRequest.type, {
      textDocument: Types.TextDocumentIdentifier.create(uri),
      position,
      context: { includeDeclaration },
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("finds references in a file", async () => {
    openDocument(outdent`
      let num = 42
      let sum = num + 13
      let sum2 = sum + num
    `);

    let result = await query(Types.Position.create(0, 4));

    expect(result).toMatchInlineSnapshot(`
Array [
  Object {
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
    "uri": "file:///test.ml",
  },
  Object {
    "range": Object {
      "end": Object {
        "character": 20,
        "line": 2,
      },
      "start": Object {
        "character": 17,
        "line": 2,
      },
    },
    "uri": "file:///test.ml",
  },
]
`);
  });

  it.each([
    {
      name: "a value",
      source: "let value = 42\nlet result = value + value",
      declaration: [0, 4, 9],
      usage: [1, 13],
      count: 2,
    },
    {
      name: "a function parameter",
      source: "let f parameter = parameter + parameter",
      declaration: [0, 6, 15],
      usage: [0, 18],
      count: 2,
    },
    {
      name: "an alias pattern",
      source: "let f (value as alias) = alias + value",
      declaration: [0, 16, 21],
      usage: [0, 25],
      count: 1,
    },
    {
      name: "a constructor",
      source:
        "type t = Constructor\nlet f = function Constructor -> Constructor",
      declaration: [0, 9, 20],
      usage: [1, 17],
      count: 2,
    },
    {
      name: "a type",
      source: "type thing = int\nlet f (x : thing) : thing = x",
      declaration: [0, 5, 10],
      usage: [1, 11],
      count: 2,
    },
    {
      name: "an unused binding",
      source: "let unused = 42",
      declaration: [0, 4, 10],
      usage: null,
      count: 0,
    },
    {
      name: "a loop variable",
      source: "let () = for index = 0 to 3 do ignore index done",
      declaration: [0, 13, 18],
      usage: [0, 41],
      count: 1,
    },
    {
      name: "a recursive binding",
      source: "let rec recur x = if x = 0 then 0 else recur (x - 1)",
      declaration: [0, 8, 13],
      usage: [0, 39],
      count: 1,
    },
    {
      name: "a local module",
      source:
        "let f () = let module Local = struct let x = 1 end in let open Local in x",
      declaration: [0, 22, 27],
      usage: [0, 64],
      count: 1,
    },
    {
      name: "a functor parameter",
      source: "module F (Arg : sig end) = struct open Arg end",
      declaration: [0, 10, 13],
      usage: [0, 39],
      count: 1,
    },
    {
      name: "an exception",
      source:
        "exception Failure\nlet f = function Failure -> raise Failure | exn -> raise exn",
      declaration: [0, 10, 17],
      usage: [1, 17],
      count: 2,
    },
  ])(
    "honours includeDeclaration for $name",
    async ({ source, declaration, usage, count }) => {
      openDocument(source);
      const [line, start, end] = declaration;
      const declarationLocation = Types.Location.create(
        "file:///test.ml",
        Types.Range.create(line, start, line, end),
      );
      // Both a query on the declaration and a query on a use must have the same result.
      for (const [queryLine, queryCharacter] of [
        [line, start],
        ...(usage ? [usage] : []),
      ]) {
        const position = Types.Position.create(queryLine, queryCharacter);
        const without = await query(position, false);
        const withDeclaration = await query(position, true);
        expect(without).toHaveLength(count);
        expect(without).not.toContainEqual(declarationLocation);
        expect(withDeclaration).toHaveLength(count + 1);
        expect(withDeclaration).toContainEqual(declarationLocation);
        expect(withDeclaration).toEqual(expect.arrayContaining(without ?? []));
      }
    },
  );

  it("keeps shadowed bindings separate", async () => {
    openDocument(
      "let value = 1\nlet first = value\nlet value = 2\nlet second = value",
    );
    const first = await query(Types.Position.create(1, 12));
    const second = await query(Types.Position.create(3, 13));
    expect(first?.map((location) => location.range)).toEqual([
      Types.Range.create(1, 12, 1, 17),
    ]);
    expect(second?.map((location) => location.range)).toEqual([
      Types.Range.create(3, 13, 3, 18),
    ]);
  });

  it("keeps a type reference inside an annotated binding pattern", async () => {
    openDocument(
      "type thing = int\nlet (value : thing) = 1\nlet result : thing = value",
    );
    const result = await query(Types.Position.create(0, 5));
    expect(result?.map((location) => location.range)).toEqual([
      Types.Range.create(1, 13, 1, 18),
      Types.Range.create(2, 13, 2, 18),
    ]);
  });

  it("distinguishes a record pun binding from the record label", async () => {
    openDocument("type t = { value : int }\nlet f { value } = value");
    expect(
      (await query(Types.Position.create(1, 8)))?.map(
        (location) => location.range,
      ),
    ).toEqual([Types.Range.create(1, 18, 1, 23)]);
  });

  it("excludes both binding sites of an or-pattern", async () => {
    openDocument(
      "let f = function Some value | Some value -> value | None -> 0",
    );
    const position = Types.Position.create(0, 44);
    expect((await query(position))?.map((location) => location.range)).toEqual([
      Types.Range.create(0, 44, 0, 49),
    ]);
    expect(
      (await query(position, true))?.map((location) => location.range),
    ).toEqual([
      Types.Range.create(0, 22, 0, 27),
      Types.Range.create(0, 35, 0, 40),
      Types.Range.create(0, 44, 0, 49),
    ]);
  });

  it("excludes interface declarations", async () => {
    const uri = "file:///test.mli";
    openDocument("type thing\nval value : thing", uri);
    expect(
      (await query(Types.Position.create(0, 5), false, uri))?.map(
        (location) => location.range,
      ),
    ).toEqual([Types.Range.create(1, 12, 1, 17)]);
    expect(await query(Types.Position.create(1, 4), false, uri)).toEqual([]);
    expect(await query(Types.Position.create(1, 4), true, uri)).toEqual([
      Types.Location.create(uri, Types.Range.create(1, 4, 1, 9)),
    ]);
  });
});
