import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";
import { URI } from "vscode-uri";

const maybeDescribe = os.type() === "Windows_NT" ? describe.skip : describe;

maybeDescribe("textDocument/rangeFormatting", () => {
  let languageServer: LanguageServer.LanguageServer;
  let directory: string;

  beforeEach(async () => {
    directory = fs.mkdtempSync(path.join(os.tmpdir(), "ocamllsp-range-"));
    fs.writeFileSync(
      path.join(directory, ".ocamlformat"),
      "profile=janestreet\nmargin=40\n",
    );
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    fs.rmSync(directory, { recursive: true, force: true });
  });

  function fixture(marked: string, extension = ".ml") {
    const start = marked.indexOf("[[");
    const stop = marked.indexOf("]]");
    const before = marked.slice(0, start);
    const selected = marked.slice(start + 2, stop);
    const after = marked.slice(stop + 2);
    const source = before + selected + after;
    const position = (offset: number) => {
      const lines = source.slice(0, offset).split("\n");
      return Types.Position.create(
        lines.length - 1,
        lines[lines.length - 1].length,
      );
    };
    const range = Types.Range.create(
      position(start),
      position(start + selected.length),
    );
    const uri = URI.file(
      path.join(directory, "selection" + extension),
    ).toString();
    languageServer.sendNotification(
      Protocol.DidOpenTextDocumentNotification.type,
      {
        textDocument: Types.TextDocumentItem.create(uri, "ocaml", 0, source),
      },
    );
    const request = (requestedRange = range) =>
      languageServer.sendRequest(Protocol.DocumentRangeFormattingRequest.type, {
        textDocument: { uri },
        range: requestedRange,
        options: Types.FormattingOptions.create(2, true),
      });
    const format = async () => {
      const edits = await request();
      // Edits must be confined to the selection, even on a shared boundary line.
      expect(edits).not.toBeNull();
      expect(edits!.length).toBeLessThanOrEqual(1);
      for (const edit of edits!) expect(edit.range).toEqual(range);
      return before + (edits!.length ? edits![0].newText : selected) + after;
    };
    return { request, format, source };
  }

  it.each(["selection.ml", "*.ml", "*"])(
    "formats a selection ignored by %s",
    async (ignore) => {
      fs.writeFileSync(
        path.join(directory, ".ocamlformat-ignore"),
        ignore + "\n",
      );
      const test = fixture(
        "let unformatted=0\n[[let value=1+2]]\nlet untouched=3\n",
      );
      expect(await test.format()).toBe(
        "let unformatted=0\nlet value = 1 + 2\nlet untouched=3\n",
      );
    },
  );

  it("keeps full-document formatting disabled in ignored files", async () => {
    fs.writeFileSync(path.join(directory, ".ocamlformat-ignore"), "*\n");
    const uri = URI.file(path.join(directory, "selection.ml")).toString();
    fixture("[[let value=1+2]]\n");
    const edits = await languageServer.sendRequest(
      Protocol.DocumentFormattingRequest.type,
      {
        textDocument: { uri },
        options: Types.FormattingOptions.create(2, true),
      },
    );
    expect(edits).toEqual([]);
  });

  it("keeps trailing newlines and indentation when whole lines are selected", async () => {
    const test = fixture("let f () =\n[[  let value=1+2\n]]  in value\n");
    expect(await test.format()).toBe(
      "let f () =\n  let value = 1 + 2\n  in value\n",
    );
  });

  it("formats a nested let binding without inserting semicolons before in", async () => {
    const test = fixture("let f () =\n  [[let value=1+2]] in value\n");
    expect(await test.format()).toBe(
      "let f () =\n  let value = 1 + 2 in value\n",
    );
  });

  it("formats a fragment when the rest of the file has syntax errors", async () => {
    const test = fixture(
      "this is not valid OCaml !!\n[[let value=1+2]]\nlet broken =\n",
    );
    expect(await test.format()).toBe(
      "this is not valid OCaml !!\nlet value = 1 + 2\nlet broken =\n",
    );
  });

  it("preserves the effective project profile and explicit overrides", async () => {
    fs.writeFileSync(
      path.join(directory, ".ocamlformat"),
      "profile=janestreet\nbreak-cases=all\n",
    );
    fs.writeFileSync(path.join(directory, ".ocamlformat-ignore"), "*\n");
    const test = fixture("[[let f = function A -> 1 | B -> 2]]");
    expect(await test.format()).toBe(
      "let f = function\n  | A -> 1\n  | B -> 2\n;;",
    );
  });

  it("formats interface fragments", async () => {
    fs.writeFileSync(path.join(directory, ".ocamlformat-ignore"), "*\n");
    const test = fixture("[[val f:int->int]]\nval untouched:int\n", ".mli");
    expect(await test.format()).toBe("val f : int -> int\nval untouched:int\n");
  });

  it("uses UTF-16 selection positions without altering adjacent Unicode", async () => {
    const test = fixture('let emoji = "😀" in [[1+2]]');
    expect(await test.format()).toBe('let emoji = "😀" in 1 + 2');
  });

  it.each(["", " \n  "])(
    "leaves empty and whitespace selections unchanged: %j",
    async (selected) => {
      const test = fixture("let value = 1\n[[" + selected + "]]\n");
      expect(await test.request()).toEqual([]);
    },
  );

  it("leaves an already formatted selection unchanged", async () => {
    const test = fixture("[[let value = 1 + 2]]");
    expect(await test.request()).toEqual([]);
  });

  it("rejects reversed ranges and keeps serving requests", async () => {
    const test = fixture("[[let value=1+2]]");
    await expect(
      test.request(Types.Range.create(0, 8, 0, 2)),
    ).rejects.toMatchObject({ code: -32602 });
    expect(await test.format()).toBe("let value = 1 + 2");
  });

  it("reports invalid project configuration without silently using defaults", async () => {
    fs.writeFileSync(
      path.join(directory, ".ocamlformat"),
      "unknown-option=true\n",
    );
    const test = fixture("[[let value=1+2]]");
    await expect(test.request()).rejects.toMatchObject({ code: -32603 });
    fs.writeFileSync(
      path.join(directory, ".ocamlformat"),
      "profile=janestreet\n",
    );
    expect(await test.format()).toBe("let value = 1 + 2");
  });
});
