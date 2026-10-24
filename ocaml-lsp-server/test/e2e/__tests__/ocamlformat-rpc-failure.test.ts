import * as LanguageServer from "../src/LanguageServer";
import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

const maybeDescribe = os.type() === "Windows_NT" ? describe.skip : describe;

maybeDescribe("ocamlformat-rpc startup failure", () => {
  let languageServer: LanguageServer.LanguageServer;
  let directory: string;

  afterEach(async () => {
    if (languageServer) await LanguageServer.exit(languageServer);
    fs.rmSync(directory, { recursive: true, force: true });
  });

  it.each(["exit 1", "printf 'invalid protocol'; exit 1"])(
    "keeps hover working after %s",
    async (script) => {
      directory = fs.mkdtempSync(
        path.join(os.tmpdir(), "ocamllsp-rpc-failure-"),
      );
      fs.writeFileSync(
        path.join(directory, "ocamlformat-rpc"),
        "#!/bin/sh\n" + script + "\n",
        { mode: 0o700 },
      );
      languageServer = LanguageServer.start({
        env: {
          ...process.env,
          OCAMLLSP_TEST: "true",
          PATH: directory + path.delimiter + process.env.PATH,
        },
      });
      await languageServer.sendRequest(Protocol.InitializeRequest.type, {
        processId: process.pid,
        rootUri: null,
        workspaceFolders: [],
        capabilities: {},
      });
      const uri = "file:///rpc-failure.ml";
      languageServer.sendNotification(
        Protocol.DidOpenTextDocumentNotification.type,
        {
          textDocument: Types.TextDocumentItem.create(
            uri,
            "ocaml",
            0,
            "let value = 42",
          ),
        },
      );
      // The first request triggers negotiation; the second must still work
      // after the failed formatter has been marked unavailable.
      for (let attempt = 0; attempt < 2; attempt++) {
        const hover = await languageServer.sendRequest(
          Protocol.HoverRequest.type,
          {
            textDocument: { uri },
            position: Types.Position.create(0, 5),
          },
        );
        expect(hover?.contents).toMatchObject({
          kind: "plaintext",
          value: "int",
        });
      }
    },
  );
});
