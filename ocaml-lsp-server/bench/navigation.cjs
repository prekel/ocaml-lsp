// Run from any directory after building @install and installing the e2e dependencies:
// node navigation.cjs /path/to/ocamllsp [source.ml needle]
const fs = require("fs");
const path = require("path");
const cp = require("child_process");
const { pathToFileURL, fileURLToPath } = require("url");
const { createRequire } = require("module");
const requireE2e = createRequire(
  path.resolve(__dirname, "../test/e2e/package.json"),
);
const rpc = requireE2e("vscode-jsonrpc/node");
const root = path.resolve(__dirname, "../..");
const binary = path.resolve(process.argv[2]);
const samples = 5;

function summary(values) {
  const sorted = [...values].sort((a, b) => a - b);
  return {
    medianMs: +sorted[Math.floor(sorted.length / 2)].toFixed(2),
    p95Ms: +sorted[Math.ceil(sorted.length * 0.95) - 1].toFixed(2),
  };
}

async function measure(source, needle) {
  const file = path.resolve(root, source);
  const text = fs.readFileSync(file, "utf8");
  const offset = text.indexOf(needle);
  if (offset < 0) throw Error(`Missing needle ${needle} in ${file}`);
  const prefix = text.slice(0, offset + needle.indexOf(".") + 1).split("\n");
  const params = {
    textDocument: { uri: pathToFileURL(file).href },
    position: { line: prefix.length - 1, character: prefix.at(-1).length },
  };
  const child = cp.spawn(binary, [], {
    cwd: root,
    env: { ...process.env, OCAMLLSP_TEST: "true" },
    stdio: ["pipe", "pipe", "ignore"],
  });
  const exited = new Promise((resolve) => child.once("exit", resolve));
  const connection = rpc.createMessageConnection(
    new rpc.StreamMessageReader(child.stdout),
    new rpc.StreamMessageWriter(child.stdin),
  );
  connection.listen();
  let trace;
  connection.onRequest("window/showDocument", ({ uri }) => {
    const filename = fileURLToPath(uri);
    trace = JSON.parse(fs.readFileSync(filename, "utf8"));
    fs.unlinkSync(filename);
    return { success: true };
  });
  async function timedClick(hover = false) {
    const cancellation = new rpc.CancellationTokenSource();
    const start = performance.now();
    const timeout = setTimeout(() => cancellation.cancel(), 15000);
    try {
      const pendingHover = hover
        ? connection.sendRequest(
            "textDocument/hover",
            params,
            cancellation.token,
          )
        : Promise.resolve();
      const [click] = await Promise.all([
        connection
          .sendRequest("textDocument/definition", params, cancellation.token)
          .then((result) => ({ result, elapsed: performance.now() - start })),
        pendingHover,
      ]);
      const { result, elapsed } = click;
      if (!result || result.length !== 1)
        throw Error(`Missing target: ${JSON.stringify(result)}`);
      return { elapsed, target: result[0].uri, range: result[0].range };
    } finally {
      clearTimeout(timeout);
      cancellation.dispose();
    }
  }
  try {
    await connection.sendRequest("initialize", {
      processId: process.pid,
      rootUri: pathToFileURL(root).href,
      workspaceFolders: [{ name: "ocaml-lsp", uri: pathToFileURL(root).href }],
      capabilities: {},
    });
    connection.sendNotification("initialized", {});
    connection.sendNotification("textDocument/didOpen", {
      textDocument: {
        uri: params.textDocument.uri,
        languageId: "ocaml",
        version: 0,
        text,
      },
    });
    const cold = await timedClick();
    const warm = [],
      edited = [],
      queued = [];
    for (let i = 0; i < samples; i++) warm.push((await timedClick()).elapsed);
    for (let i = 0; i < samples; i++) {
      connection.sendNotification("textDocument/didChange", {
        textDocument: { uri: params.textDocument.uri, version: i + 1 },
        contentChanges: [{ text: text + "\n".repeat(i + 1) }],
      });
      edited.push((await timedClick()).elapsed);
    }
    for (let i = 0; i < samples; i++)
      queued.push((await timedClick(true)).elapsed);
    await connection.sendRequest("workspace/executeCommand", {
      command: "ocamllsp/view-metrics",
      arguments: [],
    });
    const pipelines = {};
    for (const event of trace.traceEvents)
      pipelines[event.name] = (pipelines[event.name] || 0) + 1;
    console.log(
      JSON.stringify({
        binary,
        source,
        needle,
        samples,
        coldMs: +cold.elapsed.toFixed(2),
        warm: summary(warm),
        afterEdit: summary(edited),
        hoverThenClick: summary(queued),
        target: cold.target,
        targetRange: cold.range,
        pipelines,
      }),
    );
  } finally {
    connection.sendNotification("exit");
    const kill = setTimeout(() => child.kill("SIGKILL"), 5000);
    await exited;
    clearTimeout(kill);
    connection.dispose();
  }
}

(async () => {
  if (process.argv[3]) await measure(process.argv[3], process.argv[4]);
  else {
    await measure(
      "ocaml-lsp-server/src/document.ml",
      "Merlin_config.config config",
    );
    await measure(
      "ocaml-lsp-server/src/hover_req.ml",
      "Ppx_expansion.at_position",
    );
  }
})().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
