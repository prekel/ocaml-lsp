// Run from any directory with Node.js after building @install:
// node navigation.cjs /path/to/ocamllsp [source-file needle]
const fs = require("fs");
const path = require("path");
const cp = require("child_process");
const { pathToFileURL, fileURLToPath } = require("url");
const root = path.resolve(__dirname, "../..");
const binary = path.resolve(process.argv[2]);
const samples = Number(process.env.OCAMLLSP_BENCH_SAMPLES || 5);

// A small stdio LSP client keeps this benchmark independent of test packages.
function connect(child) {
  let sequence = 0;
  let buffer = Buffer.alloc(0);
  let failure;
  const pending = new Map();
  const handlers = new Map();
  const send = (message) => {
    const body = JSON.stringify({ jsonrpc: "2.0", ...message });
    child.stdin.write(`Content-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
  };
  const fail = (error) => {
    failure = error;
    for (const request of pending.values()) request.reject(error);
    pending.clear();
  };
  const dispatch = (message) => {
    if (message.method) {
      if (message.id !== undefined) {
        const handler = handlers.get(message.method);
        Promise.resolve()
          .then(() => {
            if (!handler) throw Error(`Unsupported request: ${message.method}`);
            return handler(message.params);
          })
          .then(
            (result) => send({ id: message.id, result }),
            (error) =>
              send({ id: message.id, error: { code: -32603, message: error.message } }),
          );
      }
      return;
    }
    const request = pending.get(message.id);
    if (!request) return;
    pending.delete(message.id);
    if (message.error) request.reject(Error(JSON.stringify(message.error)));
    else request.resolve(message.result);
  };
  const read = (chunk) => {
    try {
      buffer = Buffer.concat([buffer, chunk]);
      for (;;) {
        const end = buffer.indexOf("\r\n\r\n");
        if (end < 0) return;
        const header = buffer.subarray(0, end).toString("ascii");
        const match = /^Content-Length:\s*(\d+)\s*$/im.exec(header);
        if (!match) throw Error(`Invalid LSP header: ${header}`);
        const length = Number(match[1]);
        const start = end + 4;
        if (buffer.length < start + length) return;
        const body = buffer.subarray(start, start + length);
        buffer = buffer.subarray(start + length);
        dispatch(JSON.parse(body.toString("utf8")));
      }
    } catch (error) {
      fail(error);
    }
  };
  child.stdout.on("data", read);
  child.stdin.on("error", fail);
  child.on("error", fail);
  child.on("close", (code) => fail(Error(`Language server closed (${code})`)));
  return {
    onRequest: (method, handler) => handlers.set(method, handler),
    sendNotification: (method, params) => send({ method, params }),
    sendRequest: (method, params) => {
      if (failure) return Promise.reject(failure);
      const id = ++sequence;
      let timeout;
      return new Promise((resolve, reject) => {
        pending.set(id, { resolve, reject });
        timeout = setTimeout(() => {
          send({ method: "$/cancelRequest", params: { id } });
          pending.delete(id);
          reject(Error(`Request timed out: ${method}`));
        }, 15000);
        send({ id, method, params });
      }).finally(() => clearTimeout(timeout));
    },
    dispose: () => child.stdout.off("data", read),
  };
}

function summary(values) {
  const sorted = [...values].sort((a, b) => a - b);
  return {
    medianMs: +sorted[Math.floor(sorted.length / 2)].toFixed(2),
    p95Ms: +sorted[Math.ceil(sorted.length * 0.95) - 1].toFixed(2),
  };
}

async function measure(source, needle) {
  const file = path.resolve(root, source);
  const dune = path.basename(file) === "dune";
  const text = fs.readFileSync(file, "utf8");
  const match = dune
    ? new RegExp(
        "(^|[^A-Za-z0-9_.+-])(" +
          needle.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") +
          ")(?![A-Za-z0-9_.+-])",
      ).exec(text)
    : null;
  const offset = dune
    ? match
      ? match.index + match[1].length
      : -1
    : text.indexOf(needle);
  if (offset < 0) throw Error(`Missing needle ${needle} in ${file}`);
  const prefix = text
    .slice(0, dune ? offset : offset + needle.indexOf(".") + 1)
    .split("\n");
  const params = {
    textDocument: { uri: pathToFileURL(file).href },
    position: { line: prefix.length - 1, character: prefix.at(-1).length },
  };
  const child = cp.spawn(binary, [], {
    cwd: root,
    env: { ...process.env, OCAMLLSP_TEST: "true" },
    stdio: ["pipe", "pipe", "ignore"],
  });
  const exited = new Promise((resolve) => child.once("close", resolve));
  const connection = connect(child);
  let trace;
  connection.onRequest("window/showDocument", ({ uri }) => {
    const filename = fileURLToPath(uri);
    trace = JSON.parse(fs.readFileSync(filename, "utf8"));
    fs.unlinkSync(filename);
    return { success: true };
  });
  async function timedClick(hover = false) {
    const start = performance.now();
    const pendingHover = hover
      ? connection.sendRequest("textDocument/hover", params)
      : Promise.resolve();
    const [click] = await Promise.all([
      connection
        .sendRequest("textDocument/definition", params)
        .then((result) => ({ result, elapsed: performance.now() - start })),
      pendingHover,
    ]);
    const { result, elapsed } = click;
    if (!result || result.length !== 1)
      throw Error(`Missing target: ${JSON.stringify(result)}`);
    return { elapsed, target: result[0].uri, range: result[0].range };
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
        languageId: dune ? "dune" : "ocaml",
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
