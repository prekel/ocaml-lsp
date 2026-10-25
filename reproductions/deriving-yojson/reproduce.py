#!/usr/bin/env python3
"""Capture actual LSP responses for a yojson-derived record and a control."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import queue
import subprocess
import tempfile
import threading
from urllib.parse import unquote, urlparse

SOURCE = """type address =
  { city : string
  ; zip : int
  }
[@@deriving yojson]

type t =
  { name : string
  ; age : int
  ; address : address
  ; tags : string list
  ; nickname : string option
  }
[@@deriving yojson]
"""


class Client:
    def __init__(self, server, cwd, env, stderr):
        self.process = subprocess.Popen(
            [str(server)], cwd=cwd, env=env, stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=stderr,
        )
        self.messages = queue.Queue()
        self.notifications = []
        self.shown_documents = []
        self.next_id = 0
        threading.Thread(target=self.read, daemon=True).start()

    def read(self):
        try:
            while True:
                headers = {}
                while True:
                    line = self.process.stdout.readline()
                    if not line:
                        raise EOFError("LSP server closed stdout")
                    if line == b"\r\n":
                        break
                    key, value = line.decode().split(":", 1)
                    headers[key.lower()] = value.strip()
                data = self.process.stdout.read(int(headers["content-length"]))
                self.messages.put(json.loads(data))
        except Exception as error:
            self.messages.put(error)

    def send(self, message):
        data = json.dumps({"jsonrpc": "2.0", **message}).encode()
        self.process.stdin.write(f"Content-Length: {len(data)}\r\n\r\n".encode() + data)
        self.process.stdin.flush()

    def notify(self, method, params):
        self.send({"method": method, **({"params": params} if params is not None else {})})

    def receive(self):
        message = self.messages.get(timeout=45)
        if isinstance(message, Exception):
            raise message
        if "method" in message:
            if "id" in message:
                method = message["method"]
                if method == "workspace/configuration":
                    result = [None for _ in message["params"]["items"]]
                elif method == "workspace/workspaceFolders":
                    result = []
                elif method == "window/showDocument":
                    params = message["params"]
                    path = Path(unquote(urlparse(params["uri"]).path))
                    self.shown_documents.append({"params": params, "text": path.read_text()})
                    path.unlink()
                    result = {"success": True}
                else:
                    result = None
                self.send({"id": message["id"], "result": result})
            else:
                self.notifications.append(message)
        return message

    def request(self, method, params):
        self.next_id += 1
        request_id = self.next_id
        self.send({"id": request_id, "method": method, **({"params": params} if params is not None else {})})
        while True:
            message = self.receive()
            if message.get("id") == request_id and "method" not in message:
                if "error" in message:
                    raise RuntimeError(message)
                return message.get("result")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ppx-root", type=Path, required=True)
    parser.add_argument("--server", type=Path, default=Path(__file__).resolve().parents[2] / "_build/default/ocaml-lsp-server/bin/main.exe")
    parser.add_argument("--output", type=Path, default=Path(__file__).resolve().parent / "results/deriving-ui")
    args = parser.parse_args()
    ppx_root = args.ppx_root.resolve()
    env = os.environ.copy()
    env["PATH"] = str(ppx_root / "_opam/bin") + os.pathsep + env["PATH"]
    env["OCAMLPATH"] = os.pathsep.join(filter(None, [
        str(ppx_root / "_build/install/default/lib"),
        str(ppx_root / "_opam/lib"), env.get("OCAMLPATH"),
    ]))
    # The production server must filter ghost locations; test mode disables it.
    env.pop("OCAMLLSP_TEST", None)
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    metadata = {}
    for name, command in {
        "server_version": [str(args.server.resolve()), "--version"],
        "ocaml_version": ["ocamlc", "-version"],
        "dune_version": ["dune", "--version"],
        "ppx_version": ["ocamlfind", "query", "-format", "%v", "ppx_deriving_yojson"],
        "merlin_version": ["opam", "list", "--switch", str(Path(__file__).resolve().parents[2]), "--installed", "--columns=version", "--short", "merlin-lib"],
        "ppx_commit": ["git", "-C", str(ppx_root), "rev-parse", "HEAD"],
        "server_commit": ["git", "-C", str(Path(__file__).resolve().parents[2]), "rev-parse", "HEAD"],
    }.items():
        metadata[name] = subprocess.check_output(command, env=env, text=True).strip()
    metadata["server_binary_sha256"] = hashlib.sha256(args.server.resolve().read_bytes()).hexdigest()
    metadata["server_worktree_status"] = subprocess.check_output(
        ["git", "-C", str(Path(__file__).resolve().parents[2]), "status", "--short"],
        text=True,
    ).strip()
    (output / "metadata.json").write_text(json.dumps(metadata, indent=2) + "\n")
    for derived in [True, False]:
        name = "derived" if derived else "control"
        source = SOURCE if derived else SOURCE.replace("[@@deriving yojson]", "")
        (output / f"{name}.ml").write_text(source)
        with tempfile.TemporaryDirectory(prefix="ocamllsp-yojson-") as tmp:
            root = Path(tmp)
            (root / "dune-project").write_text("(lang dune 3.0)\n(name yojson_repro)\n")
            (root / "dune").write_text(
                "(library (name example) (libraries yojson)"
                + (" (preprocess (pps ppx_deriving_yojson))" if derived else "") + ")\n"
            )
            file = root / "example.ml"
            file.write_text(source)
            build = subprocess.run(["dune", "build", "--root", str(root)], env=env, text=True, capture_output=True)
            (output / f"{name}-build.log").write_text(build.stdout + build.stderr)
            build.check_returncode()
            with (output / f"{name}-server.log").open("w") as log:
                client = Client(args.server.resolve(), root, env, log)
                try:
                    initialization = client.request("initialize", {
                        "processId": os.getpid(), "rootUri": root.as_uri(),
                        "capabilities": {"window": {"showDocument": {"support": True}}},
                        "workspaceFolders": [{"uri": root.as_uri(), "name": "yojson_repro"}],
                    })
                    client.notify("initialized", {})
                    client.notify("workspace/didChangeConfiguration", {"settings": {
                        "codelens": {"enable": True},
                        "inlayHints": {"hintLetBindings": True, "hintPatternVariables": True, "hintFunctionParams": True},
                    }})
                    document = {"uri": file.as_uri()}
                    client.notify("textDocument/didOpen", {"textDocument": {
                        **document, "languageId": "ocaml", "version": 1, "text": source,
                    }})
                    while not any(n["method"] == "textDocument/publishDiagnostics" for n in client.notifications):
                        client.receive()
                    diagnostics = [n["params"]["diagnostics"] for n in client.notifications
                                   if n["method"] == "textDocument/publishDiagnostics"]
                    if any(diagnostics):
                        raise RuntimeError(f"Unexpected diagnostics: {diagnostics}")
                    responses = {"initialize": initialization, "hover": {}}
                    for label, line, character in [
                        ("address", 0, 7), ("t", 6, 5), ("city", 1, 5),
                        ("city_type", 1, 12), ("address_field", 9, 5),
                        ("deriving", 4, 5), ("deriver_name", 4, 13),
                    ]:
                        params = {"textDocument": document, "position": {"line": line, "character": character}}
                        responses["hover"][label] = {"params": params, "result": client.request("textDocument/hover", params)}
                    responses["inlayHints"] = client.request("textDocument/inlayHint", {
                        "textDocument": document,
                        "range": {"start": {"line": 0, "character": 0}, "end": {"line": len(source.splitlines()), "character": 0}},
                    })
                    responses["codeLens"] = client.request("textDocument/codeLens", {"textDocument": document})
                    responses["codeActions"] = {}
                    for label, line, character in [("type", 0, 7), ("attribute", 4, 13)]:
                        position = {"line": line, "character": character}
                        actions = client.request("textDocument/codeAction", {
                            "textDocument": document,
                            "range": {"start": position, "end": position},
                            "context": {"diagnostics": [], "only": ["ocamllsp.showPpxExpansion"]},
                        }) or []
                        commands = [action["command"] for action in actions
                                    if isinstance(action.get("command"), dict)
                                    and action["command"]["command"] == "ocamllsp/show-ppx-expansion"]
                        assert len(commands) == (1 if derived and label == "attribute" else 0)
                        responses["codeActions"][label] = actions
                        if commands:
                            command = commands[0]
                            responses["executeCommand"] = client.request("workspace/executeCommand", {
                                "command": command["command"], "arguments": command["arguments"],
                            })
                    responses["shownDocuments"] = client.shown_documents
                    for shown in client.shown_documents:
                        (output / f"{name}-expansion.ml").write_text(shown["text"])
                    responses["notifications"] = client.notifications
                    (output / f"{name}.json").write_text(json.dumps(responses, indent=2) + "\n")
                    print(f"{name}: {len(responses['inlayHints'] or [])} inlay hints, {len(responses['codeLens'] or [])} lenses")
                    client.request("shutdown", None)
                    client.notify("exit", None)
                    client.process.wait(timeout=10)
                finally:
                    if client.process.poll() is None:
                        client.process.kill()
                        client.process.wait()


if __name__ == "__main__":
    main()
