# `deriving yojson` leaks generated types into editor hints

This standalone reproduction sends real JSON-RPC requests to the server built
from this checkout. It creates temporary Dune projects, builds them, waits for
LSP diagnostics, and captures hover, inlay hints, and CodeLens. It does not
require VS Code or change the server implementation.

## Run

Prerequisites: Python 3, this repository's existing opam switch, and a checkout
of `ppx_deriving_yojson` with its own `_opam` switch and built `@install` artifacts.
The PPX checkout used here is `/home/vprekel/ppx_deriving_yojson`.

From the OCaml LSP repository root:

```sh
opam exec -- dune build ocaml-lsp-server/bin/main.exe
opam exec --switch=/home/vprekel/ppx_deriving_yojson -- dune build --root /home/vprekel/ppx_deriving_yojson @install
python3 reproductions/deriving-yojson/reproduce.py --ppx-root /home/vprekel/ppx_deriving_yojson
```

Use `--server /absolute/path/to/ocamllsp` or `--output /path/to/results` to
override the default server or output directory. The script uses the PPX
checkout's Dune and libraries, and unsets `OCAMLLSP_TEST` so ghost-location
filtering follows production behavior. All three inlay hint categories and
CodeLens are enabled through `workspace/didChangeConfiguration`.

## Before the fix

Both fixtures build successfully and publish empty diagnostics.

| Request | With `[@@deriving yojson]` | Without the attributes |
| --- | --- | --- |
| Hover on `address` | `Yojson.Safe.t -> address Ppx_deriving_yojson_runtime.error_or` | `type address = { city : string; zip : int; }` |
| Hover on `t` | `Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or` | Record type declaration |
| Inlay hints | 69 | 0 |
| CodeLens | 10, showing generated converter signatures | 0 |

For example, the derived fixture gets both `: Yojson.Safe.t` and `: string`
at the end of `city : string`. Many more hints share the end position of each
`deriving` attribute. This reproduces the stacked gray annotations in the photo.
The hover range for `address` spans the declaration and its attribute, rather
than just the name. Hovering on the `deriving` attribute itself intentionally
shows the PPX expansion, including `address_of_yojson` with the exact signature
incorrectly shown on the type name.

The checked-in results use OCaml 4.14.1, Merlin 4.19-414, Dune 3.24.2, and the
local `ppx_deriving_yojson` 3.7.0 checkout. This checkout contains customized
error handling; reproduction with an unmodified upstream PPX has not been tested.
Exact server and PPX commits are recorded in `results/metadata.json`.

## After the fix

The same example now returns 0 inlay hints and 0 CodeLens with or without
`deriving`. Hover on `address` and `t` returns the original record declarations.
Hover on the `string` field type matches the control; the generated decoder's
`(string, string) result` no longer appears. The `deriving` attribute still shows
the PPX expansion. Both fixtures still build and publish empty diagnostics.

New runs default to `results/after-fix/`, preserving the original captures in
`results/`. The post-fix metadata includes the server binary hash and worktree
status because the implementation changes are not committed.

There is also a self-contained regression test in
`ocaml-lsp-server/test/e2e-new/ppx_source_hints.ml`. Its local PPX deliberately
generates bindings and parameters at non-ghost locations belonging to record
declarations and their field types. It checks source types, documentation,
nested modules, normal function hints and CodeLens, references to generated
functions, and expansion hover on the attribute. Run it with the existing suite:

```sh
opam exec -- dune build @ocaml-lsp-server/test/e2e-new/runtest @ocaml-lsp-server/test/runtest
```

## Evidence

- `results/derived.ml` and `results/control.ml`: exact input text; removing the
  two attributes preserves line numbers.
- `results/derived.json` and `results/control.json`: actual responses, hover
  request positions, initialization capabilities, and notifications.
- `results/*-build.log` and `results/*-server.log`: build and server output.
- `results/after-fix/`: corresponding captures from the fixed server.

JSON positions are zero-based. Temporary project URIs in captured responses
refer to directories removed after the run.
