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

The first fix returns 0 inlay hints and 0 CodeLens with or without
`deriving`. Hover on `address` and `t` returns the original record declarations.
Hover on the `string` field type matches the control; the generated decoder's
`(string, string) result` no longer appears. The `deriving` attribute still shows
the PPX expansion. Both fixtures still build and publish empty diagnostics.

These captures are preserved in `results/after-fix/`.

## Signatures and expansion action

New runs default to `results/deriving-ui/`, preserving both earlier sets of
captures. Type hovers now append the generated signatures; attribute hovers
show the signatures, including when pointing at the generator name `yojson`.
The signatures contain named exported declarations, without anonymous `val _`
entries or local helper functions.

The script also requests the **Show generated code** action inside the
attribute, executes it, and captures the resulting `window/showDocument`
request and full expansion in `derived-expansion.ml`. It checks that this
action is absent on the type name and in the control fixture. In an editor,
place the cursor inside the attribute and invoke the lightbulb menu or `Ctrl+.`.

The metadata includes the server binary hash and worktree status to identify
the exact build even when implementation changes are not committed.

The script now also requests `ocamllsp/inferIntf`, saves the inferred interface
and rebuilds the temporary project with it. This checks that PPX-generated
anonymous bindings do not leak into an invalid `.mli`. The general PPX tests
in `ppx_all.ml` cover all extension point categories, ordinary PPX attributes,
and UTF16 cursor positions. `inferred_interface_formatting.ml` checks project
formatting, valid inferred signatures, and counterpart navigation.

There is also a self-contained regression test in
`ocaml-lsp-server/test/e2e-new/ppx_source_hints.ml`. Its local PPX deliberately
generates bindings and parameters at non-ghost locations belonging to record
declarations and their field types. It checks source types, documentation,
nested modules, normal function hints and CodeLens, references to generated
functions, and signature hover on the attribute. Additional tests in
`ppx_deriving_actions.ml` cover recursive type groups, multiple generators,
generated modules inside `include`, interface files, unsaved buffers, stale
actions, unsupported clients, opening failures, and expansion fallback when
type inference fails. Run these with the existing suite:

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
- `results/deriving-ui/`: signature hovers, code actions, and the opened expansion.

JSON positions are zero-based. Temporary project URIs in captured responses
refer to directories removed after the run.
