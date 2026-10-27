# Navigation on OCaml 4.14

| LSP action | Result |
| --- | --- |
| Go to Definition / Ctrl-click | Prefer `.mli`; on a known interface declaration, open its `.ml` body. |
| Go to Declaration | The API declaration, preferring `.mli`. |
| Go to Type Definition | The declaration of the selected expression's type. |
| Go to Implementation | A value, type or module body in `.ml`, including from an interface declaration. |
| Find All References | Symbol occurrences in the current workspaces, including closed files. |
| Incoming Calls | Callers of the selected callable, grouped with their call sites. |
| Outgoing Calls | Direct callees of the selected callable, grouped with their call sites. |

Definition and Declaration use Merlin Locate for the selected symbol, preferring
an available `.mli` declaration. A bounded syntax cache refines the compiler's
point into a complete identifier range. It also follows ordinary exported
bindings between exact `.ml`/`.mli` counterparts and relocates known declarations
in unsaved target buffers by module path and namespace. Local and hidden bindings
remain distinct from exported declarations.

The source buffer is the only document passed through a Merlin typing pipeline.
The target uses the built-in syntax reader with no project preprocessors or PPX
execution. The cache holds 32 content-validated file versions, discarding ASTs
after extracting declaration locations. Files larger than 1 MB retain Merlin's
point location. Locate results themselves are not cached: configuration changes
and rebuilt annotations are checked by Merlin on every request.

The selected compilation unit's source digest validates saved target positions
when matching annotations are available. Preprocessed annotations use source and
artifact timestamps. Stale closed targets are omitted; changed open targets are
matched against the saved declaration's path and namespace. Syntax-only matching
refuses uncertain includes, PPX forms, recovered syntax and hidden declarations.
Unrefined, unchanged targets retain Merlin's point; unsafe unsaved relocations
return no target. Use Go to Implementation for complex interface bodies.

Go to Implementation follows named modules, includes, aliases and functor
results. It honors open implementation buffers and returns no body for an
interface-only module. It does not search for every module or class satisfying
a structural signature.

The editor chooses which LSP action a mouse gesture invokes. Ctrl-click and a
menu action have the same server behavior when both send
`textDocument/definition`: both prefer `.mli`. Use Go to Implementation for a
direct `.ml` jump. On ordinary interface declarations, Definition follows the
known declaration into its implementation.

## Dune files

Go to Definition and Go to Declaration also work in `dune` files without
running Dune or Merlin:

| Dune reference | Target |
| --- | --- |
| `libraries`, `re_export`, `implements` | The matching local `name` or `public_name` declaration. |
| `pps`, `staged_pps`, including `per_module` | The local PPX library declaration; arguments after `--` are ignored. |
| `modules`, `private_modules`, `virtual_modules`, `modules_without_implementation`, `per_module` module names | The module's `.mli`, falling back to `.ml`. |
| `name`/`names` in executable and test stanzas | The entry point's `.ml`. |
| Literal `include`, ordered-set `:include`, `deps`, `preprocessor_deps`, `(file ...)`, named dependencies | The existing file, including unsaved open files. |
| `(alias path/name)`, `(alias_rec path/name)` dependencies | Matching alias and rule declarations in that directory, or its descendants for `alias_rec`. |

Private library names are scoped to the nearest `dune-project`; public names
can resolve across projects in the workspace. Ambiguous declarations are
returned as multiple locations instead of choosing arbitrarily. Module and
file targets open at the start of the file; declaration targets select their
complete names using the negotiated UTF-8 or UTF-16 encoding. Paths and module
names inside `subdir` stanzas use the stanza's directory.

A dedicated worker discovers and parses Dune files, separate from the Merlin
queue. Definitions use an in-memory name index and validate the selected closed
target's bytes on each request. Open Dune buffers override disk contents and
are checked again before returning. Watch notifications update changed files;
clients without watchers also rediscover files on a request after two seconds.
Discovery skips dot directories, underscore directories (including `_build`
and `_opam`), `node_modules` and directory symlinks. Parsing is limited to 1 MB,
128 levels and 100,000 nodes per file.

This is source configuration navigation, not an evaluation of the Dune build
graph. Variables, globs, generated files that do not exist, installed dependency
libraries, `select` forms and modules inherited through `include_subdirs` are
not resolved. Literal include paths can be opened, but declarations inside
included fragments are not indexed. Conditions and directory-selection stanzas
are not evaluated. Comments and action strings never become declarations.

For VS Code / OCaml Platform, the server dynamically registers Definition,
Declaration and incremental document synchronization after `initialized`, when
the client advertises support. For Zed, the existing OCaml extension needs a
language association in project or user settings, without changing the extension:

```json
{
  "languages": {
    "Dune": {
      "language_servers": ["ocamllsp"]
    }
  }
}
```

This repository includes that setting in `.zed/settings.json`. Restart the
language server after installing the new binary.

The point-navigation benchmark below also accepts a Dune source file:

```sh
node ocaml-lsp-server/bench/navigation.cjs /path/to/ocamllsp \
  ocaml-lsp-server/src/dune jsonrpc
node ocaml-lsp-server/bench/navigation.cjs /path/to/ocamllsp \
  ocaml-lsp-server/bin/dune ocaml_lsp_server
```

Local measurements of the installed binary on 2026-10-27: these real-source
library jumps took 5.48 ms and 5.16 ms cold, with warm medians of 0.81 ms and
0.73 ms (five samples each). After unsaved source edits the medians were
1.18 ms and 1.08 ms. Traces contained
no Merlin pipelines. These are server/IPC timings, excluding editor UI and SSH.

## Call hierarchy

The graph describes statically identifiable applications, including operators,
partial applications, recursive calls, local functions and anonymous functions.
The standard `|>` and `@@` primitives are recognized by their typed identity;
redefining an operator does not give it the standard operator's meaning.
Passing a function as a value does not call it. Calls inside lambdas, methods,
lazy expressions and module initializers belong to those bodies rather than to
the function that creates them.

There is no dataflow analysis through callback parameters or dynamic object
method dispatch. Dependency functions can be targets and have workspace
callers, but expanding them does not index their dependency bodies. The server
expands one graph level per request, so recursive edges are safe to browse.

Each prepared item carries an opaque identity for its server session and
project snapshot. After an input changes, prepare the hierarchy again; expanding
an outdated item returns `ContentModified`. Malformed items return
`InvalidParams`.

## References and call hierarchy index freshness

The project index uses compiler identities and build annotations (`.cmt` and
`.cmti`), including interface digests to distinguish same-named units. References
first select the relevant compiler unit and traverse its importers rather than
collecting every typed tree on each search. Open buffers replace disk records.

Run `dune build` manually after changing closed files or project dependencies.
Outdated closed-file positions are omitted from indexed results with a log
message, and the next request observes rebuilt annotations. Navigation never
starts a project build. Indexed requests check cancellation and changes to
open buffers or workspace folders before returning results, and convert
positions to the negotiated UTF-8 or UTF-16 encoding.

The interface-type Implementation support adapts upstream PR
[#2074](https://github.com/ocaml/ocaml-lsp/pull/2074) to Merlin 4.19. Value/module
Implementation, installed-source references and Call Hierarchy are separate
local enhancements, with separate commits for future upstream porting.

## Point-navigation measurements

Definition/Declaration use the native Merlin locator with cached syntax refinement.
Go to Type Definition retains the Merlin 4.19 pattern-annotation and range fixes.

The benchmark uses real `document.ml` (527 lines) and `hover_req.ml` (427 lines),
jumping to `Merlin_config.config` and `Ppx_expansion.at_position`. Each file gets
a fresh server: one cold Definition request, five repeats, five requests after
unsaved source edits, then five requests queued immediately after Hover at the
same position. OCaml 4.14.1, Dune 3.24.2; local measurements on 2026-10-26.

Times are milliseconds of LSP response latency, including local IPC. GUI and
remote-editor transport delays are outside this measurement. The cold request
also includes queued document setup. All versions return an actual target. The
upstream comparison returns a `.ml` point; the syntax-cache version returns a
complete `.mli` identifier range. Upstream and syntax-cache runs below were made
sequentially under the same conditions; the indexed run is the earlier baseline.

| Source | Version | Cold | Repeat median | Repeat p95 | After edit median | Hover + click median |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| document.ml | Project index (`4126bb0f`) | 946.80 | 33.09 | 46.05 | 35.59 | 46.44 |
| document.ml | Upstream Locate | 184.66 | 6.71 | 9.97 | 14.89 | 17.98 |
| document.ml | Locate + syntax cache | 163.45 | 7.78 | 8.54 | 14.41 | 14.09 |
| hover_req.ml | Project index (`4126bb0f`) | 835.78 | 50.55 | 70.82 | 49.02 | 46.82 |
| hover_req.ml | Upstream Locate | 153.90 | 6.63 | 8.64 | 8.44 | 12.47 |
| hover_req.ml | Locate + syntax cache | 181.85 | 8.25 | 9.31 | 10.42 | 13.20 |

Clicking `at_position` on its declaration in `ppx_expansion.mli` also resolves the
complete `.ml` name: cold 108.35 ms, repeated median 3.05 ms, repeated p95 4.77 ms.

The trace records 16 `definition` pipelines per file, with zero project snapshots
or target typing pipelines. Regression tests check this with 50 unrelated buffers
and a malformed unrelated annotation, as well as modified targets, same-size disk
edits, namespaces, shadowing, cancellation, UTF-8/UTF-16 and rebuilt annotations.

Reproduce after `dune build @install` and installing the e2e dependencies:

```sh
node ocaml-lsp-server/bench/navigation.cjs /path/to/ocamllsp
```

An optional source path and literal needle select a different call site:

```sh
node ocaml-lsp-server/bench/navigation.cjs /path/to/ocamllsp source.ml Module.function
```
