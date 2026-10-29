# Navigation on OCaml 5.5

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
| `libraries`, `re_export`, `implements` | The matching local declaration, or an installed package target. |
| `pps`, `staged_pps`, including `per_module` | The local or installed PPX library; arguments after `--` are ignored. |
| `modules`, `private_modules`, `virtual_modules`, `modules_without_implementation`, `per_module` module names | The module's `.mli`, falling back to `.ml`. |
| `name`/`names` in executable and test stanzas | The entry point's `.ml`. |
| Literal `include`, ordered-set `:include`, `deps`, `preprocessor_deps`, `(file ...)`, named dependencies | The existing file, including unsaved open files. |
| Rule `target`/`targets`, literal inferred output filenames, generated module/file references | The existing artifact in its build context, or its promoted source copy. |
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
libraries without available metadata, `select` forms and modules inherited through `include_subdirs` are
not resolved. Literal include paths can be opened, but declarations inside
included fragments are not indexed. Conditions and directory-selection stanzas
are not evaluated. Comments and action strings never become declarations.

For VS Code / OCaml Platform, the server dynamically registers Definition,
Declaration, semantic tokens and incremental document synchronization after
`initialized`, when the client advertises support. For Zed, the existing OCaml extension needs a
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

### Rule-generated files

For a rule output, Definition and Declaration open the corresponding existing
file under `_build/<context>/<project-relative-path>`. The source path is preserved,
so equal basenames in different directories stay distinct. The default context
is preferred; a context selected with `(merlin)` in `dune-workspace` takes priority.
When only other contexts contain the output, their existing targets are returned.
`DUNE_BUILD_DIR` is honored when set in the server environment.

With `(mode promote)`, the promoted file in the source tree is preferred. This
also handles `(until-clean)`, `(into dir)` and `only` filters expressed as literal
globs or `and`/`or`/`not` predicates. Until a promoted copy exists, an existing build
copy can still be opened. Fallback rules prefer existing source copies as well.
Unsaved rule mode changes are reflected immediately in Dune-file navigation.

Targets can be declared with `target`/`targets` or inferred from literal
`with-stdout-to`, `with-stderr-to`, `with-outputs-to`, `write-file`, `copy` and
`copy#` actions, including standard action wrappers and shorthand rules. File
references inside a directory target resolve to existing files within its tree.
Shell commands and variables are not evaluated. Missing outputs produce no
location; navigation does not build or promote anything.

OCaml Definition, Declaration and Type Definition can also reach generated
`.ml`/`.mli` files without a promoted source copy. If native Merlin lookup cannot
find a source, it retries using the source directories corresponding to the
already configured annotation build contexts, without another typing pipeline
or project artifact scan. Available original/promoted copies are preferred,
including promotion into another directory; otherwise the build source is opened.
Open source interfaces keep their existing overlay behavior.

### Installed packages and PPX

Visible local library declarations take precedence over installed packages.
Other names are resolved by `ocamlfind query` from the environment used to start
the server, including its `PATH`, `OCAMLPATH` and switch configuration. No other
switches are searched and no build or PPX executable is started. Without
`ocamlfind`, local navigation remains available.

For an installed package, the server reads the matching library entry in
`dune-package` and selects an existing interface or implementation for its main
module, preferring `.mli`. It uses declared source paths and conventional main
module filenames. If there is no usable main source, it selects the library
name in `dune-package`, or opens `META` when Dune metadata is unavailable.
Subpackages are resolved by their complete findlib names.

Package queries run only for navigation on a separate worker; semantic
highlighting never queries installed packages. Each query has a two-second
deadline and a 64 KiB output limit;
queries and their process groups are reaped on timeout and server shutdown.
Positive package locations are cached, metadata is reconsidered after two seconds,
and changed `META` contents trigger another findlib query. Missing names are
cached for two seconds. Every external click rechecks its source/fallback targets.

### Local dependency highlighting in VS Code

The server emits standard `namespace` tokens for resolvable local library
references and `macro` tokens for resolvable local PPX references. The theme
controls their colors when semantic highlighting is enabled. Installed packages
and unresolved names receive no added token, including after navigating to them.
Names containing hyphens or dots are emitted as one complete token, also when
quoted. Dune tokens use the syntax index, without a Merlin pipeline, and support
both full responses and deltas.

For optional persistent underlines, merge
[the example settings](vscode-dune-navigation.settings.json) into VS Code's user
or workspace settings. They enable Dune semantic highlighting and style the
additional `duneLocal` modifier. Text colors remain controlled by the theme.
No OCaml Platform extension changes are required.

Clients supporting `workspace/semanticTokens/refresh` receive a coalesced
refresh when local Dune declarations or workspace folders change. Restart the
language server after installing this build.

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

Interface-type Implementation retains master’s Smart Locate behavior. The
workspace index, value/module Implementation, installed-source references and
Call Hierarchy are ported from the OCaml 4.14 branch to Merlin 5.9.

## Running a navigation benchmark

After `dune build @install`, the benchmark can be run with Node.js and no
JavaScript packages. It checks that a target is returned and measures cold,
repeated and post-edit Definition requests, including requests queued after Hover:

```sh
node ocaml-lsp-server/bench/navigation.cjs _build/default/ocaml-lsp-server/bin/main.exe
node ocaml-lsp-server/bench/navigation.cjs _build/default/ocaml-lsp-server/bin/main.exe \
  ocaml-lsp-server/src/dune jsonrpc
```

An optional source path and literal needle select another call site. Set
`OCAMLLSP_BENCH_SAMPLES` to change the number of repetitions. Timings include
server processing and local IPC; they do not measure VS Code UI latency.
