# Navigation on OCaml 4.14

| LSP action | Result |
| --- | --- |
| Go to Definition | The defining binding, preferring `.ml`. |
| Go to Declaration | The API declaration, preferring `.mli`. |
| Go to Type Definition | The declaration of the selected expression's type. |
| Go to Implementation | A value, type or module body in `.ml`, including from an interface declaration. |
| Find All References | Symbol occurrences in the current workspaces, including closed files. |
| Incoming Calls | Callers of the selected callable, grouped with their call sites. |
| Outgoing Calls | Direct callees of the selected callable, grouped with their call sites. |

Go to Implementation follows named modules, includes, aliases and functor
results. It honors open implementation buffers and returns no body for an
interface-only module. It does not search for every module or class satisfying
a structural signature.

The editor chooses which LSP action a mouse gesture invokes. Ctrl-click and a
menu action cannot have different server behavior if both send
`textDocument/definition`. An editor must send `textDocument/declaration` for a
gesture intended to open `.mli`.

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

## Index freshness

The project index uses compiler identities and build annotations (`.cmt` and
`.cmti`), including interface digests to distinguish same-named units. References
first select the relevant compiler unit and traverse its importers rather than
collecting every typed tree on each search. Open buffers replace disk records.

Run `dune build` manually after changing closed files or project dependencies.
Outdated closed-file positions are omitted with a log message, and the next
request observes rebuilt annotations. Navigation never starts a project build.
Cancellation and changes to open buffers or workspace folders are checked
before results are returned. Positions follow the negotiated UTF-8 or UTF-16
encoding.

The interface-type Implementation support adapts upstream PR
[#2074](https://github.com/ocaml/ocaml-lsp/pull/2074) to Merlin 4.19. Value/module
Implementation, installed-source references and Call Hierarchy are separate
local enhancements, with separate commits for future upstream porting.
