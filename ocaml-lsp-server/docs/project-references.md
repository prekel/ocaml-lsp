# Project references on OCaml 4.14

This branch implements `textDocument/references` across a Dune workspace on
OCaml 4.14.1 / Merlin 4.19-414. VS Code's **Find All References** and Zed's
reference search use this standard LSP request; no editor extension is required.

Build the workspace once, then restart the editor's language server after
installing this branch:

```sh
dune build @check
```

The server reads the active Dune build context's `.cmt` and `.cmti` files. It
keeps a memory cache and refreshes changed, added and removed artifacts on each
reference request. It does not start builds. Run `dune build @check` again after
changing files outside the editor or changing exported interfaces.

Open OCaml buffers use their current, unsaved typed trees instead of the cached
occurrences. Closing a buffer restores its compiled index. Closed files whose
contents no longer match the indexed source digest are omitted, with a warning
in the language-server log. Unsupported preprocessing/source mappings may also
cause an artifact to be omitted even after a rebuild.

The index follows compiler declarations and implementation shapes, including
public `.ml`/`.mli` counterparts, wrapped units, module aliases, shadowing,
values, types, modules, module types, constructors, exceptions, record fields,
classes and class types. Results cover source files within the workspace
folders, including closed files. Installed libraries outside those folders
are not indexed. In a document outside the workspace, reference search remains
local to that document.

Some OCaml identities are inherently ambiguous in 4.14 artifacts. In particular,
functor applications and structural object methods do not always identify a
unique source declaration. The server skips uncertain matches. Errors or
incomplete typing can also reduce the results. This index is used for reference
search only; Rename retains its existing behavior.

Requests support cancellation. If an open document or the workspace folders
change during a search, the server returns `ContentModified` instead of returning
positions from inconsistent document versions.
