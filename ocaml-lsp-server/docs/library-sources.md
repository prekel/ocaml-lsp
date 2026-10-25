# Browsing installed libraries

OCaml-LSP recognizes OCaml sources in an opam switch's `lib` directory and in
the running compiler's standard library. This includes local `_opam` switches,
named switches, and symbolic links to installed sources.

Installed files use a browsing environment instead of the surrounding
workspace's Dune configuration. When matching `.cmt` or `.cmti` files are
available, the server recovers the compilation unit name, implicit module
opens, and dependencies. Dependencies are matched by interface digest inside
the installation being browsed, so the original build tree is not required.

Semantic highlighting remains available, along with hover and navigation where
the installed artifacts provide enough information. Without compatible
annotations, the server uses the source directory and the standard library.
Build-time preprocessors are not rerun; features requiring unavailable PPX
expansions or private modules may therefore be incomplete.

The server does not publish diagnostics for installed sources, including
unsaved edits to those buffers. This browsing behavior does not make the file
read-only in the editor. Files in the project's own source tree keep their
normal diagnostics, including configuration errors when a build is needed.
