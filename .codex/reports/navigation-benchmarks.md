# Historical navigation measurements

These measurements were made on the OCaml 4.14 branch. They are kept as
engineering records and do not establish performance relative to current master.

## Dune navigation

Historical measurements from the OCaml 4.14 branch on 2026-10-27, twenty
samples per phase (these have not been remeasured on OCaml 5.5):

| Dune jump | First request (ms) | Warm median / p95 (ms) |
| --- | ---: | ---: |
| `src/dune`: local `jsonrpc` | 6.41 | 0.81 / 1.06 |
| `bin/dune`: local `ocaml_lsp_server` | 5.99 | 0.73 / 1.27 |
| `src/dune`: installed `yojson` interface | 35.54 | 0.89 / 1.06 |
| `lsp/bin/metamodel/dune`: generated `metamodel_lsp.ml` | 7.51 | 0.86 / 1.38 |
| `submodules/lev/dune`: promoted `README.md` | 5.17 | 0.69 / 0.86 |

Before these extensions, the two local-library warm medians were 0.71 ms and
0.70 ms. The existing OCaml benchmark's warm medians were 9.35 / 7.28 ms before
and 8.02 / 7.40 ms after; no new project snapshots or target typing pipelines
appeared. Dune traces contained no Merlin pipelines. These are server/IPC
timings, excluding editor UI and SSH; cold results include setup and filesystem
cache effects, and installed-package cold lookup includes its first findlib query.

## Historical point-navigation measurements (OCaml 4.14)

Definition/Declaration use the native Merlin locator with cached syntax refinement.
Go to Type Definition returns complete identifier ranges using the current Merlin API.

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

Reproduce with Node.js after `dune build @install`; no JavaScript packages
are required:

```sh
node ocaml-lsp-server/bench/navigation.cjs /path/to/ocamllsp
```

An optional source path and literal needle select a different call site:

```sh
node ocaml-lsp-server/bench/navigation.cjs /path/to/ocamllsp source.ml Module.function
```
