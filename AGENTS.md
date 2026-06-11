# Agent Information - String Extractor

This repository contains an OCaml-based internationalization (i18n) string extraction tool. It parses source files (JS, TS, Vue, Pug, HTML) and extracts strings for translation management.

## Documentation

- **[ARCHITECTURE.md](ARCHITECTURE.md)**: Contains a deep-dive into the codebase layout, directory structure, and a comprehensive API reference. **Read this file first** when:
  - Starting a new task to understand which files are relevant.
  - Investigating the impact of changes across the system.
  - Looking for specific functionality or function definitions before searching.

- **[DEVELOPMENT.md](DEVELOPMENT.md)**: Contains instructions for environment setup, build processes for various platforms, and release workflows. **Read this file first** when:
  - Setting up the development environment or installing dependencies (OCaml, JS, QuickJS, Flow).
  - Building the project for development or release.
  - Executing the tool for manual verification or testing.
  - Managing version numbers or release artifacts.

## Project Overview

- **Language**: OCaml (5.1.1 in CI) with some C++ (QuickJS bridge) and JavaScript (parsers via Browserify).
- **Architecture**:
  - `src/cli/`: Main entry point (`strings.ml`), command-line interface, output generation (`.strings`/`.json`), and Vue file splitting (`vue.ml`).
  - `src/parsing/`: OCaml parsers using `Angstrom` for custom formats and `Flow_parser` for JS.
  - `src/quickjs/`: Bridge to QuickJS to run JavaScript-based parsers (TypeScript/Pug) from OCaml.
  - `src/utils/`: Common utilities for collection, timing, and I/O.
- **Key Libraries**: `Core`, `Lwt` (concurrency), `Angstrom`, `Yojson`, `Ppx_jane`.
- **Active branch context**: This codebase is the **Lwt** variant (an Eio port exists on other branches). CI runs on branches `lwt` and `test-suite`. Concurrency code uses `Lwt.Syntax`/`Lwt_io`, and `Strings.parse` returns `string Core.String.Table.t Lwt.t`.

## Essential Commands

### Build
- **Development build**: `dune build src/cli/strings.exe`
- **Watch mode**: `dune build src/cli/strings.exe -w`
- **Release build**: `DUNE_PROFILE=release dune build src/cli/strings.exe`
- **Full release cycle** (strip, Docker/Linux): see `DEVELOPMENT.md`.
- If `dune` is not on PATH, run `eval $(opam env)` first (or prefix with `opam exec --`).

### Test
```sh
eval $(opam env)
dune runtest tests/
```
This runs both the inline unit tests (`tests/test_runner.ml`) and an integration test defined as a `runtest` rule in `tests/dune`, which builds the CLI, runs it against `tests/fixtures/` in a temp directory, and verifies that existing French translations are preserved and `MISSING TRANSLATION` markers are emitted.

### Run
- After building: `./_build/default/src/cli/strings.exe [directory-to-extract-from]`
- The CLI **fails with "This program must be run from the root of your project"** unless the working directory contains either a `strings/` directory or a `.git` directory.
- Output directory defaults to `strings/`; override with `--output DIR` (`-o`).
- All long flags require the full `--` form (`~full_flag_required` is set everywhere).

### CLI Flags (actual, from `src/cli/strings.ml`)
- `--output DIR` / `-o`: change output directory (default `strings`).
- `--ts`: treat scripts in HTML/Pug element attributes as TypeScript.
- `--slow-pug` / `--sp`: use the official Pug parser via QuickJS instead of the fast native OCaml one.
- `--debug-pug` / `--dp` and `--debug-html` / `--dh`: debug template parsing in `.vue` files (mutually exclusive).
- There is **no** `--show-debugging` flag.

## Setup Gotchas (things that break builds)

- **Flow symlinks**: `src/flow_parser`, `src/sedlex`, and `src/collections` are symlinks into a cloned `flow` repo (v0.183.1) at the project root. If they're missing or dangling, builds fail with module errors. Recreate per `DEVELOPMENT.md`.
- **QuickJS dependency**: Requires a compiled `quickjs` directory (quickjs-2021-03-27, `make` run) at the project root. `dune` rules in `src/quickjs/dune` copy `quickjs.h`, `libquickjs.a`, and invoke `quickjs/qjsc` from there.
- **Generated runtime**: `src/quickjs/runtime.h` is generated at build time from `src/quickjs/parsers.js` via `npx browserify` then `qjsc`. Requires `npm install --no-save typescript browserify pug-lexer pug-parser pug-walk` at the repo root.
- **libomp**: `src/quickjs/dune` searches a hardcoded list of paths for `libomp.a`/`libgomp.a` (Homebrew Cellar paths on macOS, `/usr/lib/...` on Linux). If your system has it elsewhere, the build fails with "Could not find libomp.a" — add your path to the list in `src/quickjs/dune`.
- **Link flags**: Platform/profile-specific link flags live in `src/cli/link_flags.{system}.{dev,release}.dune` (the Linux dev one is just `()`). A missing file for your platform/profile combination breaks the build.
- **Version number**: `let version = "x.y.z"` in `src/cli/strings.ml` must be bumped manually for releases.

## Code Conventions & Patterns

### Parsing Strategy
1. **Direct Parsers**: Simple formats like `.strings`, `HTML`, and basic `Vue` tags are parsed using `Angstrom` in `src/parsing/`.
2. **JS/TS Parsing**:
   - JavaScript uses `Flow_parser` and a custom AST walker in `src/parsing/js_ast.ml`.
   - TypeScript uses the official TS parser running inside QuickJS (`src/quickjs/`).
3. **Pug Parsing**: Has a "fast" OCaml implementation (`src/parsing/pug.ml`) and a "slow" official Pug implementation via QuickJS (enabled with `--slow-pug`).

### Extraction Pattern
- Content is extracted into a `Utils.Collector.t` (`{ path; strings: string Queue.t; ... }`).
- The collector tracks found strings, potential scripts (to be further parsed), and file errors. Use `Collector.blit_transfer` to merge collectors.
- **Convention**: Strings found inside `L("...")` calls are treated as translations in JS/TS.

### Concurrency
- Uses `Lwt` for cooperative concurrency.
- Parallel traversal of directories is handled in `src/cli/strings.ml` via `Lwt_list` and `Lwt_pool`.
- JS workers (QuickJS) are managed via `Lwt_pool` and `Lwt_preemptive` in `src/quickjs/quickjs.ml`.
- Angstrom parsing of channels uses `Angstrom_lwt_unix.parse` taking an `Lwt_io.input_channel` (not raw strings or Eio flows).

### Style
- **OCamlFormat**: `.ocamlformat` defines a custom "Asemio Style" (margin 106, `break-cases=all`, `if-then-else=keyword-first`, etc.). Format OCaml code before submitting.
- **Memory Safety**: Be cautious with C++ FFI code in `src/quickjs/quickjs.cpp`, particularly regarding OCaml's GC interaction (`CAMLparam`, `CAMLreturn`, `caml_release_runtime_system`).

## Testing Approach

- **Inline Tests**: `ppx_inline_test` (`let%test_unit`) with `ppx_assert` (`[%test_eq:]`). Tests live in `tests/test_runner.ml` (a library with `(inline_tests)`); parsers in `src/parsing/` can also be tested inline.
- **Lwt in tests**: Wrap async test bodies in `Lwt_main.run`. For testing `Strings.parse` without the filesystem, build an in-memory channel: `Lwt_io.of_bytes ~mode:Lwt_io.input (Lwt_bytes.of_string content)`.
- **Synchronous parsers**: `Js.extract_to_collector` and the HTML/Pug parsers work on raw strings synchronously — no Lwt needed in those tests.
- **Integration Tests**: `tests/dune` contains a bash-based `runtest` rule using fixtures in `tests/fixtures/` (demo.html, demo.js, demo.pug, demo.vue). The CI workflow (`.github/workflows/test.yml`) requires `mkdir -p strings` before `dune runtest tests/`.

## Troubleshooting

### "File modified since last read"
If you receive an error stating that a file has been **"modified since it was last read"**, it usually indicates a discrepancy between the file's filesystem timestamp and the internal state tracking.

**Example Error:**
> `Edit failed: The file '/path/to/file' was modified since it was last read. Please read the file again before trying to edit it.`

**Recommended Fix:**
1. Execute `touch filename` to reset the file's modification time to the current system time.
2. Re-read the file using the `view` tool.
3. Attempt the edit again.
