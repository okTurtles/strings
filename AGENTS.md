# Agent Information - String Extractor

This repository contains an OCaml-based internationalization (i18n) string extraction tool. It parses source files (JS, TS, Vue, Pug, HTML) and extracts strings for translation management.

## Documentation

- **[ARCHITECTURE.md](ARCHITECTURE.md)**: Contains a deep-dive into the codebase layout, directory structure, and a comprehensive API reference. **Read this file first** when:
  - Starting a new task to understand which files are relevant.
  - Investigating the impact of changes across the system.
  - Looking for specific functionality or function definitions before searching.

- **[DEVELOPMENT.md](DEVELOPMENT.md)**: Contains instructions for environment setup, build processes for various platforms, and release workflows. **Read this file first** when:
  - Setting up the development environment or installing dependencies (OCaml, JS, QuickJS).
  - Building the project for development or release.
  - Executing the tool for manual verification or testing.
  - Managing version numbers or release artifacts.

## Project Overview

- **Language**: OCaml (5.1.1) with some C++ (QuickJS bridge) and JavaScript (parsers via Browserify).
- **Architecture**:
  - `src/cli/`: Main entry point, command-line interface, and output generation logic.
  - `src/parsing/`: OCaml parsers using `Angstrom` for custom formats and `Flow_parser` for JS.
  - `src/quickjs/`: Bridge to QuickJS to run JavaScript-based parsers (TypeScript/Pug) from OCaml.
  - `src/utils/`: Common utilities for collection, timing, and I/O.
- **Key Libraries**: `Core`, `Lwt` (concurrency), `Angstrom` (parsing), `Yojson`, `Ppx_jane`.

## Essential Commands

### Build
- **Development build**: `dune build src/cli/strings.exe`
- **Watch mode**: `dune build src/cli/strings.exe -w`
- **Release build (MacOS)**: `DUNE_PROFILE=release dune build src/cli/strings.exe`
- **Full release cycle**: See `DEVELOPMENT.md` for `cp`, `strip`, and Docker commands.

### Run
- After building: `./_build/default/src/cli/strings.exe [directory-to-extract-from]`
- The CLI expects to be run from the root of a project containing a `strings/` directory (or it will create one if a `.git` folder is present).

### Installation (Dev Setup)
Refer to `DEVELOPMENT.md` for specific `opam` and `npm` setup steps, as the project has several external dependencies (Flow, QuickJS, pug-lexer, etc.).

## Code Conventions & Patterns

### Parsing Strategy
1. **Direct Parsers**: Simple formats like `.strings`, `HTML`, and basic `Vue` tags are parsed using `Angstrom` in `src/parsing/`.
2. **JS/TS Parsing**: 
   - Javascript uses `Flow_parser` and a custom AST walker in `src/parsing/js_ast.ml`.
   - TypeScript uses the official TS parser running inside QuickJS (`src/quickjs/`).
3. **Pug Parsing**: Has a "fast" OCaml implementation (`src/parsing/pug.ml`) and a "slow" official Pug implementation via QuickJS (`src/quickjs/`).

### Extraction Pattern
- Content is extracted into a `Utils.Collector.t`.
- The collector tracks found strings, potential scripts (to be further parsed), and file errors.
- **Convention**: Strings found inside `L("...")` calls are treated as translations in JS/TS.

### Concurrency
- Uses `Lwt` for cooperative concurrency.
- Parallel traversal of directories is handled in `src/cli/strings.ml` via `Lwt_list` and `Lwt_pool`.
- JS workers (QuickJS) are managed via `Lwt_pool` and `Lwt_preemptive` in `src/quickjs/quickjs.ml`.

## Important Gotchas

- **QuickJS Dependency**: Requires a compiled `quickjs` directory at the project root for building. `dune` rules in `src/quickjs/dune` copy headers and libraries from there.
- **Generated Headers**: `src/quickjs/runtime.h` is generated from `src/quickjs/parsers.js` using `browserify` and `qjsc`.
- **Linking**: MacOS builds use specific link flags (e.g., `ld64.lld`) defined in `src/cli/link_flags.*`.
- **OCamlFormat**: `.ocamlformat` is present; ensure you format OCaml code before submitting.
- **Memory Safety**: Be cautious with C++ FFI code in `src/quickjs/quickjs.cpp`, particularly regarding OCaml's GC interaction (`CAMLparam`, `CAMLreturn`, `caml_release_runtime_system`).

## Testing Approach

- **Inline Tests**: The project uses `ppx_inline_test`. Parsers in `src/parsing/` can be tested directly within the OCaml files or in the `tests/` directory.
- **Test Suite**: A standard test suite is located in `tests/test_runner.ml`. It covers JS, HTML, Pug, and `.strings` file parsing.
- **Integration Tests**: Verification can be performed by running the built binary against fixtures in `tests/fixtures/` and checking the generated output in the `strings/` directory.
- **Debug Flags**: Use `--show-debugging` or `--debug-pug` / `--debug-html` flags in the CLI to inspect internal parsing results.

## Troubleshooting

### "File modified since last read"
If you receive an error stating that a file has been **"modified since it was last read"**, it usually indicates a discrepancy between the file's filesystem timestamp and the internal state tracking.

**Example Error:**
> `Edit failed: The file '/path/to/file' was modified since it was last read. Please read the file again before trying to edit it.`

**Recommended Fix:**
1. Execute `touch filename` to reset the file's modification time to the current system time.
2. Re-read the file using the `view` tool.
3. Attempt the edit again.
