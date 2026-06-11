# Architecture Documentation - String Extractor

This document provides a high-level overview of the String Extractor's architecture, directory structure, and internal APIs.

## Project Entry Point

The main entry point of the application is **`src/cli/strings.ml`**. It handles command-line argument parsing using `Core.Command`, sets up the `Lwt` runtime, and initiates the file traversal process.

## Directory Structure

```text
/
├── src/
│   ├── cli/            # Main CLI application logic
│   │   ├── strings.ml  # CLI entry point, traversal coordination
│   │   ├── vue.ml      # Vue-specific parsing and extraction logic
│   │   └── generate.ml # Localization file generation (.strings, .json)
│   ├── parsing/        # Core parsers using Angstrom and Flow
│   │   ├── basic.ml    # Common parsing utilities and combinators
│   │   ├── js_ast.ml   # Flow AST walker for string extraction
│   │   ├── js.ml       # JavaScript string extraction entry point
│   │   ├── pug.ml      # Native Pug template parsing
│   │   ├── html.ml     # HTML template parsing
│   │   ├── astro.ml    # Native Astro file scanning (frontmatter, I18n, expressions)
│   │   ├── strings.ml  # .strings file parsing logic
│   │   └── ...         # Other specialized parsers (vue blocks, styles)
│   ├── quickjs/        # Interface to QuickJS for JS/TS/Pug parsing
│   │   ├── quickjs.ml  # OCaml FFI to QuickJS
│   │   ├── quickjs.cpp # C++ implementation of the bridge
│   │   └── parsers.js  # JS-based parsers running in QuickJS
│   └── utils/          # Shared utility modules
│       ├── collector.ml # State container for collected strings/errors
│       ├── io.ml       # I/O helpers
│       ├── timing.ml   # Performance measurement
│       └── exception.ml # Exception handling
├── strings/            # Directory where .strings files are managed
├── dune-project        # Dune build system configuration
└── README.md           # Project overview and usage instructions
```

## Core API Reference

### `src/cli/`
- **`Strings.main`**: Coordinates the entire run, including directory traversal and result generation.
- **`Vue.parse`**: Splits a `.vue` file into its constituent parts (template, script, style).
- **`Generate.write_english`**: Creates `english.strings` and `english.json` from the collected strings.
- **`Generate.write_other`**: Updates existing translations for other languages.

### `src/parsing/`
- **`Parsing.Basic`**: Provides foundational Angstrom parsers for whitespace, strings, and standard error handling.
- **`Parsing.Js.extract_to_collector`**: Entry point for scanning JavaScript source code.
- **`Parsing.Js_ast.extract`**: A comprehensive walker for the Flow AST that identifies and extracts strings from `L("...")` calls.
- **`Parsing.Pug.collect`**: Traverses the native Pug AST to extract strings.
- **`Parsing.Astro.parser` / `Parsing.Astro.collect`**: Native Angstrom scanner for `.astro` files. Segments a file into frontmatter, `<script>` blocks, `{...}` expressions (brace matching respects strings, template literals, and comments), and `<I18n>`/`<i18n>` blocks. `collect` enqueues I18n slot text into `strings`, all code segments into `possible_scripts` (always parsed as TypeScript), and emits a non-fatal warning when I18n text contains `{placeholders}` without the `is:raw` directive. `parser` takes `unit` because it uses an internal shared buffer — create a fresh parser per file.
- **`Parsing.Strings.parse`**: Parses existing `.strings` files into a lookup table. Takes a `Lwt_io.input_channel` and returns a `string Core.String.Table.t Lwt.t`.

### `src/quickjs/`
- **`Quickjs.extract_to_collector`**: Offloads extraction to QuickJS for TypeScript and advanced Pug templates.

### `src/utils/`
- **`Utils.Collector.create`**: Initializes a new string collection state for a specific file. (type `t = { path: string; strings: string Queue.t; possible_scripts: string Queue.t; file_errors: string Queue.t; warnings: string Queue.t }`)
- **`Utils.Collector.render_errors` / `Utils.Collector.render_warnings`**: Render collected errors (`❌`, fatal) and warnings (`⚠️`, non-fatal) for terminal output.
- **`Utils.Collector.blit_transfer`**: Merges results from one collector into another.

## Control Flow
1. **Initiation**: `strings.exe` starts, parses CLI flags, and identifies the target directory.
2. **Traversal**: Uses `Lwt` to cooperatively walk the directory tree via `Lwt_list` and `Lwt_pool`.
3. **Dispatch**: For each supported file extension, the corresponding parser in `src/parsing` is invoked.
4. **Collection**: Parsers find strings (usually inside `L()`) and add them to a `Collector.t`.
5. **Generation**: `Generate.ml` aggregates strings from all collectors and updates the `strings/` directory.

## Testing Setup

The project implements a multi-layered testing strategy:

1. **Inline Tests**: Using `ppx_inline_test` (e.g. `let%test_unit`) together with `ppx_assert` (e.g. `[%test_eq]`), logic can be tested directly within the source files. This is primarily used for parser validation in `src/parsing/`.
2. **Standard Test Suite**: Located in `tests/test_runner.ml`, this suite runs the inline tests via `ppx_inline_test` and uses `ppx_assert` to verify:
   - JavaScript string extraction via `Flow_parser`.
   - HTML extraction via `SZXX` and Pug extraction via `Angstrom`.
   - Apple-style `.strings` file parsing (via `Lwt_main.run` and `Lwt_io`).
3. **Integration Testing**: The `tests/fixtures/` directory contains sample files of all supported types (including `demo.astro`). The CLI can be run against these fixtures to verify end-to-end extraction and output generation (`.strings` and `.json` files).

The `tests/dune` file configures the test library and enables inline tests for the module.
