# Rewrite test_runner.ml for Lwt

Status: COMPLETED

## Sub tasks

1. [x] Replace `Eio_posix.run` with `Lwt_main.run`
2. [x] Replace `Eio.Flow.string_source` with `Lwt_io.of_bytes ~mode:Lwt_io.input`
3. [x] Use `Lwt.Syntax` `let+` for Lwt promise handling
4. [x] Keep JS/HTML/Pug tests unchanged (they use synchronous `exec_parser`)

## NOTES

Key changes in test_runner.ml:
- `strings_parsing` and `french_strings_parsing` tests now use `Lwt_main.run` instead of `Eio_posix.run`
- Created `Lwt_io` input channels from in-memory byte strings using `Lwt_io.of_bytes ~mode:Lwt_io.input @@ Lwt_bytes.of_string content`
- Used `let+` from `Lwt.Syntax` since `Strings.parse` returns `Lwt.t`
- JS, HTML, and Pug tests are unchanged as they use the synchronous `Basic.exec_parser`
- Initially wrote `Lwt_io.Input` (uppercase) — corrected to `Lwt_io.input` (lowercase value) after verifying against Lwt's own test suite via Sourcegraph
