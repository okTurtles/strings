# Project Knowledge - Feedback Task

## Key Gotchas

- The `lwt` branch uses `Angstrom_lwt_unix.parse` which takes `Lwt_io.input_channel`, not strings or Eio flows.
- `Strings.parse` on lwt returns `string Core.String.Table.t Lwt.t` (wrapped in Lwt).
- The lwt `exec_parser_lwt` error handler signature is `~unparsed:string -> 'a option -> 'b Lwt.t` (different from Eio's).
- `exec_parser` (non-Lwt, non-Eio) works on raw strings and is synchronous — the JS/HTML/Pug tests use this and should work without changes on both branches.
- The test-suite branch was 6 commits ahead of master (Eio). Rebasing onto lwt was impractical due to heavy divergence — instead we reset the branch to `origin/lwt` and recreated all PR changes from scratch, adapted for Lwt.
- `ppx_inline_test` tests using `%test_unit` work fine with Lwt as long as we use `Lwt_main.run` inside the test body.
- `Lwt_io.of_bytes ~mode:Lwt_io.Input` can create an in-memory input channel from `Lwt_bytes.t` for testing `Strings.parse` without touching the filesystem.
- The lwt quickjs/dune keeps `lwt` and `lwt.unix` in library deps (the Eio port removed them).
- The lwt `src/cli/dune` uses `angstrom-lwt-unix`, `lwt`, `lwt.unix` (not `eio_main`, `angstrom-eio`).
- CI workflow branches should reference `lwt` (not `main` or `master`) since that's the active build branch.
- There was no `link_flags.linux.dev.dune` on the lwt branch — we created it with `()` (empty flags, same as the Eio PR did).
- The `default_error_handler` signature differs between branches: lwt uses `~unparsed -> 'a`, Eio uses `?unparsed ~msg -> unit -> 'a`.
- The `DEVELOPMENT.md` already exists on lwt and references lwt-compatible setup — no changes needed.
