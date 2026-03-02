# Plan - Feedback Task

## Goal

Retarget PR #7 from `master` (Eio) to `lwt` branch, rewriting all Eio-specific code to use Lwt, and updating documentation accordingly.

## Approach

1. **Rebase the `test-suite` branch onto `origin/lwt`** instead of `origin/master`. This will bring the lwt source code as the base, and our PR additions (tests, CI, docs) on top. Conflicts are expected in files we modified that differ between Eio and Lwt — we'll resolve them to match lwt.

2. **Rewrite `tests/test_runner.ml` for Lwt**: The `.strings` parsing tests use `Eio_posix.run` and `Eio.Flow.string_source`. These need to use `Lwt_main.run` and `Lwt_io` instead. JS/HTML/Pug tests that don't use Eio should be unaffected.

3. **Update `tests/dune`**: Replace `eio_main` with `lwt`, `lwt.unix`, and `angstrom-lwt-unix` in the test library dependencies.

4. **Update `AGENTS.md`**: Replace all Eio references with Lwt equivalents.

5. **Update `ARCHITECTURE.md`**: Replace all Eio references with Lwt equivalents.

6. **Update CI workflow** if needed: Branch triggers should target `lwt` instead of `main`/`master`.

7. **Change PR base branch** from `master` to `lwt` using `gh pr edit`.

8. **Verify**: Ensure tests pass or at least the code compiles correctly for the lwt branch.
