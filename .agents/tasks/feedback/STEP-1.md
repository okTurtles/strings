# Rebase test-suite onto origin/lwt and recreate PR changes

Status: COMPLETED

## Sub tasks

1. [x] Reset `test-suite` branch to `origin/lwt` base
2. [x] Recreate all PR file additions adapted for Lwt

## NOTES

Instead of rebasing (which would produce many conflicts in files we didn't change due to heavy divergence between master/Eio and lwt), we reset the branch to `origin/lwt` and recreated all PR changes from scratch:

- `dune` — added `flow` to `data_only_dirs`
- `.gitignore` — added `tests/integration_test_run/`
- `src/cli/link_flags.linux.dev.dune` — created with `()` (empty flags)
- `src/quickjs/dune` — replaced fragile libomp.a cp chain with robust path search loop
- `tests/fixtures/` — created demo.html, demo.js, demo.pug, demo.vue
- `tests/dune` — created with lwt/lwt.unix/angstrom-lwt-unix deps (instead of eio_main)
- `tests/test_runner.ml` — rewritten for Lwt (see STEP-2)
- `.github/workflows/test.yml` — targets `lwt` branch instead of `main`
- `AGENTS.md` — created, references Lwt (not Eio)
- `ARCHITECTURE.md` — created, references Lwt (not Eio)
- `DEVELOPMENT.md` — added Testing section
