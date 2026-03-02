# Update CI workflow for lwt dependencies

Status: COMPLETED

## Sub tasks

1. [x] Change branch triggers from `main`/`master` to `lwt`
2. [x] Keep opam deps the same (lwt branch opam already has correct deps)

## NOTES

CI workflow `.github/workflows/test.yml` created with `lwt` and `test-suite` as push branches, and `lwt` as PR branch. The opam install step uses `--update-invariant` for flambda compatibility.
