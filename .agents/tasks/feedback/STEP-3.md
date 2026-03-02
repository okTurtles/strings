# Update tests/dune dependencies (eio_main → lwt)

Status: COMPLETED

## Sub tasks

1. [x] Replace `eio_main` with `lwt lwt.unix angstrom-lwt-unix` in library deps

## NOTES

The test library in `tests/dune` was created with Lwt dependencies from the start (since we recreated all files from scratch on the lwt base in STEP-1). Final deps: `parsing utils core lwt lwt.unix angstrom-lwt-unix`.
