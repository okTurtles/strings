# Update ARCHITECTURE.md to reference Lwt instead of Eio

Status: COMPLETED

## Sub tasks

1. [x] Replace "Eio runtime" with "Lwt runtime" in Project Entry Point
2. [x] Update Control Flow section (Eio → Lwt, `Lwt_list` and `Lwt_pool`)
3. [x] Update `Parsing.Strings.parse` API description to mention `Lwt_io.input_channel`
4. [x] Update Testing Setup section to reference `Lwt_main.run` and `Lwt_io`

## NOTES

Created ARCHITECTURE.md from scratch on the lwt base. Key differences from Eio version:
- Control flow references `Lwt` cooperative concurrency via `Lwt_list` and `Lwt_pool`
- `Strings.parse` documented as taking `Lwt_io.input_channel` returning `Lwt.t`
- Testing section notes `.strings` tests use `Lwt_main.run`
