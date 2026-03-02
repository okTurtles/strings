# Update AGENTS.md to reference Lwt instead of Eio

Status: COMPLETED

## Sub tasks

1. [x] Replace "Eio (concurrency)" with "Lwt (concurrency)" in Key Libraries
2. [x] Replace Eio concurrency description with Lwt (`Lwt_list`, `Lwt_pool`, `Lwt_preemptive`)
3. [x] Remove `Fiber.List.iter` / `Eio.Executor_pool` references

## NOTES

Created AGENTS.md from scratch on the lwt base. All references use Lwt terminology: `Lwt_pool`, `Lwt_list`, `Lwt_preemptive`.
