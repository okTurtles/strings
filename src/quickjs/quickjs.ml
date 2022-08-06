open Core
open Lwt.Infix

external stub_init_context : string -> string -> (unit, string) Result.t = "stub_init_context"

external stub_extract_ts : string -> string -> (string array, string) Result.t = "stub_extract_ts"

let init_code = [%blob "runtime.js"]

let init_context () =
  let id = Uuidm.create `V4 |> Uuidm.to_string in
  Lwt_preemptive.detach (stub_init_context id) init_code >|= function
  | Ok () -> id
  | Error msg -> failwith msg

let js_contexts = lazy (Lwt_pool.create 4 init_context)

let extract_ts code =
  Lwt_pool.use (force js_contexts) (fun id -> Lwt_preemptive.detach (stub_extract_ts id) code)
  >|= Result.ok_or_failwith
