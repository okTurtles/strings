open Core
open Lwt.Infix
open Lwt.Syntax

external stub_init_contexts : int -> (unit, string) Result.t = "stub_init_contexts"

external stub_extract_ts : int -> string -> (string array, string) Result.t = "stub_extract_ts"

let num_threads = 4

let init_time = ref Int63.zero

let init_contexts =
  lazy
    (let time = Utils.time () in
     Lwt_preemptive.detach (fun () -> stub_init_contexts num_threads) () >>= function
     | Ok () ->
       let time = time () in
       init_time := time;
       Lwt_io.printlf !"✅ [%{Int63}ms] Initialized %d JS runtimes for TS processing" time num_threads
     | Error msg -> failwith msg)

let js_contexts =
  let ctr = ref 0 in
  Lwt_pool.create num_threads (fun () ->
      let i = !ctr in
      incr ctr;
      Lwt.return i)

let extract_ts code =
  let* () = force init_contexts in
  Lwt_pool.use js_contexts (fun id -> Lwt_preemptive.detach (fun () -> stub_extract_ts id code) ())
  >|= Result.ok_or_failwith
