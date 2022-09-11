open! Core
open Lwt.Infix
open Lwt.Syntax

type kind =
  | Typescript
  | Pug

let fn_name_of_kind = function
| Typescript -> "extractFromTypeScript"
| Pug -> "extractFromPug"

external stub_init_contexts : int -> (unit, string) Result.t = "stub_init_contexts"

external stub_extract : int -> string -> fn_name:string -> (string array * string array, string) Result.t
  = "stub_extract"

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

let extract kind code =
  let* () = force init_contexts in
  let fn_name = fn_name_of_kind kind in
  Lwt_pool.use js_contexts (fun id -> Lwt_preemptive.detach (fun () -> stub_extract id code ~fn_name) ())

let extract_to_collector (collector : Utils.Collector.t) kind code =
  extract kind code >|= function
  | Error msg -> Queue.enqueue collector.file_errors msg
  | Ok (strings, possible_scripts) ->
    Array.iter strings ~f:(Queue.enqueue collector.strings);
    Array.iter possible_scripts ~f:(Queue.enqueue collector.possible_scripts)
