open Core
open Lwt.Infix
open Lwt.Syntax

type pug_response = {
  strings: string array;
  possible_js: string array;
}

type _ kind =
  | Typescript : string array kind
  | Pug : pug_response kind

let fn_name_of_kind (type a) : a kind -> string = function
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

let extract (type a) code (kind : a kind) : a Lwt.t =
  let* () = force init_contexts in
  let fn_name = fn_name_of_kind kind in
  Lwt_pool.use js_contexts (fun id -> Lwt_preemptive.detach (fun () -> stub_extract id code ~fn_name) ())
  >|= function
  | Error msg -> failwith msg
  | Ok (strings, possible_js) ->
    (match kind with
     | Typescript -> strings
     | Pug -> { strings; possible_js }
      : a)
