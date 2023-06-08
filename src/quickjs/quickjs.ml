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
    (let time = Utils.Timing.start () in
     Lwt_preemptive.detach (fun () -> stub_init_contexts num_threads) () >>= function
     | Ok () ->
       let time = time `Stop in
       init_time := time;
       Lwt_io.printlf
         !"✅ [%{Int63}ms] Initialized %d JS runtimes for TS and/or Pug processing"
         time num_threads
     | Error msg -> failwith msg )

let js_contexts =
  let ctr = ref 0 in
  Lwt_pool.create num_threads (fun () ->
    let i = !ctr in
    incr ctr;
    Lwt.return i )

(* Re-indent from 0 if base indent is greater than 0 *)
let clean_pug code =
  let rec next i =
    match String.index_from code i '\n' with
    | None -> Seq.Nil
    | Some x -> Seq.Cons (x, (fun () -> next (x + 1)))
  in
  Sequence.of_seq (fun () -> next 0)
  |> Sequence.folding_map ~init:0 ~f:(fun start stop ->
       stop, if stop > 0 then String.slice code (start + 1) stop else "" )
  |> Sequence.filter_map ~f:(function
       | "" -> None
       | s -> (
         match String.lfindi s ~f:(fun _i c -> Char.(c <> ' ')) with
         | None as x -> x
         | Some indent ->
           let is_comment = String.is_substring_at code ~pos:indent ~substring:"//" in
           Some (indent, is_comment, s) ) )
  |> Sequence.fold_until ~init:None ~finish:(Option.value ~default:0) ~f:(fun in_comment data ->
       match in_comment, data with
       | None, (indent, true, _) -> Continue (Some indent)
       | Some indent1, (indent2, true, _) -> Continue (Some (min indent1 indent2))
       | Some stop, (indent, false, _) when stop <= indent -> Stop indent
       | Some stop, (_, false, _) -> Continue (Some stop)
       | None, (indent, false, _) -> Stop indent )
  |> function
  | 0 -> code
  | shift -> String.substr_replace_all code ~pattern:(sprintf "\n%s" (String.make shift ' ')) ~with_:"\n"

let extract kind code =
  let* () = force init_contexts in
  let code =
    match kind with
    | Typescript -> code
    | Pug -> clean_pug code
  in
  let fn_name = fn_name_of_kind kind in
  Lwt_pool.use js_contexts (fun id -> Lwt_preemptive.detach (fun () -> stub_extract id code ~fn_name) ())

let extract_to_collector (collector : Utils.Collector.t) kind code =
  extract kind code >|= function
  | Error msg -> Queue.enqueue collector.file_errors msg
  | Ok (strings, possible_scripts) ->
    Array.iter strings ~f:(Queue.enqueue collector.strings);
    Array.iter possible_scripts ~f:(Queue.enqueue collector.possible_scripts)
