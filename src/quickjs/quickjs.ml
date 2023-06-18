open! Core
open Eio.Std

type kind =
  | Typescript
  | Pug

let fn_name_of_kind = function
| Typescript -> "extractFromTypeScript"
| Pug -> "extractFromPug"

external stub_init_contexts : int -> (unit, string) Result.t = "stub_init_contexts"

external stub_extract : int -> string -> fn_name:string -> (string array * string array, string) Result.t
  = "stub_extract"

module Pool : sig
  type t

  val create : int -> t

  val with_pool : t -> f:(int -> 'a) -> 'a
end = struct
  type t = int Eio.Stream.t

  let create n =
    let stream = Eio.Stream.create n in
    for i = 0 to n - 1 do
      Eio.Stream.add stream i
    done;
    stream

  let with_pool stream ~f =
    let id = Eio.Stream.take stream in
    let result =
      try f id with
      | exn ->
        Eio.Stream.add stream id;
        raise exn
    in
    Eio.Stream.add stream id;
    result
end

let init_time = Atomic.make Int63.zero

let init_contexts =
  let initialized = Atomic.make None in
  let p, w = Promise.create () in
  let cell = Some p in
  fun () ->
    let num_js_workers = Utils.Io.num_js_workers in
    if Atomic.compare_and_set initialized None cell
    then (
      let time = Utils.Timing.start () in
      (fun () -> stub_init_contexts num_js_workers) () |> Result.ok_or_failwith;

      let time = time `Stop in
      Atomic.set init_time time;
      Promise.resolve w (Pool.create num_js_workers);
      print_endline
        (sprintf
           !"✅ [%{Int63}ms] Initialized %d JS runtimes for TS and/or Pug processing\n"
           time num_js_workers ) );
    p

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
  let pool = Promise.await (init_contexts ()) in
  let code =
    match kind with
    | Typescript -> code
    | Pug -> clean_pug code
  in
  let fn_name = fn_name_of_kind kind in
  Pool.with_pool pool ~f:(fun id -> stub_extract id code ~fn_name)

let extract_to_collector (collector : Utils.Collector.t) kind code =
  match extract kind code with
  | Error msg -> Queue.enqueue collector.file_errors msg
  | Ok (strings, possible_scripts) ->
    Array.iter strings ~f:(Queue.enqueue collector.strings);
    Array.iter possible_scripts ~f:(Queue.enqueue collector.possible_scripts)
