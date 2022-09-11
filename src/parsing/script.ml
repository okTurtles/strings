open! Core

type raw =
  | JS of string
  | TS of string
[@@deriving sexp, yojson]

let boundaries =
  let open Angstrom in
  let starts, ends = Basic.boundary_parsers "script" in
  let starts =
    starts >>| fun attrs ->
    List.find_map attrs ~f:(function
      | "lang", Some "js" -> Some `JS
      | "lang", Some "ts" -> Some `TS
      | _ -> None)
  in
  starts, ends

let parser buf =
  Basic.block_parser boundaries buf ~f:(fun raw -> function
    | None
     |Some `JS ->
      JS raw
    | Some `TS -> TS raw)
