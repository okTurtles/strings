open Core

type raw =
  | HTML of string
  | PUG  of string
[@@deriving sexp, yojson]

let boundaries =
  let open Angstrom in
  let starts, ends = Basic.boundary_parsers "template" in
  let starts =
    starts >>| fun attrs ->
    List.find_map attrs ~f:(function
      | "lang", Some "html" -> Some `HTML
      | "lang", Some "pug" -> Some `PUG
      | _ -> None)
  in
  starts, ends

let parser buf =
  Basic.block_parser boundaries buf ~f:(fun raw -> function
    | None
     |Some `HTML ->
      HTML raw
    | Some `PUG -> PUG raw)
