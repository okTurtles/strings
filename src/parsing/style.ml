open! Core

type raw = CSS of string [@@deriving sexp, yojson]

let boundaries = Basic.boundary_parsers "style"

let parser buf = Basic.block_parser boundaries buf ~f:(fun raw _attrs -> CSS raw)
