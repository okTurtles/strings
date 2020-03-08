open Core_kernel

type raw = string list [@@deriving sexp, yojson]

let parser =
  let open Angstrom in
  let open Basic in

  let js_begin = string "<script" *> skip_remaining *> end_of_line in
  let js_end = string "</script>" in
  let line = take_remaining <* (advance 1) in
  js_begin *> (many_till line js_end)
