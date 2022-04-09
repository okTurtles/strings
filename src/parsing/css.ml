open! Core

let parser =
  let open Angstrom in
  let open Basic in
  let css_begin = string "<style" *> skip_remaining *> end_of_line in
  let css_end = string "</style>" in
  let line = take_remaining <* advance 1 in
  css_begin *> many_till line css_end *> return ()
