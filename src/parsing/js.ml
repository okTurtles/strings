open Core_kernel

type raw = string array [@@deriving sexp, yojson]

let parser =
  let open Angstrom in
  let open Basic in

  let js_begin = string "<script" *> skip_remaining *> end_of_line in
  let js_end = string "</script>" in
  let buf = Buffer.create 64 in
  let line = (take_remaining <* (advance 1)) >>| (fun src_line ->
      Buffer.add_string buf src_line;
      Buffer.add_char buf '\n'
    )
  in
  js_begin *> (many_till line js_end) *> (return ())
  >>| (fun () ->
    let source = Buffer.contents buf in
    let queue = Queue.create () in
    Js_ast.strings queue source;
    Queue.to_array queue

  )
