open Core

type raw =
  | JS of string
  | TS of string
[@@deriving sexp, yojson]

let parser buf =
  let open Angstrom in
  let open Basic in
  Buffer.clear buf;
  let sq_string = escapable_string_parser ~escape:'\\' ~separator:'\'' in
  let dq_string = escapable_string_parser ~escape:'\\' ~separator:'"' in
  let quoted_string =
    peek_char >>= function
    | Some '\'' -> sq_string
    | Some '"' -> dq_string
    | Some _ -> fail "Not a string"
    | None -> sq_string <|> dq_string
  in
  let attribute =
    lift2 Tuple2.create (mlws *> take_while is_identifier) (mlws *> char '=' *> mlws *> quoted_string)
  in
  let script_begin =
    char '<' *> mlws *> string "script" *> many attribute <* mlws <* char '>' >>| fun attrs ->
    List.find_map attrs ~f:(function
      | "lang", "js" -> Some `JS
      | "lang", "ts" -> Some `TS
      | _ -> None)
  in
  let script_end = string "</script>" in
  let line =
    take_remaining <* advance 1 >>| fun src_line ->
    Buffer.add_string buf src_line;
    Buffer.add_char buf '\n'
  in
  script_begin <* many_till line script_end >>| function
  | None
   |Some `JS ->
    JS (Buffer.contents buf)
  | Some `TS -> TS (Buffer.contents buf)
