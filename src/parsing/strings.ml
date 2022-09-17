open! Core

type line =
  | Translation of (string * string)
  | Comment

let parser =
  let open Angstrom in
  let open Basic in
  let double_quoted_string = escapable_string_parser ~escape:'\\' ~separator:'"' in
  let line =
    lift2
      (fun x y -> Translation (x, y))
      (mlws *> double_quoted_string <* mlws <* char '=')
      (mlws *> double_quoted_string <* mlws <* char ';' <* mlws)
  in
  let comment =
    (mlws
    *> string "/*"
    *>
    let rec loop n =
      any_char >>= function
      | '*' when Int.(n = 0) -> loop 1
      | '/' when Int.(n = 1) -> return Comment
      | _ -> loop 0
    in
    loop 0)
    <* mlws
  in
  many (line <|> comment) <* mlws

let parse ~path ic =
  let open Lwt.Syntax in
  let table = String.Table.create () in

  let error_handler ~unparsed _opt =
    failwithf
      "There is a syntax error in file [%s].\n\
       Translations must follow this format and end in a semicolon: \"english text\" = \"translated \
       text\";\n\
       This line is malformed:\n\
       %s"
      path
      (String.take_while ~f:(Char.( <> ) '\n') unparsed)
      ()
  in
  let+ lines =
    Basic.exec_parser_lwt ~on_ok:Lwt.return ~on_error:error_handler parser ~path ~language_name:".strings"
      ic
  in
  List.iter lines ~f:(function
    | Translation (x, y) -> String.Table.set table ~key:x ~data:y
    | Comment -> ());
  table
