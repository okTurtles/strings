open! Core

type line =
  | Translation of (string * string)
  | Comment

let parser ~dq_string =
  let open Angstrom in
  let open Basic in
  let line =
    lift2
      (fun x y -> Translation (x, y))
      (mlws *> dq_string <* mlws <* char '=')
      (mlws *> dq_string <* mlws <* char ';' <* mlws)
  in
  let comment =
    ( mlws
    *> string "/*"
    *>
    let rec loop n =
      any_char >>= function
      | '*' when Int.(n = 0) -> loop 1
      | '/' when Int.(n = 1) -> return Comment
      | _ -> loop 0
    in
    loop 0 )
    <* mlws
  in
  many (line <|> comment) <* mlws

let parse ~path flow =
  let table = String.Table.create () in

  let error_handler ?unparsed ~msg () =
    let preview =
      match unparsed with
      | None -> ""
      | Some s -> sprintf "\nThis line is malformed:\n%s" (String.take_while ~f:(Char.( <> ) '\n') s)
    in
    failwithf
      "There is a syntax error in file [%s] (%s).\n\
       Translations must follow this format and end in a semicolon: \"english text\" = \"translated \
       text\";%s"
      path msg preview ()
  in
  let dq_string = Basic.make_dq_string () in
  let lines =
    Basic.exec_parser_eio ~on_ok:Fn.id ~on_error:error_handler (parser ~dq_string) ~path
      ~language_name:".strings" flow
  in
  List.iter lines ~f:(function
    | Translation (x, y) -> Hashtbl.set table ~key:x ~data:y
    | Comment -> () );
  table
