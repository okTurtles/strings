open Core
open Angstrom

let lowercase = function
| 'a' .. 'z' -> true
| _ -> false

let alphanum = function
| 'a' .. 'z'
 |'A' .. 'Z'
 |'0' .. '9' ->
  true
| _ -> false

let is_identifier = function
| 'a' .. 'z'
 |'A' .. 'Z'
 |'0' .. '9'
 |'_' ->
  true
| _ -> false

let is_ws = function
| '\x20'
 |'\x0d'
 |'\x09' ->
  true
| _ -> false

let is_mlws = function
| '\x20'
 |'\x0d'
 |'\x09'
 |'\x0a' ->
  true
| _ -> false

let ws = skip_while is_ws

let ws1 = satisfy is_ws *> ws

let mlws = skip_while is_mlws

let mlws1 = satisfy is_mlws *> mlws

let take_remaining = take_while (Char.( <> ) '\x0a')

let skip_remaining = skip_while (Char.( <> ) '\x0a')

let maybe p = option None (p >>| Option.return)

let escapable_string_parser ~escape ~separator =
  let is_separator = Char.( = ) separator in
  let is_escape = Char.( = ) escape in
  let buf = Buffer.create 50 in
  (char separator
  *>
  let rec loop escaping =
    any_char >>= fun x ->
    match x, escaping with
    | c, false when is_escape c -> loop true
    | c, true when is_separator c ->
      Buffer.add_char buf c;
      loop false
    | c, false when is_separator c ->
      let result = Buffer.contents buf in
      Buffer.clear buf;
      return result
    | c, true ->
      Buffer.add_char buf c;
      loop false
    | c, false ->
      Buffer.add_char buf c;
      loop escaping
  in
  loop false)
  <?> "Escapable string"

let boundary_parsers tag =
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
    lift2 Tuple2.create
      (mlws *> take_while1 is_identifier)
      (mlws *> maybe (char '=' *> mlws *> quoted_string))
  in
  let starts = char '<' *> mlws *> string tag *> many attribute <* mlws <* char '>' in
  let ends = string "</" *> mlws *> string tag <* mlws <* char '>' in
  starts, ends

let block_parser (starts, ends) buf ~f =
  let line =
    take_remaining <* advance 1 >>| fun src_line ->
    Buffer.add_string buf src_line;
    Buffer.add_char buf '\n'
  in
  starts
  >>| (fun x ->
        Buffer.clear buf;
        x)
  <* many_till line ends
  >>| fun x -> f (Buffer.contents buf) x

let exec_parser parser ~filename ~language_name raw =
  let result = Angstrom.parse_string ~consume:All parser raw in
  match result with
  | Ok parsed -> parsed
  | Error err ->
    failwithf
      "The file [%s] contains invalid syntax or %s features unsupported by this tool.\n\
       Please report this so it can be improved. Error: %s" filename language_name err ()
