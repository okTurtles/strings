open! Core
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
 |'_'
 |'-' ->
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
  char separator
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
  loop false
  <|> ( return () >>= fun () ->
        Buffer.clear buf;
        fail "Invalid escapable string" )

let make_sq_string () = escapable_string_parser ~escape:'\\' ~separator:'\''

let make_dq_string () = escapable_string_parser ~escape:'\\' ~separator:'"'

type string_parsers = {
  sq_string: string Angstrom.t;
  dq_string: string Angstrom.t;
}

let make_string_parsers () = { sq_string = make_sq_string (); dq_string = make_dq_string () }

let boundary_parsers tag =
  let sq_string = make_sq_string () in
  let dq_string = make_dq_string () in
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

let block_parser boundaries buf ~f =
  let starts, ends = boundaries () in
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

let default_error_handler ~path ~language_name ?unparsed ~msg () =
  let preview =
    match unparsed with
    | None -> ""
    | Some s ->
      sprintf
        !"\nThe unsupported syntax starts at:\n%{Yojson.Basic}"
        (`String (String.slice s 0 Int.(min 20 (String.length s))))
  in
  failwithf
    "The file [%s] contains invalid syntax or %s features unsupported by this tool. (%s)\n\
     Please report this so it can be improved.%s" path language_name msg preview ()

let default_syntax_error_handler ~path ~language_name ~msg =
  failwithf
    "The file [%s] contains invalid syntax or %s features unsupported by this tool.\n\
     If you are certain the syntax is valid, then please report this error.\n\
     Error: %s" path language_name msg ()

let exec_parser ~on_ok ?on_error parser ~path ~language_name raw =
  Angstrom.parse_string ~consume:All parser raw |> function
  | Ok x -> on_ok x
  | Error msg -> (
    match on_error with
    | None -> default_syntax_error_handler ~path ~language_name ~msg
    | Some handler -> handler ~msg )

let exec_parser_eio ~on_ok ?on_error parser ~path ~language_name source =
  let get_handler () =
    match on_error with
    | None -> default_error_handler ~path ~language_name
    | Some x -> x
  in
  match Angstrom_eio.parse parser source with
  | { len = 0; _ }, Ok x -> on_ok x
  | { buf; len; off }, Ok _ ->
    (get_handler ())
      ~unparsed:(Bigstringaf.substring buf ~off ~len)
      ~msg:"Not all input could be processed. There must be invalid syntax." ()
  | { buf; len; off }, Error msg ->
    (get_handler ()) ~unparsed:(Bigstringaf.substring buf ~off ~len) ~msg ()
