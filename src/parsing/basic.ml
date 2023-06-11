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

type 'a status =
  | Incomplete
  | Success of 'a
  | Partial of {
      partial: 'a;
      unparsed: string;
    }
  | Failed of {
      message: string;
      unparsed: string;
    }
  | Not_enough_input

class virtual ['a] parser_sink =
  object
    inherit Eio.Flow.sink

    method virtual data : 'a status
  end

let parser_sink (type a) (parser : a Angstrom.t) : a parser_sink =
  let data = ref Incomplete in
  let to_status = function
    | Angstrom.Buffered.Done ({ len = 0; _ }, x) -> Success x
    | Angstrom.Buffered.Done ({ buf; off; len }, x) ->
      Partial { partial = x; unparsed = Bigstringaf.substring buf ~off ~len }
    | Angstrom.Buffered.Fail ({ buf; off; len }, marks, s) ->
      let message = sprintf "%s (%s)" s (String.concat ~sep:", " marks) in
      Failed { message; unparsed = Bigstringaf.substring buf ~off ~len }
    | Angstrom.Buffered.Partial _feed -> Not_enough_input
  in
  let finalize state =
    let final =
      match state with
      | (Angstrom.Buffered.Done _ as acc)
       |(Angstrom.Buffered.Fail _ as acc) ->
        acc
      | Angstrom.Buffered.Partial feed -> feed `Eof
    in
    data := to_status final
  in
  object
    inherit Eio.Flow.sink

    method copy src =
      let buf = Cstruct.create Utils.Io.parser_buffer_size in
      let rec loop = function
        | (Angstrom.Buffered.Done _ as state)
         |(Angstrom.Buffered.Fail _ as state) ->
          finalize state
        | Angstrom.Buffered.Partial feed as state -> (
          try
            let got = src#read_into buf in
            loop (feed (`Bigstring (Bigstringaf.sub buf.buffer ~off:0 ~len:got)))
          with
          | End_of_file -> finalize state )
      in
      loop (Angstrom.Buffered.parse parser)

    method! write bufs =
      List.fold_until bufs ~finish:Fn.id ~init:(Angstrom.Buffered.parse parser)
        ~f:(fun acc { buffer; off; len } ->
        match acc with
        | Angstrom.Buffered.Done _
         |Angstrom.Buffered.Fail _ ->
          Stop acc
        | Angstrom.Buffered.Partial feed ->
          Continue (feed (`Bigstring (Bigstringaf.sub buffer ~off ~len))) )
      |> finalize

    method data = !data
  end

let to_cstructs flow =
  let q = Queue.create () in
  let rec loop () =
    let buf = Cstruct.create Utils.Io.parser_buffer_size in
    Queue.enqueue q buf;
    match Eio.Flow.read_exact flow buf with
    | () -> loop ()
    | exception End_of_file -> ()
  in
  loop ();
  Queue.to_list q

let exec_parser_eio ~on_ok ?on_error parser ~path ~language_name source =
  let sink = parser_sink parser in
  Eio.Flow.copy source sink;
  (* Eio.Flow.write sink (to_cstructs source); *)
  let get_handler () =
    match on_error with
    | None -> default_error_handler ~path ~language_name
    | Some x -> x
  in
  match sink#data with
  | Success x -> on_ok x
  | Incomplete -> (get_handler ()) ~msg:"Incomplete. Please report this bug." ()
  | Not_enough_input -> (get_handler ()) ~msg:"Not enough input." ()
  | Partial { unparsed; _ } -> (get_handler ()) ~unparsed ~msg:"" ()
  | Failed { message = msg; unparsed } -> (get_handler ()) ~unparsed ~msg ()
