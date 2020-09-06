open Core_kernel
open Angstrom

let lowercase = function
| 'a'..'z' -> true
| _ -> false

let alphanum = function
| 'a'..'z' | 'A'..'Z' | '0'..'9' -> true
| _ -> false


let is_ws = function
| '\x20' | '\x0d' | '\x09' -> true
| _ -> false

let is_mlws = function
| '\x20' | '\x0d' | '\x09' | '\x0a' -> true
| _ -> false

let ws = (skip_while is_ws)
let ws1 = (satisfy is_ws *> ws)
let mlws = (skip_while is_mlws)
let mlws1 = (satisfy is_mlws *> mlws)
let take_remaining = take_while (Char.(<>) '\x0a')
let skip_remaining = skip_while (Char.(<>) '\x0a')
let maybe p = option None (p >>| Option.return)

let escapable_string_parser ~escape ~separator =
  let is_separator = Char.(=) separator in
  let is_escape = Char.(=) escape in
  let buf = Buffer.create 50 in
  char separator *> (
    let rec loop escaping =
      any_char >>= (fun x -> begin match x, escaping with
        | c, false when is_escape c ->
          loop true

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
        end
      )
    in
    loop false
  ) <?> "Escapable string"
