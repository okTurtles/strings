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
let double x y = x, y
let triple x y z = x, y, z
let _quad w x y z = w, x, y, z

let escapable_string_parser ~escape ~separator =
  let is_separator = Char.(=) separator in
  let is_escape = Char.(=) escape in
  let escaping = ref false in
  let buffer = Buffer.create 50 in
  skip_while (fun c ->
    begin match c, !escaping with
    | c, false when is_escape c ->
      escaping := true;
      true

    | c, false when is_separator c ->
      false

    | c, true when is_separator c ->
      escaping := false;
      Buffer.add_char buffer c;
      true

    | c, true ->
      escaping := false;
      Buffer.add_char buffer escape;
      Buffer.add_char buffer c;
      true

    | c, false ->
      Buffer.add_char buffer c;
      true
    end
  )
  >>= fun () ->
  let result = Buffer.contents buffer in
  Buffer.clear buffer;
  return result
  <?> "Escapable string"
