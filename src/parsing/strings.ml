open Core_kernel

type line =
| Translation of (string * string)
| Comment

let parse ~filename ic =
  let open Angstrom in
  let open Basic in
  let double_quoted_string = escapable_string_parser ~escape:'\\' ~separator:'"' in

  let line = lift2 (fun x y -> Translation (x, y))
      (mlws *> double_quoted_string <* mlws <* char '=')
      (mlws *> double_quoted_string <* mlws <* char ';' <* mlws)
  in

  let comment = mlws *> string "/*" *> (
      let rec loop n =
        any_char >>= begin function
        | '*' when Int.(n = 0) -> loop 1
        | '/' when Int.(n = 1) -> return Comment
        | _ -> loop 0
        end
      in
      loop 0
    ) <* mlws
  in

  let parser = lift2 Tuple2.create (many (line <|> comment)) (mlws *> take_while (fun _ -> true)) in

  let table = String.Table.create () in

  let%lwt _unconsumed, result = Angstrom_lwt_unix.parse parser ic in
  begin match result with
  | Ok (lines, "") ->
    List.iter lines ~f:(function
    | Translation (x, y) -> String.Table.set table ~key:x ~data:y
    | Comment -> ()
    );
    Lwt.return table
  | Ok (_, unparsed) ->
    failwithf "There is a syntax error in file [%s].\nTranslations must follow this format and end in a semicolon: \"english text\" = \"translated text\";\nThis line is malformed:\n%s" filename
      (String.take_while ~f:(Char.(<>) '\n') unparsed) ()
  | Error err -> failwithf "Syntax Error: %s" err ()
  end
