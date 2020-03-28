open Core_kernel

type line =
| Translation of (string * string)
| Comment

let parse ic =
  let open Angstrom in
  let open Basic in
  let double_quoted_string = escapable_string_parser ~escape:'\\' ~separator:'"' in

  let line = lift2 double
      (mlws *> double_quoted_string <* mlws <* char '=')
      (mlws *> double_quoted_string <* mlws <* char ';' <* mlws)
    >>| (fun pair -> Translation pair)
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

  let table = String.Table.create () in

  let%lwt _unconsumed, result = Angstrom_lwt_unix.parse_many
      (line <|> comment)
      (function
      | Translation (x, y) ->
        (String.Table.update table x ~f:(fun _ -> y));
        Lwt.return_unit
      | Comment -> Lwt.return_unit
      )
      ic
  in
  begin match result with
  | Ok () -> Lwt.return table
  | Error err -> failwithf "Syntax Error: %s" err ()
  end
