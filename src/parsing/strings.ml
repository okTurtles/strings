open Core_kernel

type line =
| Translation of (string * string)
| Comment

let parse ic =
  let open Angstrom in
  let open Basic in
  let double_quoted_string = char '"' *> (escapable_string_parser ~escape:'\\' ~separator:'"') <* char '"' in

  let line = lift2 double
      (mlws *> double_quoted_string <* mlws <* char '=')
      (mlws *> double_quoted_string <* mlws <* char ';' <* mlws)
    >>| (fun pair -> Translation pair)
  in

  let comment = mlws *> string "/*" *> (
      let rec loop () =
        any_char >>= begin function
        | '*' ->
          any_char >>= begin function
          | '/' -> return Comment
          | _ -> loop ()
          end
        | _ -> loop ()
        end
      in
      loop ()
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
