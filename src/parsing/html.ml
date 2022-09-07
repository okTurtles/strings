open! Core
open SZXX

let parser =
  Angstrom.many
    Xml.(make_parser { accept_html_boolean_attributes = true; accept_unquoted_attributes = true })

let finalize ~filename ll =
  List.fold_result ll ~init:Xml.SAX.To_DOM.init ~f:(fun acc x ->
      Xml.SAX.To_DOM.folder ~strict:false (Ok acc) x)
  |> function
  | Error err -> failwithf "Syntax error in %s: %s" filename err ()
  | Ok Xml.SAX.To_DOM.{ top; _ } -> top
