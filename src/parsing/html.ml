open! Core
open SZXX

let finalize ll =
  List.fold_result ll ~init:Xml.SAX.To_DOM.init ~f:(fun acc x ->
      Xml.SAX.To_DOM.folder ~strict:false (Ok acc) x)
  |> function
  | Error _ as err -> err
  | Ok Xml.SAX.To_DOM.{ top = None; _ } -> Error "No root HTML element"
  | Ok Xml.SAX.To_DOM.{ top = Some x; _ } -> Ok x

let parser =
  let open Angstrom in
  many Xml.(make_parser { accept_html_boolean_attributes = true; accept_unquoted_attributes = true })
  >>= fun ll ->
  match finalize ll with
  | Ok x -> return x
  | Error msg -> fail msg
