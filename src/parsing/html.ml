open! Core
open SZXX

type t = Xml.DOM.element [@@deriving sexp_of]

let collect Utils.Collector.{ strings; possible_scripts; _ } top =
  let rec loop (node : Xml.DOM.element) =
    (match node with
    | { text = ""; _ } -> ()
    | { tag = "i18n"; text; _ } -> Queue.enqueue strings text
    | { text; _ } -> Queue.enqueue possible_scripts text);
    List.iter node.attrs ~f:(function
      | "class", _
       |"id", _
       |_, "" ->
        ()
      | _, source -> Queue.enqueue possible_scripts (SZXX.Xml.unescape source));
    Array.iter node.children ~f:loop
  in
  loop top

let finalize ll =
  List.fold_result ll ~init:Xml.SAX.To_DOM.init ~f:(fun acc x ->
      Xml.SAX.To_DOM.folder ~strict:false (Ok acc) x)
  |> function
  | Error _ as err -> err
  | Ok Xml.SAX.To_DOM.{ top = None; _ } -> Error "No root HTML element"
  | Ok Xml.SAX.To_DOM.{ top = Some top; _ } -> Ok top

let parser =
  let open Angstrom in
  many Xml.(make_parser { accept_html_boolean_attributes = true; accept_unquoted_attributes = true })
  >>= fun ll ->
  match finalize ll with
  | Ok x -> return x
  | Error msg -> fail msg
