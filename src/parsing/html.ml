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
      | _, source -> Queue.enqueue possible_scripts (SZXX.Xml.DOM.unescape source) );
    List.iter node.children ~f:loop
  in
  loop top

let finalize ll =
  let open Xml.SAX.Expert.To_DOM in
  List.fold ll ~init ~f:(fun acc x -> folder ~strict:false acc x) |> function
  | { top = None; _ } -> Error "No root HTML element"
  | { top = Some top; _ } -> Ok top

let parser =
  let open Angstrom in
  many Xml.html_parser >>= fun ll ->
  match finalize ll with
  | Ok x -> return x
  | Error msg -> fail msg
