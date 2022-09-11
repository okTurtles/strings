open! Core
open Lwt.Syntax
open Lwt.Infix
open Parsing

module Source = struct
  type t =
    | Template of Template.raw
    | Script   of Script.raw
    | Style    of Style.raw
end

module Language = struct
  type t =
    | Js   of string
    | Ts   of string
    | Html of {
        top: SZXX.Xml.DOM.element;
        length: int option;
      }
    | Pug  of {
        nodes: Pug.node array;
        length: int option;
      }
    | Css  of int

  let of_source ~filename : Source.t -> t = function
  | Template (Template.HTML source) ->
    let top = Parsing.Basic.exec_parser Parsing.Html.parser ~filename ~language_name:"HTML" source in
    Html { top; length = Some (String.length source) }
  | Template (Template.PUG source) ->
    let nodes = Basic.exec_parser Pug.parser ~filename ~language_name:"Pug" source in
    Pug { nodes; length = Some (String.length source) }
  | Script (Script.JS s) -> Js s
  | Script (Script.TS s) -> Ts s
  | Style (Style.CSS s) -> Css (String.length s)
end

type template_script =
  | JS
  | TS

module Debug = struct
  type t =
    | Pug
    | Html
end

let extract_ts strings source =
  let+ parsed = Quickjs.extract source Typescript in
  Array.iter parsed ~f:(Queue.enqueue strings)

let extract_template strings template_script possible_code =
  match template_script with
  | JS ->
    List.iter possible_code ~f:(Js_ast.strings_from_template strings);
    Lwt.return_unit
  | TS -> Lwt_list.iter_p (extract_ts strings) possible_code

let collect_pug strings template_script nodes =
  let rec loop acc Pug.{ selector; arguments; text; children } =
    let acc =
      match text, selector with
      | Some s, Element { parts = "i18n" :: _ } ->
        Queue.enqueue strings s;
        acc
      | Some s, _ -> s :: acc
      | None, _ -> acc
    in
    let acc =
      List.fold arguments ~init:acc ~f:(fun acc -> function
        | Pug.{ contents = None; _ } -> acc
        | Pug.{ contents = Some s; _ } -> s :: acc)
    in
    Array.fold children ~init:acc ~f:loop
  in
  Array.fold nodes ~init:[] ~f:loop |> extract_template strings template_script

let collect_html strings template_script top =
  let rec loop acc (node : SZXX.Xml.DOM.element) =
    let acc =
      match node with
      | { text = ""; _ } -> acc
      | { tag = "i18n"; text; _ } ->
        Queue.enqueue strings text;
        acc
      | { text; _ } -> text :: acc
    in
    let acc =
      List.fold node.attrs ~init:acc ~f:(fun acc -> function
        | "class", _
         |"id", _
         |_, "" ->
          acc
        | _, source -> SZXX.Xml.unescape source :: acc)
    in
    Array.fold node.children ~init:acc ~f:loop
  in
  loop [] top |> extract_template strings template_script

let extract_strings ~filename js_file_errors template_script languages =
  let strings = Queue.create () in
  let+ () =
    Lwt_list.iter_p
      (function
        | Language.Html { top; length = _ } -> collect_html strings template_script top
        | Pug { nodes; length = _ } -> collect_pug strings template_script nodes
        | Js source -> Js_ast.strings_from_js ~filename strings js_file_errors source
        | Ts source -> extract_ts strings source
        | Css _ -> Lwt.return_unit)
      languages
  in
  Queue.iter strings

let debug_template ~filename languages template_script target =
  let print_iter iter =
    let buf = Buffer.create 256 in
    iter ~f:(fun s ->
        Buffer.add_string buf s;
        Buffer.add_char buf '\n');
    Lwt_io.printl (Buffer.contents buf)
  in
  let js_file_errors = Queue.create () in
  let* () =
    Lwt_list.iter_s
      (fun lang ->
        match (lang : Language.t), (target : Debug.t) with
        | Js source, _ -> Lwt_io.printlf "<JS Code - %d bytes>" (String.length source)
        | Ts source, _ -> Lwt_io.printlf "<TS Code - %d bytes>" (String.length source)
        | Css length, _ -> Lwt_io.printlf "<CSS Code - %d bytes>" length
        | Html { top; length = _ }, Html ->
          let* () = Lwt_io.printlf !"%{sexp#hum: SZXX.Xml.DOM.element}" top in
          let* iter = extract_strings ~filename js_file_errors template_script [ lang ] in
          print_iter iter
        | (Pug { nodes; length = _ } as lang), Pug ->
          let* () = Lwt_io.printlf !"%{sexp#hum: Pug.t}" nodes in
          let* iter = extract_strings ~filename js_file_errors template_script [ lang ] in
          print_iter iter
        | Html { length = Some len; _ }, Pug -> Lwt_io.printlf "<HTML code - %d bytes>" len
        | Html { length = None; _ }, Pug -> Lwt_io.printl "<HTML code>"
        | Pug { length = Some len; _ }, Html -> Lwt_io.printlf "<Pug code - %d bytes>" len
        | Pug { length = None; _ }, Html -> Lwt_io.printl "<Pug code>")
      languages
  in
  Lwt_io.printl
    (Queue.to_array js_file_errors |> Array.map ~f:Utils.Failed.to_string |> String.concat_array ~sep:"\n")

let parse ~filename ic =
  let open Angstrom in
  let open Basic in
  let buf = Buffer.create 256 in
  let languages =
    choice
      [
        (Template.parser buf >>| fun x -> Source.Template x);
        (Script.parser buf >>| fun x -> Source.Script x);
        (Style.parser buf >>| fun x -> Source.Style x);
      ]
  in
  let parser = mlws *> sep_by mlws languages <* mlws in
  Basic.exec_parser_lwt parser ~filename ~language_name:"Vue" ic
  >|= List.map ~f:(Language.of_source ~filename)
