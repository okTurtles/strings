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
    | Js         of string
    | Ts         of string
    | Html       of {
        parsed: Html.t;
        length: int option;
      }
    | Pug_native of {
        parsed: Pug.t;
        length: int option;
      }
    | Css        of int

  let of_source ~filename : Source.t -> t = function
  | Template (Template.HTML source) ->
    let parsed = Parsing.Basic.exec_parser Parsing.Html.parser ~filename ~language_name:"HTML" source in
    Html { parsed; length = Some (String.length source) }
  | Template (Template.PUG source) ->
    let parsed = Basic.exec_parser Pug.parser ~filename ~language_name:"Pug" source in
    Pug_native { parsed; length = Some (String.length source) }
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

let collect_from_possible_scripts Utils.Collector.{ possible_scripts; _ } template_script ~on_string =
  Queue.fold possible_scripts ~init:Lwt.return_unit ~f:(fun acc raw ->
      let* () = acc in
      match template_script with
      | JS ->
        Js.extract raw ~on_string;
        Lwt.return_unit
      | TS -> (
        Quickjs.extract Typescript raw >|= function
        | Error _ -> ()
        | Ok (strings, _) -> Array.iter strings ~f:on_string))

let collect_from_languages collector languages =
  Lwt_list.iter_p
    (function
      | Language.Html { parsed; length = _ } ->
        Html.collect collector parsed;
        Lwt.return_unit
      | Pug_native { parsed; length = _ } ->
        Pug.collect collector parsed;
        Lwt.return_unit
      | Js source ->
        Js.extract_to_collector collector source;
        Lwt.return_unit
      | Ts source -> Quickjs.extract_to_collector collector Typescript source
      | Css _ -> Lwt.return_unit)
    languages

let debug_template ~filename languages template_script target =
  let print_collector ~error_kind (Utils.Collector.{ strings; file_errors; _ } as collector) =
    let* () =
      collect_from_possible_scripts collector template_script ~on_string:(Queue.enqueue strings)
    in
    let buf = Buffer.create 256 in
    Queue.iter strings ~f:(fun s -> bprintf buf "%s\n" s);
    if not (Queue.is_empty file_errors)
    then (
      bprintf buf "\n%s errors in %s:\n" error_kind filename;
      Queue.iter file_errors ~f:(bprintf buf "- %s\n"));
    Lwt_io.printl (Buffer.contents buf)
  in
  Lwt_list.iter_s
    (fun lang ->
      match (lang : Language.t), (target : Debug.t) with
      | Js source, _ -> Lwt_io.printlf "<JS Code - %d bytes>" (String.length source)
      | Ts source, _ -> Lwt_io.printlf "<TS Code - %d bytes>" (String.length source)
      | Css length, _ -> Lwt_io.printlf "<CSS Code - %d bytes>" length
      | Html { parsed; length = _ }, Html ->
        let collector = Utils.Collector.create ~filename in
        let* () = Lwt_io.printlf !"%{sexp#hum: Html.t}" parsed in
        let* () = collect_from_languages collector [ lang ] in
        print_collector ~error_kind:"HTML" collector
      | (Pug_native { parsed; length = _ } as lang), Pug ->
        let* () = Lwt_io.printlf !"%{sexp#hum: Pug.t}" parsed in
        let collector = Utils.Collector.create ~filename in
        let* () = collect_from_languages collector [ lang ] in
        print_collector ~error_kind:"Pug" collector
      | Html { length = Some len; _ }, Pug -> Lwt_io.printlf "<HTML code - %d bytes>" len
      | Html { length = None; _ }, Pug -> Lwt_io.printl "<HTML code>"
      | Pug_native { length = Some len; _ }, Html -> Lwt_io.printlf "<Pug code - %d bytes>" len
      | Pug_native { length = None; _ }, Html -> Lwt_io.printl "<Pug code>")
    languages

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
