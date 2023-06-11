open! Core
open Eio.Std
open Parsing

module Source = struct
  type t =
    | Template of Template.raw
    | Script of Script.raw
    | Style of Style.raw
end

module Language = struct
  type t =
    | Js of string
    | Ts of string
    | Html of {
        parsed: Html.t;
        length: int option;
      }
    | Pug_native of {
        parsed: Pug.t;
        length: int option;
      }
    | Pug of {
        collector: Utils.Collector.t;
        length: int;
      }
    | Css of int
    | Failed of string

  let of_source ~path ~slow_pug : Source.t -> t = function
  | Template (Template.HTML source) ->
    let on_ok parsed = Html { parsed; length = Some (String.length source) } in
    let on_error ~msg = Failed msg in
    Basic.exec_parser ~on_ok ~on_error Html.parser ~path ~language_name:"HTML" source
  | Template (Template.PUG source) -> (
    let slow_parse () =
      let collector = Utils.Collector.create ~path in
      Quickjs.extract_to_collector collector Pug source;
      Pug { collector; length = String.length source }
    in
    match slow_pug with
    | true -> slow_parse ()
    | false ->
      let on_ok parsed = Pug_native { parsed; length = Some (String.length source) } in
      let on_error ~msg:_ = slow_parse () in
      Basic.exec_parser ~on_ok ~on_error
        (Pug.parser (Basic.make_string_parsers ()))
        ~path ~language_name:"Pug" source )
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
  Queue.iter possible_scripts ~f:(fun raw ->
    match template_script with
    | JS -> Js.extract raw ~on_string
    | TS -> (
      match Quickjs.extract Typescript raw with
      | Error _ -> ()
      | Ok (strings, _) -> Array.iter strings ~f:on_string ) )

let collect_from_languages collector languages =
  Fiber.List.iter
    (function
      | Language.Html { parsed; length = _ } -> Html.collect collector parsed
      | Pug_native { parsed; length = _ } -> Pug.collect collector parsed
      | Pug { collector = src; length = _ } -> Utils.Collector.blit_transfer ~src ~dst:collector
      | Js source -> Js.extract_to_collector collector source
      | Ts source -> Quickjs.extract_to_collector collector Typescript source
      | Css _ -> ()
      | Failed msg -> Queue.enqueue collector.file_errors msg)
    languages

let debug_template env ~path languages template_script target =
  let print_collector ~error_kind (Utils.Collector.{ strings; file_errors; _ } as collector) =
    collect_from_possible_scripts collector template_script ~on_string:(Queue.enqueue strings);
    let module W = Eio.Buf_write in
    W.with_flow env#stdout (fun w ->
      let deduped = Queue.fold strings ~init:String.Set.empty ~f:String.Set.add in
      Eio.Flow.copy_string (sprintf "Found %d strings:\n" (String.Set.length deduped)) env#stdout;
      String.Set.iter deduped ~f:(fun s -> W.string w (sprintf !"%{Yojson.Basic}\n" (`String s)));
      if not (Queue.is_empty file_errors)
      then (
        W.string w (sprintf "\n❌ %s errors in %s:\n" error_kind path);
        Queue.iter file_errors ~f:(fun s -> W.string w (sprintf "- %s\n" s)) );
      W.char w '\n' )
  in
  List.iter languages ~f:(fun lang ->
    match (lang : Language.t), (target : Debug.t) with
    | Js source, _ ->
      Eio.Flow.copy_string (sprintf "<JS Code - %d bytes>\n" (String.length source)) env#stdout
    | Ts source, _ ->
      Eio.Flow.copy_string (sprintf "<TS Code - %d bytes>\n" (String.length source)) env#stdout
    | Css length, _ -> Eio.Flow.copy_string (sprintf "<CSS Code - %d bytes>\n" length) env#stdout
    | Html { parsed; length = _ }, Html ->
      let collector = Utils.Collector.create ~path in
      Eio.Flow.copy_string (sprintf !"%{sexp#hum: Html.t}" parsed) env#stdout;
      collect_from_languages collector [ lang ];
      print_collector ~error_kind:"HTML" collector
    | (Pug_native { parsed; length = _ } as lang), Pug ->
      Eio.Flow.copy_string (sprintf !"%{sexp#hum: Pug.t}" parsed) env#stdout;
      let collector = Utils.Collector.create ~path in
      collect_from_languages collector [ lang ];
      print_collector ~error_kind:"Pug" collector
    | Pug { collector; length = _ }, Pug -> print_collector ~error_kind:"Pug" collector
    | Html { length = Some len; _ }, Pug ->
      Eio.Flow.copy_string (sprintf "<HTML code - %d bytes>" len) env#stdout
    | Html { length = None; _ }, Pug -> Eio.Flow.copy_string "<HTML code>" env#stdout
    | Pug_native { length = Some len; _ }, Html ->
      Eio.Flow.copy_string (sprintf "<Pug code - %d bytes>" len) env#stdout
    | Pug_native { length = None; _ }, Html -> Eio.Flow.copy_string "<Pug code>" env#stdout
    | Pug { length; _ }, Html -> Eio.Flow.copy_string (sprintf "<Pug code - %d bytes>" length) env#stdout
    | Failed msg, _ -> Eio.Flow.copy_string (sprintf "❌ Parsing error in path: %s" msg) env#stdout )

let parse ~path ~slow_pug flow =
  let buf = Buffer.create 256 in
  let parser =
    let open Angstrom in
    let open Basic in
    let languages =
      choice
        [
          (Template.parser buf >>| fun x -> Source.Template x);
          (Script.parser buf >>| fun x -> Source.Script x);
          (Style.parser buf >>| fun x -> Source.Style x);
        ]
    in
    mlws *> sep_by mlws languages <* mlws
  in
  let on_ok ll = Fiber.List.map (Language.of_source ~path ~slow_pug) ll in
  Basic.exec_parser_eio ~on_ok parser ~path ~language_name:"Vue" flow
