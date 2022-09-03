open Core
open Lwt.Syntax
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
        top: SZXX.Xml.DOM.element option;
        length: int;
      }
    | Pug  of {
        nodes: Pug.node array;
        length: int;
      }
    | Css  of int

  let of_source ~filename : Source.t -> t = function
  | Template (Template.HTML source) ->
    let open SZXX in
    let Xml.SAX.To_DOM.{ top; _ } =
      Angstrom.parse_string ~consume:All (Angstrom.many Xml.parser) source
      |> Result.map_error ~f:(sprintf "Parsing error in %s: %s" filename)
      |> Result.ok_or_failwith
      |> List.fold_result ~init:Xml.SAX.To_DOM.init ~f:(fun acc x -> Xml.SAX.To_DOM.folder (Ok acc) x)
      |> Result.map_error ~f:(sprintf "Syntax error in %s: %s" filename)
      |> Result.ok_or_failwith
    in
    Html { top; length = String.length source }
  | Template (Template.PUG source) ->
    let nodes = Basic.exec_parser Pug.parser ~filename ~language_name:"Pug" source in
    Pug { nodes; length = String.length source }
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

let rec collect_pug strings acc Pug.{ selector; arguments; text; children } =
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
  Array.fold children ~init:acc ~f:(collect_pug strings)

let rec collect_html strings acc (node : SZXX.Xml.DOM.element) =
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
  Array.fold node.children ~init:acc ~f:(collect_html strings)

let extract_ts strings source =
  let+ parsed = Quickjs.extract_ts source in
  Array.iter parsed ~f:(Queue.enqueue strings)

let extract_template strings possible_code = function
| JS ->
  List.iter possible_code ~f:(Js_ast.strings_from_template strings);
  Lwt.return_unit
| TS -> Lwt_list.iter_p (extract_ts strings) possible_code

let extract_strings ~filename js_file_errors languages template_script =
  let strings = Queue.create () in
  let+ () =
    Lwt_list.iter_p
      (function
        | Language.Html { top = None; length = _ } -> Lwt.return_unit
        | Html { top = Some node; length = _ } ->
          let possible_code = collect_html strings [] node in
          extract_template strings possible_code template_script
        | Pug { nodes; length = _ } ->
          let possible_code =
            Array.fold nodes ~init:[] ~f:(fun acc node -> collect_pug strings acc node)
          in
          extract_template strings possible_code template_script
        | Js source -> Js_ast.strings_from_js ~filename strings js_file_errors source
        | Ts source -> extract_ts strings source
        | Css _ -> Lwt.return_unit)
      languages
  in
  Queue.iter strings

let debug_template ~filename languages target template_script =
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
          let* () = Lwt_io.printlf !"%{sexp#hum: SZXX.Xml.DOM.element option}" top in
          let* iter = extract_strings ~filename js_file_errors [ lang ] template_script in
          print_iter iter
        | (Pug { nodes; length = _ } as lang), Pug ->
          let* () = Lwt_io.printlf !"%{sexp#hum: Pug.nodes}" nodes in
          let* iter = extract_strings ~filename js_file_errors [ lang ] template_script in
          print_iter iter
        | Html { length; _ }, Pug -> Lwt_io.printlf "<HTML code - %d bytes>" length
        | Pug { length; _ }, Html -> Lwt_io.printlf "<Pug code - %d bytes>" length)
      languages
  in
  Lwt_io.printl
    (Queue.to_array js_file_errors |> Array.map ~f:Failed.to_string |> String.concat_array ~sep:"\n")

let parse ~filename ic =
  let open Angstrom in
  let open Lwt.Syntax in
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
  let parser = mlws *> lift2 Tuple2.create (sep_by mlws languages) (mlws *> take_while (fun _ -> true)) in
  let+ _unconsumed, result = Angstrom_lwt_unix.parse parser ic in
  (match result with
  | Ok (parsed, "") -> parsed
  | Ok (_, unparsed) ->
    failwithf
      "The file [%s] contains invalid syntax or Vue features unsupported by this tool.\n\
       Please report this so it can be improved.\n\
       The unsupported syntax starts at:\n\
       %s"
      filename
      (Yojson.Basic.to_string (`String (String.slice unparsed 0 Int.(min 20 (String.length unparsed)))))
      ()
  | Error err -> failwithf "Syntax Error: %s" err ())
  |> List.map ~f:(Language.of_source ~filename)
