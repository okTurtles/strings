open Core
open Lwt.Syntax
open Parsing

type language =
  | Pug    of Pug.nodes
  | Script of Script.raw
  | Css
[@@deriving sexp, yojson]

type languages = language list [@@deriving sexp, yojson]

let rec loop_pug strings Pug.{ selector; arguments; text; children } =
  (match text, selector with
  | Some s, Element { parts = "i18n" :: _ } -> Queue.enqueue strings s
  | Some s, _ -> Js_ast.strings_from_pug strings s
  | None, _ -> ());
  List.iter arguments ~f:(fun { contents; _ } ->
      Option.iter contents ~f:(fun source -> Js_ast.strings_from_pug strings source));
  Array.iter children ~f:(loop_pug strings)

let extract_strings ~filename js_file_errors languages =
  let strings = Queue.create () in
  let+ () =
    Lwt_list.iter_p
      (function
        | Pug nodes ->
          Array.iter nodes ~f:(loop_pug strings);
          Lwt.return_unit
        | Script (Script.JS source) -> Js_ast.strings_from_js ~filename strings js_file_errors source
        | Script (Script.TS source) ->
          let+ parsed = Quickjs.extract_ts source in
          Array.iter parsed ~f:(Queue.enqueue strings)
        | Css -> Lwt.return_unit)
      languages
  in
  Queue.iter strings

let debug_pug ~filename languages =
  let js_file_errors = Queue.create () in
  let* () =
    Lwt_list.iter_s
      (function
        | Script (JS source) -> Lwt_io.printlf "<JS Code - %d bytes>" (String.length source)
        | Script (TS source) -> Lwt_io.printlf "<TS Code - %d bytes>" (String.length source)
        | Css -> Lwt_io.printl "<CSS Code>"
        | Pug nodes as lang ->
          let* () = Lwt_io.printl (Pug.sexp_of_nodes nodes |> Sexp.to_string_hum) in
          let* iter = extract_strings ~filename js_file_errors [ lang ] in
          let buf = Buffer.create 256 in
          iter ~f:(fun s ->
              Buffer.add_string buf s;
              Buffer.add_char buf '\n');
          Lwt_io.printl (Buffer.contents buf))
      languages
  in
  Lwt_io.printl
    (Queue.to_array js_file_errors |> Array.map ~f:Failed.to_string |> String.concat_array ~sep:"\n")

let parse ~filename ic ~f =
  let open Angstrom in
  let open Lwt.Syntax in
  let open Basic in
  let buf = Buffer.create 256 in
  let languages =
    choice
      [
        (Pug.parser >>| fun x -> Pug x);
        (Script.parser buf >>| fun x -> Script x);
        (Css.parser >>| fun () -> Css);
      ]
  in
  let parser = lift2 Tuple2.create (sep_by mlws1 languages) (mlws *> take_while (fun _ -> true)) in

  let* _unconsumed, result = Angstrom_lwt_unix.parse parser ic in
  match result with
  | Ok (parsed, "") -> f ~filename parsed
  | Ok (_, unparsed) ->
    failwithf
      "The file [%s] contains invalid syntax or Pug features unsupported by this tool.\n\
       Please report this so it can be improved.\n\
       The unsupported syntax starts at:\n\
       %s"
      filename
      (Yojson.Basic.to_string (`String (String.slice unparsed 0 Int.(min 20 (String.length unparsed)))))
      ()
  | Error err -> failwithf "Syntax Error: %s" err ()
