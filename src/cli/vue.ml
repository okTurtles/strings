open Core_kernel

open Parsing

type language =
| Pug of Pug.nodes
| Js of Js.raw
| Css
[@@deriving sexp, yojson]

type languages = language list [@@deriving sexp, yojson]

let rec loop_pug queue Pug.{ selector; arguments; text; children } =
  begin match text, selector with
  | Some s, (Element { parts = "i18n"::_ }) -> Queue.enqueue queue s
  | Some s, _ ->
    begin try Js_ast.strings queue s
    with _exn -> () end
  | None, _ -> ()
  end;
  List.iter arguments ~f:(fun { contents; _ } -> Option.iter contents ~f:(fun source ->
      try Js_ast.strings queue source
      with _exn -> ()
    ));
  Array.iter children ~f:(loop_pug queue)

let extract_strings ~filename parsed =
  let strings = Queue.create () in
  List.iter parsed ~f:(function
  | Pug nodes -> Array.iter nodes ~f:(loop_pug strings)
  | Js source ->
    begin try Js_ast.strings strings source
    with _exn -> failwithf "JS Syntax Error in %s" filename () end
  | Css -> ()
  );
  Lwt.return strings

let debug_pug ~filename parsed =
  Lwt_list.iter_s (function
  | Js _
  | Css -> Lwt.return_unit
  | (Pug nodes as lang) ->
    let%lwt () = Lwt_io.printl (Pug.sexp_of_nodes nodes |> Sexp.to_string_hum) in
    let%lwt strings = extract_strings ~filename [lang] in
    Lwt_io.printl (Queue.to_array strings |> String.concat_array ~sep:"\n")
  ) parsed

let parse ~filename ic ~f =
  let open Angstrom in
  let open Basic in

  let languages = choice [
      Pug.parser >>| (fun x -> Pug x);
      Js.parser () >>| (fun x -> Js x);
      Css.parser >>| (fun () -> Css);
    ]
  in
  let parser = lift2 Tuple2.create (sep_by mlws1 languages) (mlws *> take_while (fun _ -> true)) in

  let%lwt _unconsumed, result = Angstrom_lwt_unix.parse parser ic in
  begin match result with
  | Ok (parsed, "") -> f ~filename parsed
  | Ok (_, unparsed) ->
    failwithf "The file [%s] contains invalid syntax or Pug features unsupported by this tool.\nPlease report this so it can be improved.\nThe unsupported syntax starts at:\n%s"
      filename (Yojson.Basic.to_string (`String (String.slice unparsed 0 Int.(min 20 (String.length unparsed))))) ()
  | Error err -> failwithf "Syntax Error: %s" err ()
  end
