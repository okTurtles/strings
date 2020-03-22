open Core_kernel

open Parsing

type language =
| Pug of Pug.nodes
| Js of Js.raw
| Css of Css.raw
[@@deriving sexp, yojson]

type languages = language list [@@deriving sexp, yojson]

let rec loop_pug queue Pug.{ parts; arguments; text; children } =
  begin match text, parts with
  | (Some s), [Element { name = "i18n" }] -> Queue.enqueue queue s
  | Some _, _
  | None, _ -> ()
  end;
  List.iter arguments ~f:(fun { contents; _ } -> Option.iter contents ~f:(Js_ast.strings queue));
  List.iter children ~f:(loop_pug queue)

let parse input_channel =
  let open Angstrom in
  let open Basic in

  let languages = choice [
      Pug.parser >>| (fun x -> Pug x);
      Js.parser >>| (fun x -> Js x);
      Css.parser >>| (fun x -> Css x);
    ]
  in
  let parser = lift2 double (sep_by mlws1 languages) (mlws *> take_while (fun _ -> true)) in

  let%lwt _unconsumed, result = Angstrom_lwt_unix.parse parser input_channel in
  begin match result with
  (* | Ok (parsed, "") -> Lwt.return (languages_to_yojson parsed |> Yojson.Safe.pretty_to_string) *)
  | Ok (parsed, "") ->
    let strings = Queue.create () in
    List.iter parsed ~f:(function
    | Pug nodes ->
      List.iter nodes ~f:(loop_pug strings)

    | Js strs -> Array.iter strs ~f:(fun str -> Queue.enqueue strings str)
    | Css _ -> ()
    );
    Lwt.return (Queue.to_array strings)

  | Ok (_, unparsed) ->
    failwithf "Could not process data starting at:\n%s"
      (Yojson.Basic.to_string (`String (String.slice unparsed 0 Int.(min 20 (String.length unparsed))))) ()
  | Error err -> failwithf "Syntax Error: %s" err ()
  end
