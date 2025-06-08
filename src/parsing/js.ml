open! Core

let errors_to_string errors =
  let buf = Buffer.create 128 in
  List.iter errors ~f:(function
    | Loc.{ source = _; start = { line = sl; column = sc }; _end = { line = el; column = ec } }, err
      when sl = el ->
      bprintf buf "Line %d (%d-%d): %s\n" sl sc ec (Parse_error.PP.error err)
    | Loc.{ source = _; start = { line = sl; column = sc }; _end = { line = el; column = ec } }, err ->
      bprintf buf "Line %d (%d) to line %d (%d): %s\n" sl sc el ec (Parse_error.PP.error err) );
  Buffer.contents buf

let parse_error = function
| First (_ :: _ :: _ as ll) -> errors_to_string ll
| First ll -> errors_to_string ll
| Second msg -> msg

let debug statements =
  let pp fmt ll =
    List.iteri ll ~f:(fun i stmt ->
      if i > 0 then Format.pp_print_string fmt ", ";
      Format.fprintf fmt "%a" (Flow_ast.Statement.pp (fun _ _ -> ()) (fun _ _ -> ())) stmt )
  in
  Format.printf "Statements: %a" pp statements

let parse_options = Some { Parser_env.default_parse_options with esproposal_export_star_as = true }

let parse ~path ~source =
  match Parser_flow.program ~parse_options source with
  | _, (_ :: _ as errors) -> Error (lazy (errors_to_string errors))
  | ast, [] -> (
    match ast with
    | _, Flow_ast.Program.{ statements; comments = _; all_comments = _ } -> Ok statements )
  | exception Parse_error.Error (_, (_ :: _ as errors)) -> Error (lazy (parse_error (First errors)))
  | exception Parse_error.Error (_, []) -> Error (lazy (parse_error (Second "Syntax error")))
  | exception exn ->
    Error
      ( lazy
        (sprintf "Unexpected error in %s: %s\nPlease report this bug." path (Utils.Exception.human exn))
        )

let unescape source =
  match parse ~path:"attribute" ~source with
  | Ok stmts -> (
    match Js_ast.unescape stmts with
    | Some s -> s
    | None -> source )
  | Error _ -> source

let extract ~on_string ~function_name ~source =
  match parse ~path:"attribute" ~source with
  | Ok stmts -> Js_ast.extract ~on_string ~function_name stmts
  | Error _ -> ()

let extract_to_collector
  ({ path; needle_names = function_name, _; strings; file_errors; _ } : Utils.Collector.t) ~source =
  match parse ~path ~source with
  | Ok stmts -> Js_ast.extract ~on_string:(Queue.enqueue strings) ~function_name stmts
  | Error (lazy msg) -> Queue.enqueue file_errors msg
