open! Core
open Utils

let errors_to_string errors =
  let buf = Buffer.create 128 in
  List.iter errors ~f:(function
    | Loc.{ source = _; start = { line = sl; column = sc }; _end = { line = el; column = ec } }, err
      when sl = el ->
      bprintf buf "Line %d (%d-%d): %s\n" sl sc ec (Parse_error.PP.error err)
    | Loc.{ source = _; start = { line = sl; column = sc }; _end = { line = el; column = ec } }, err ->
      bprintf buf "Line %d (%d) to line %d (%d): %s\n" sl sc el ec (Parse_error.PP.error err));
  Buffer.contents buf

let parse_error ({ file_errors; _ } : Collector.t) error =
  let message =
    match error with
    | First (_ :: _ :: _ as ll) -> errors_to_string ll
    | First ll -> errors_to_string ll
    | Second msg -> msg
  in
  Queue.enqueue file_errors message

let debug statements =
  sprintf "Statements: %s"
    (List.map statements ~f:(fun stmt ->
         Format.asprintf "%a" (Flow_ast.Statement.pp (fun _ _ -> ()) (fun _ _ -> ())) stmt)
    |> String.concat ~sep:", ")
  |> print_endline

let extract source ~on_string =
  match Parser_flow.program source with
  | _, _ :: _ -> ()
  | ast, [] -> (
    match ast with
    | _, Flow_ast.Program.{ statements; comments = _; all_comments = _ } ->
      (* debug statements; *)
      Js_ast.extract ~on_string statements)
  | exception _ -> ()

let parse_options = Some { Parser_env.default_parse_options with esproposal_export_star_as = true }

let extract_to_collector (collector : Utils.Collector.t) source =
  match Parser_flow.program ~parse_options source with
  | _, (_ :: _ as errors) -> failwith (errors_to_string errors)
  | ast, [] -> (
    match ast with
    | _, Flow_ast.Program.{ statements; comments = _; all_comments = _ } ->
      (* debug statements; *)
      Js_ast.extract ~on_string:(Queue.enqueue collector.strings) statements)
  | exception Parse_error.Error (_, (_ :: _ as errors)) -> parse_error collector (First errors)
  | exception Parse_error.Error (_, []) -> parse_error collector (Second "Syntax error")
  | exception exn ->
    print_endline (sprintf "Unexpected error in %s\nPlease report this bug." collector.path);
    raise exn
