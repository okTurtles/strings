open! Core
open Lwt.Infix
open Lwt.Syntax

let version = "2.2.0"

let header = sprintf "/* Generated by okTurtles/strings v%s */\n\n" version

let () =
  Lwt.async_exception_hook :=
    fun ex ->
      let p1 =
        Lwt_io.write_line Lwt_io.stderr
          (sprintf "💀 PLEASE REPORT THIS BUG. UNCAUGHT EXCEPTION: %s" (Utils.Exception.human ex))
      in
      let p2 = Lwt_unix.sleep 3. in
      Lwt.join [ p1; p2 ] >>= (fun () -> exit 2) |> ignore

let plural i = if i = 1 then "" else "s"

let pool = Lwt_pool.create 6 (fun () -> Lwt.return_unit)

type counts = {
  vue: int ref;
  pug: int ref;
  html: int ref;
  js: int ref;
  ts: int ref;
}

type common_options = {
  outdir: string;
  targets: string list;
  template_script: Vue.template_script;
  slow_pug: bool;
}

type action =
  | Debug of Vue.Debug.t
  | Run

type traversal = {
  rootlen: int;
  table: String.Set.t String.Table.t;
  template_script: Vue.template_script;
  slow_pug: bool;
  counts: counts;
}

let process_file traversal count filename ~f:get_collector =
  Lwt_pool.use pool (fun () ->
      incr count;
      let* (collector : Utils.Collector.t) =
        Lwt_io.with_file ~mode:Input ~flags:Utils.Io.read_flags filename get_collector
      in
      let handler string =
        let realname = String.slice filename traversal.rootlen 0 in
        String.Table.update traversal.table string ~f:(function
          | None -> String.Set.add String.Set.empty realname
          | Some set -> String.Set.add set realname)
      in
      let* () =
        Utils.Collector.render_errors collector
        |> Option.value_map ~default:Lwt.return_unit ~f:Lwt_io.eprintl
      in
      Queue.iter collector.strings ~f:handler;
      Vue.collect_from_possible_scripts collector traversal.template_script ~on_string:handler)

let rec process_dir traversal ~path = function
| "node_modules" -> Lwt.return_unit
| filename when String.is_prefix ~prefix:"." filename -> Lwt.return_unit
| filename -> (
  Lwt_unix.lstat path >>= fun stat ->
  match stat, lazy (String.slice filename (-4) 0), lazy (String.slice filename (-3) 0) with
  | { st_kind = S_REG; _ }, _, (lazy ".js") when String.is_suffix ~suffix:".js" filename ->
    process_file traversal traversal.counts.js path ~f:(fun ic ->
        let collector = Utils.Collector.create ~path in
        let+ source = Lwt_io.read ic in
        Parsing.Js.extract_to_collector collector source;
        collector)
  | { st_kind = S_REG; _ }, _, (lazy ".ts") ->
    process_file traversal traversal.counts.ts path ~f:(fun ic ->
        let collector = Utils.Collector.create ~path in
        let* source = Lwt_io.read ic in
        let+ () = Quickjs.extract_to_collector collector Typescript source in
        collector)
  | { st_kind = S_REG; _ }, (lazy ".vue"), _ ->
    process_file traversal traversal.counts.vue path ~f:(fun ic ->
        let collector = Utils.Collector.create ~path in
        let* languages = Vue.parse ~path ~slow_pug:traversal.slow_pug ic in
        let+ () = Vue.collect_from_languages collector languages in
        collector)
  | { st_kind = S_REG; _ }, (lazy ".pug"), _ ->
    process_file traversal traversal.counts.pug path ~f:(fun ic ->
        let collector = Utils.Collector.create ~path in
        let* source = Lwt_io.read ic in
        let slow_parse () = Quickjs.extract_to_collector collector Pug source in
        let+ () =
          match traversal.slow_pug with
          | true -> slow_parse ()
          | false ->
            let on_ok parsed =
              Parsing.Pug.collect collector parsed;
              Lwt.return_unit
            in
            let on_error ~msg:_ = slow_parse () in
            Parsing.Basic.exec_parser ~on_ok ~on_error Parsing.Pug.parser ~path ~language_name:"Pug"
              source
        in
        collector)
  | { st_kind = S_REG; _ }, _, _ when String.is_suffix filename ~suffix:".html" ->
    process_file traversal traversal.counts.html path ~f:(fun ic ->
        let collector = Utils.Collector.create ~path in
        let on_ok parsed =
          Parsing.Html.collect collector parsed;
          Lwt.return collector
        in
        Parsing.Basic.exec_parser_lwt ~on_ok Parsing.Html.parser ~path ~language_name:"HTML" ic)
  | { st_kind = S_DIR; _ }, _, _ -> traverse traversal path
  | _ -> Lwt.return_unit)

and traverse traversal directory =
  Lwt_pool.use pool (fun () -> Lwt_unix.files_of_directory directory |> Lwt_stream.to_list)
  >>= Lwt_list.iter_p (fun entry ->
          let path = Filename.concat directory entry in
          process_dir traversal ~path entry)

let fmt s = Yojson.Basic.to_string (`String s)

let json_pair left right first =
  sprintf "%s\n  %s: %s"
    (if !first
    then begin
      first := false;
      ""
    end
    else ",")
    left right

let write_english ~outdir english =
  let time = Utils.Timing.start () in
  let path_strings = Filename.concat outdir "english.strings" in
  let path_json = Filename.concat outdir "english.json" in
  let first = ref true in
  let* () =
    Lwt_io.with_file ~flags:Utils.Io.write_flags ~mode:Output path_strings (fun oc_strings ->
        Lwt_io.with_file ~flags:Utils.Io.write_flags ~mode:Output path_json (fun oc_json ->
            let* () = Lwt.join [ Lwt_io.write oc_strings header; Lwt_io.write_char oc_json '{' ] in
            let* () =
              (* Switch to a map to preserve order as much as possible and therefore reduce merge conflicts *)
              let map =
                String.Table.fold english ~init:String.Map.empty ~f:(fun ~key ~data acc ->
                    String.Map.set acc ~key ~data)
              in
              String.Map.fold map ~init:Lwt.return_unit ~f:(fun ~key ~data acc ->
                  let fmt_key = fmt key in
                  let output_strings = sprintf "/* %s */\n%s = %s;\n\n" data fmt_key fmt_key in
                  let output_json = json_pair fmt_key fmt_key first in
                  let* () = acc in
                  Lwt_io.write oc_strings output_strings <&> Lwt_io.write oc_json output_json)
            in
            Lwt_io.write oc_json "\n}\n"))
  in
  Lwt_io.printlf
    !"✅ [%{Int63}ms] Generated '%s' and '%s' with:\n- %d unique strings\n"
    (time `Stop) path_strings path_json (String.Table.length english)

let write_other ~outdir ~language english other =
  let time = Utils.Timing.start () in
  let path_strings = Filename.concat outdir (sprintf "%s.strings" language) in
  let path_json = Filename.concat outdir (sprintf "%s.json" language) in
  let* n_left, n_right, n_both =
    Lwt_io.with_file ~flags:Utils.Io.write_flags ~mode:Output path_strings (fun oc_strings ->
        Lwt_io.with_file ~flags:Utils.Io.write_flags ~mode:Output path_json (fun oc_json ->
            let english_only = ref String.Map.empty in
            let other_only = ref String.Map.empty in
            let both = ref String.Map.empty in
            let add_entry map_ref ~line_strings ~line_json =
              map_ref := String.Map.set !map_ref ~key:line_strings ~data:line_json;
              None
            in
            let missing_translation key x =
              let fmt_key = fmt key in
              let line_strings =
                sprintf "/* MISSING TRANSLATION - %s */\n%s = %s;\n\n" x fmt_key fmt_key
              in
              add_entry english_only ~line_strings ~line_json:(fmt_key, fmt_key)
            in
            let _table =
              String.Table.merge english other ~f:(fun ~key -> function
                | `Left x -> missing_translation key x
                | `Both (x, y) when String.(key = y) -> missing_translation key x
                | `Both (x, y) ->
                  let fmt_key = fmt key in
                  let fmt_y = fmt y in
                  let line_strings = sprintf "/* %s */\n%s = %s;\n\n" x fmt_key fmt_y in
                  add_entry both ~line_strings ~line_json:(fmt_key, fmt_y)
                | `Right y when String.(key = y) -> None
                | `Right y ->
                  (* No need to write "deprecated translations" to JSON *)
                  let line_strings = sprintf "/* Not currently used */\n%s = %s;\n\n" (fmt key) (fmt y) in
                  add_entry other_only ~line_strings ~line_json:())
            in
            let first = ref true in
            let* () = Lwt.join [ Lwt_io.write oc_strings header; Lwt_io.write_char oc_json '{' ] in
            let write_pairs map =
              String.Map.fold map ~init:(Lwt.return 0) ~f:(fun ~key:line_strings ~data:(x, y) acc ->
                  let* () =
                    Lwt_io.write oc_strings line_strings <&> Lwt_io.write oc_json (json_pair x y first)
                  in
                  acc >|= succ)
            in
            let* n_left = write_pairs !english_only in
            let* n_both = write_pairs !both in
            let* n_right =
              String.Map.fold !other_only ~init:(Lwt.return 0) ~f:(fun ~key:line_strings ~data:() acc ->
                  let* () = Lwt_io.write oc_strings line_strings in
                  acc >|= succ)
            in
            let+ () = Lwt_io.write oc_json "\n}\n" in
            n_left, n_right, n_both))
  in
  Lwt_io.printlf
    !"✅ [%{Int63}ms] Generated '%s' and '%s' with:\n\
      - %d new strings\n\
      - %d existing strings\n\
      - %d unused strings\n"
    (time `Stop) path_strings path_json n_left n_both n_right

let main options = function
| Debug lang ->
  Lwt_list.iter_s
    (fun path ->
      let* () = Lwt_io.printlf "\n>>> Debugging [%s]" path in
      let ({ slow_pug; template_script; _ } : common_options) = options in
      Lwt_io.with_file ~flags:Utils.Io.read_flags ~mode:Input path (fun ic ->
          match lang, String.slice path (-4) 0 with
          | _, ".vue" ->
            let* languages = Vue.parse ~path ~slow_pug ic in
            Vue.debug_template ~path languages template_script lang
          | Pug, ".pug" -> (
            let* source = Lwt_io.read ic in
            let slow_parse () =
              let collector = Utils.Collector.create ~path in
              let* () = Quickjs.extract_to_collector collector Pug source in
              Vue.debug_template ~path
                [ Pug { collector; length = String.length source } ]
                template_script lang
            in
            match slow_pug with
            | true -> slow_parse ()
            | false ->
              let on_ok parsed =
                Vue.debug_template ~path [ Pug_native { parsed; length = None } ] template_script lang
              in
              let on_error ~msg:_ = slow_parse () in
              Parsing.Basic.exec_parser ~on_ok ~on_error Parsing.Pug.parser ~path ~language_name:"Pug"
                source)
          | Html, _ when String.is_suffix path ~suffix:".html" ->
            let on_ok parsed =
              Vue.debug_template ~path [ Html { parsed; length = None } ] template_script lang
            in
            Parsing.Basic.exec_parser_lwt ~on_ok Parsing.Html.parser ~path ~language_name:"Pug" ic
          | _ -> Lwt_io.printlf "Nothing to do for file [%s]" path))
    options.targets
| Run ->
  let overall_time = Utils.Timing.start () in
  let outdir = options.outdir in
  (* Check current directory *)
  let* strings_dir_files =
    let git_dir_p = Utils.Io.directory_exists ".git" in
    let strings_dir_p = Utils.Io.directory_exists outdir in
    let* git_dir = git_dir_p in
    let* strings_dir = strings_dir_p in
    if not (git_dir || strings_dir) then failwith "This program must be run from the root of your project";
    match strings_dir with
    | true -> Lwt_unix.files_of_directory outdir |> Lwt_stream.to_list
    | false ->
      let+ () = Utils.Io.mkdir_p ~dir_name:outdir ~perms:0o751 in
      []
  in
  (* English *)
  let* english =
    let table = String.Table.create () in
    let counts = { vue = ref 0; pug = ref 0; html = ref 0; js = ref 0; ts = ref 0 } in
    let time = Utils.Timing.start () in
    let* () =
      Lwt_list.iter_p
        (fun directory ->
          let root = String.chop_suffix_if_exists ~suffix:"/" directory in
          let traversal =
            {
              rootlen = String.length root + 1;
              table;
              template_script = options.template_script;
              slow_pug = options.slow_pug;
              counts;
            }
          in
          traverse traversal root)
        options.targets
    in
    let english =
      String.Table.map table ~f:(fun set -> String.Set.to_array set |> String.concat_array ~sep:", ")
    in
    let* () =
      let f ext i = sprintf "%d %s file%s" i ext (plural i) in
      let time = Int63.(time `Stop - !Quickjs.init_time) in
      Lwt_io.printlf
        !"✅ [%{Int63}ms] Processed %s, %s, %s, %s, and %s"
        time (f ".js" !(counts.js)) (f ".ts" !(counts.ts)) (f ".html" !(counts.html))
        (f ".vue" !(counts.vue)) (f ".pug" !(counts.pug))
    in
    let+ () = write_english ~outdir english in
    english
  in
  (* Other languages *)
  let* () =
    Lwt_list.iter_p
      (fun filename ->
        match String.chop_suffix ~suffix:".strings" filename with
        | Some "english" -> Lwt.return_unit
        | Some language -> (
          let path = Filename.concat outdir filename in
          Lwt_unix.stat path >>= function
          | { st_kind = S_REG; _ } ->
            let* other =
              Lwt_io.with_file ~mode:Input ~flags:Utils.Io.read_flags path (Parsing.Strings.parse ~path)
            in
            write_other ~outdir ~language english other
          | _ -> Lwt.return_unit)
        | None -> Lwt.return_unit)
      strings_dir_files
  in
  Lwt_io.write_line Lwt_io.stdout (sprintf !"Completed. (%{Int63}ms)" (overall_time `Stop))

let () =
  let open Command in
  let open Command.Let_syntax in
  let common =
    let%map_open targets = "path" %: string |> sequence |> anon
    and outdir =
      flag_optional_with_default_doc "--output" ~aliases:[ "-o" ] ~full_flag_required:() string
        [%sexp_of: string] ~default:"strings" ~doc:"DIR Change default output directory"
    and use_ts =
      flag "--ts" ~full_flag_required:() no_arg
        ~doc:
          "Use this option if your HTML/Pug files use TypeScript as their scripting language in element \
           attributes like onClick=\"\""
    and slow_pug =
      flag "--slow-pug" ~aliases:[ "--sp" ] ~full_flag_required:() no_arg
        ~doc:
          "Use the official Pug parser. Much slower, especially on large files. Use this option if any \
           translation seems to be missing from a Pug file, and report the bug if this option fixes it."
    in
    { outdir; targets; template_script = (if use_ts then TS else JS); slow_pug }
  in
  let action =
    let open Param in
    let debug_pug =
      flag "--debug-pug" ~aliases:[ "--dp" ] ~full_flag_required:() no_arg
        ~doc:"Debug pug templates in .vue files"
      >>| Fn.flip Option.some_if (Debug Pug)
    in
    let debug_html =
      flag "--debug-html" ~aliases:[ "--dh" ] ~full_flag_required:() no_arg
        ~doc:"Debug html templates in .vue files"
      >>| Fn.flip Option.some_if (Debug Html)
    in
    choose_one [ debug_pug; debug_html ] ~if_nothing_chosen:(Default_to Run)
  in

  let handle_system_failure = function
    | (Failure _ as ex)
     |(Core_unix.Unix_error _ as ex)
     |(Exn.Reraised _ as ex) ->
      let message = Utils.Exception.human ex in
      let* () = Lwt_io.write_line Lwt_io.stderr (sprintf "❌ An error occured:\n%s" message) in
      exit 1
    | exn -> raise exn
  in

  Param.both common action
  >>| (fun (common, action) () ->
        let program () = main common action in
        Lwt_main.run (Lwt.catch program handle_system_failure))
  |> basic ~summary:"Extract i18n strings - https://github.com/okTurtles/strings"
  |> Command_unix.run ~version
