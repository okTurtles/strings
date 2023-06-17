open! Core
open Eio.Std

let version = "2.2.1"

let plural i = if i = 1 then "" else "s"

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

type file_type =
  | JS
  | TS
  | VUE
  | PUG
  | HTML

type traversal = {
  table: String.Set.t String.Table.t;
  counts: counts;
  dispatcher: Utils.Dispatcher.t;
  slow_pug: bool;
  template_script: Vue.template_script;
}

type job = {
  file_type: file_type;
  eio_path: Eio.Fs.dir Eio.Path.t;
  path: string;
  template_script: Vue.template_script;
  slow_pug: bool;
}

let file_type_of_filename filename =
  match lazy (String.slice filename (-4) 0), lazy (String.slice filename (-3) 0) with
  | _, (lazy ".js") when String.is_suffix ~suffix:".js" filename -> Some JS
  | _, (lazy ".ts") -> Some TS
  | (lazy ".vue"), _ -> Some VUE
  | (lazy ".pug"), _ -> Some PUG
  | _, _ when String.is_suffix filename ~suffix:".html" -> Some HTML
  | _ -> None

let reduce_collector env table count collector =
  incr count;
  Utils.Collector.render_errors collector
  |> Option.iter ~f:(fun s -> Eio.Flow.copy_string (sprintf "%s\n" s) env#stderr);
  Queue.iter collector.strings ~f:(fun string ->
    let realname = collector.filename in
    String.Table.update table string ~f:(function
      | None -> String.Set.add String.Set.empty realname
      | Some set -> String.Set.add set realname ) )

let do_work ({ path; _ } as job) =
  let collector = Utils.Collector.create ~path in
  (match job with
  | { file_type = JS; eio_path; _ } ->
    let source = Eio.Path.load eio_path in
    Parsing.Js.extract_to_collector collector source
  | { file_type = TS; eio_path; _ } ->
    let source = Eio.Path.load eio_path in
    Quickjs.extract_to_collector collector Typescript source
  | { file_type = VUE; eio_path; slow_pug; _ } ->
    Eio.Path.with_open_in eio_path @@ fun flow ->
    let languages = Vue.parse ~path ~slow_pug flow in
    Vue.collect_from_languages collector languages
  | { file_type = PUG; eio_path; slow_pug; _ } -> (
    let source = Eio.Path.load eio_path in
    let slow_parse () = Quickjs.extract_to_collector collector Pug source in
    match slow_pug with
    | true -> slow_parse ()
    | false ->
      let on_ok parsed = Parsing.Pug.collect collector parsed in
      let on_error ~msg:_ = slow_parse () in
      Parsing.(Basic.exec_parser ~on_ok ~on_error (Pug.parser (Basic.make_string_parsers ())))
        ~path ~language_name:"Pug" source )
  | { file_type = HTML; eio_path; _ } ->
    Eio.Path.with_open_in eio_path @@ fun flow ->
    let on_ok parsed = Parsing.Html.collect collector parsed in
    Parsing.Basic.exec_parser_eio ~on_ok Parsing.Html.parser ~path ~language_name:"HTML" flow);

  Vue.collect_from_possible_scripts collector job.template_script

let rec traverse env ({ slow_pug; template_script; counts; _ } as traversal) directory =
  Eio.Path.with_open_dir Eio.Path.(env#fs / directory) Eio.Path.read_dir
  |> Fiber.List.iter (fun filename ->
       let path = Filename.concat directory filename in
       match filename, Utils.Io.stat path with
       | "node_modules", _ -> ()
       | _, { st_kind = S_DIR; _ } -> traverse env traversal path
       | _, { st_kind = S_REG; _ } -> (
         match file_type_of_filename filename with
         | None -> ()
         | Some file_type ->
           let job =
             { file_type; eio_path = Eio.Path.(env#fs / path); path; template_script; slow_pug }
           in
           let collector = Utils.Dispatcher.run_exn traversal.dispatcher ~f:(fun () -> do_work job) in
           let count =
             match job.file_type with
             | JS -> counts.js
             | TS -> counts.ts
             | VUE -> counts.vue
             | PUG -> counts.pug
             | HTML -> counts.html
           in
           reduce_collector env traversal.table count collector )
       | _ -> () )

let main env options = function
| Debug lang ->
  let string_parsers = Parsing.Basic.make_string_parsers () in
  List.iter options.targets ~f:(fun path ->
    Eio.Flow.copy_string (sprintf "\n>>> Debugging [%s]\n" path) env#stdout;
    let ({ slow_pug; template_script; _ } : common_options) = options in
    Eio.Path.with_open_in Eio.Path.(env#fs / path) @@ fun flow ->
    match lang, String.slice path (-4) 0 with
    | _, ".vue" ->
      let languages = Vue.parse ~path ~slow_pug flow in
      Vue.debug_template env ~path languages template_script lang
    | Pug, ".pug" -> (
      let source = Utils.Io.load_flow flow in
      let slow_parse () =
        let collector = Utils.Collector.create ~path in
        Quickjs.extract_to_collector collector Pug source;
        Vue.debug_template env ~path
          [ Pug { collector; length = String.length source } ]
          template_script lang
      in
      match slow_pug with
      | true -> slow_parse ()
      | false ->
        let on_ok parsed =
          Vue.debug_template env ~path [ Pug_native { parsed; length = None } ] template_script lang
        in
        let on_error ~msg =
          Eio.Flow.copy_string
            (sprintf "Falling back to official Pug parser for %s (%s)\n" path msg)
            env#stdout;
          slow_parse ()
        in
        Parsing.Basic.exec_parser ~on_ok ~on_error (Parsing.Pug.parser string_parsers) ~path
          ~language_name:"Pug" source )
    | Html, _ when String.is_suffix path ~suffix:".html" ->
      let on_ok parsed =
        Vue.debug_template env ~path [ Html { parsed; length = None } ] template_script lang
      in
      Parsing.Basic.exec_parser_eio ~on_ok Parsing.Html.parser ~path ~language_name:"Pug" flow
    | _ -> Eio.Flow.copy_string (sprintf "Nothing to do for file [%s]\n" path) env#stdout )
| Run ->
  let overall_time = Utils.Timing.start () in
  let outdir = options.outdir in
  (* Check current directory *)
  let strings_dir_files =
    let git_dir, strings_dir =
      Fiber.pair (fun () -> Utils.Io.directory_exists ".git") (fun () -> Utils.Io.directory_exists outdir)
    in
    if not (git_dir || strings_dir) then failwith "This program must be run from the root of your project";
    match strings_dir with
    | true -> Eio.Path.with_open_dir Eio.Path.(env#fs / outdir) Eio.Path.read_dir
    | false ->
      Utils.Io.mkdir_p env ~dir_name:outdir ~perms:0o751;
      []
  in

  (* Traverse *)
  let time = Utils.Timing.start () in
  let table, counts =
    let table = String.Table.create () in
    let counts = { vue = ref 0; pug = ref 0; html = ref 0; js = ref 0; ts = ref 0 } in
    Switch.run (fun sw ->
      let dispatcher =
        Utils.Dispatcher.create ~sw ~num_workers:Utils.Io.num_processors
          ~worker_limit:Utils.Io.processor_async env#domain_mgr
      in
      options.targets
      |> Fiber.List.iter (fun directory ->
           let root = String.chop_suffix_if_exists ~suffix:"/" directory in
           traverse env
             {
               table;
               counts;
               dispatcher;
               slow_pug = options.slow_pug;
               template_script = options.template_script;
             }
             root ) );
    table, counts
  in

  Switch.run (fun sw ->
    (* English *)
    let english =
      let english =
        String.Table.map table ~f:(fun set -> String.Set.to_array set |> String.concat_array ~sep:", ")
      in
      let f ext i = sprintf "%d %s file%s" i ext (plural i) in
      let time = Int63.(time `Stop - Atomic.get Quickjs.init_time) in
      Eio.Flow.copy_string
        (sprintf
           !"✅ [%{Int63}ms] Processed %s, %s, %s, %s, and %s\n"
           time (f ".js" !(counts.js)) (f ".ts" !(counts.ts)) (f ".html" !(counts.html))
           (f ".vue" !(counts.vue)) (f ".pug" !(counts.pug)) )
        env#stdout;
      Fiber.fork ~sw (fun () -> Generate.write_english env ~version ~outdir english);
      english
    in

    (* Other languages *)
    Fiber.fork ~sw @@ fun () ->
    Fiber.List.iter
      (fun filename ->
        match String.chop_suffix ~suffix:".strings" filename with
        | Some "english" -> ()
        | Some language -> (
          let path = Filename.concat outdir filename in
          match Utils.Io.stat path with
          | { st_kind = S_REG; _ } ->
            let other = Eio.Path.with_open_in Eio.Path.(env#fs / path) (Parsing.Strings.parse ~path) in
            Generate.write_other env ~version ~outdir ~language english other
          | _ -> () )
        | None -> ())
      strings_dir_files );
  Eio.Flow.copy_string (sprintf !"Completed. (%{Int63}ms)\n" (overall_time `Stop)) env#stderr

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

  let handle_system_failure stderr = function
    | (Eio.Io _ as ex)
     |(Eio.Exn.Multiple _ as ex)
     |(Failure _ as ex)
     |(Core_unix.Unix_error _ as ex)
     |(Exn.Reraised _ as ex) ->
      let message = Utils.Exception.human ex in
      Eio.Flow.copy_string (sprintf !"❌ An error occured:\n%s\n" message) stderr;
      exit 1
    | exn -> raise exn
  in

  Param.both common action
  >>| (fun (common, action) () ->
        let program env = main env common action in
        Eio_main.run (fun env ->
          try program env with
          | exn -> handle_system_failure env#stderr exn ))
  |> basic ~summary:"Extract i18n strings - https://github.com/okTurtles/strings"
  |> Command_unix.run ~version
