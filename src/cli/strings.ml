open! Core
open Eio.Std

let version = "2.3.0"

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
  show_debugging: bool;
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
  wp: Eio.Workpool.t;
  slow_pug: bool;
  template_script: Vue.template_script;
  root: string;
}

type job = {
  file_type: file_type;
  eio_path: Eio.Fs.dir Eio.Path.t;
  path: string;
  template_script: Vue.template_script;
  slow_pug: bool;
}

let file_type_of_filename filename =
  match lazy (String.slice filename (-4) 0), String.slice filename (-3) 0 with
  | _, ".js" when String.is_suffix ~suffix:".js" filename -> Some JS
  | _, ".ts" -> Some TS
  | (lazy ".vue"), _ -> Some VUE
  | (lazy ".pug"), _ -> Some PUG
  | _, _ when String.is_suffix filename ~suffix:".html" -> Some HTML
  | _ -> None

let reduce_collector ~stderr table count collector ~root =
  incr count;
  Utils.Collector.render_errors collector
  |> Option.iter ~f:(fun s -> Eio.Flow.copy_string (sprintf "%s\n" s) stderr);
  Queue.iter collector.strings ~f:(fun string ->
    let relpath = String.chop_prefix_exn collector.path ~prefix:root in
    Hashtbl.update table string ~f:(function
      | None -> Set.add String.Set.empty relpath
      | Some set -> Set.add set relpath ) )

let process_job ({ path; _ } as job) =
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

let rec traverse ~fs ~stderr ({ slow_pug; template_script; counts; wp; _ } as traversal) directory =
  Eio.Path.with_open_dir Eio.Path.(fs / directory) Eio.Path.read_dir
  |> Fiber.List.iter (fun filename ->
       let path = Filename.concat directory filename in
       match filename, Utils.Io.stat wp path with
       | "node_modules", _ -> ()
       | _, { st_kind = S_DIR; _ } -> traverse ~fs ~stderr traversal path
       | _, { st_kind = S_REG; _ } -> (
         match file_type_of_filename filename with
         | None -> ()
         | Some file_type ->
           let job = { file_type; eio_path = Eio.Path.(fs / path); path; template_script; slow_pug } in
           let collector = Eio.Workpool.run_exn traversal.wp (fun () -> process_job job) in
           let count =
             match job.file_type with
             | JS -> counts.js
             | TS -> counts.ts
             | VUE -> counts.vue
             | PUG -> counts.pug
             | HTML -> counts.html
           in
           reduce_collector ~stderr traversal.table count collector ~root:traversal.root )
       | _ -> () )

let main env options = function
| Debug lang ->
  let string_parsers = Parsing.Basic.make_string_parsers () in
  let fs = Eio.Stdenv.fs env in
  let stdout = Eio.Stdenv.stdout env in
  List.iter options.targets ~f:(fun path ->
    Eio.Flow.copy_string (sprintf "\n>>> Debugging [%s]\n" path) stdout;
    let ({ slow_pug; template_script; _ } : common_options) = options in
    Eio.Path.with_open_in Eio.Path.(fs / path) @@ fun flow ->
    match lang, String.slice path (-4) 0 with
    | _, ".vue" ->
      let languages = Vue.parse ~path ~slow_pug flow in
      Vue.debug_template ~stdout ~path languages template_script lang
    | Pug, ".pug" -> (
      let source = Utils.Io.load_flow flow in
      let slow_parse () =
        let collector = Utils.Collector.create ~path in
        Quickjs.extract_to_collector collector Pug source;
        Vue.debug_template ~stdout ~path
          [ Pug { collector; length = String.length source } ]
          template_script lang
      in
      match slow_pug with
      | true -> slow_parse ()
      | false ->
        let on_ok parsed =
          Vue.debug_template ~stdout ~path [ Pug_native { parsed; length = None } ] template_script lang
        in
        let on_error ~msg =
          Eio.Flow.copy_string
            (sprintf "Falling back to official Pug parser for %s (%s)\n" path msg)
            stdout;
          slow_parse ()
        in
        Parsing.Basic.exec_parser ~on_ok ~on_error (Parsing.Pug.parser string_parsers) ~path
          ~language_name:"Pug" source )
    | Html, _ when String.is_suffix path ~suffix:".html" ->
      let on_ok parsed =
        Vue.debug_template ~stdout ~path [ Html { parsed; length = None } ] template_script lang
      in
      Parsing.Basic.exec_parser_eio ~on_ok Parsing.Html.parser ~path ~language_name:"Pug" flow
    | _ -> Eio.Flow.copy_string (sprintf "Nothing to do for file [%s]\n" path) stdout )
| Run ->
  let overall_time = Utils.Timing.start () in
  Switch.run @@ fun sw ->
  let outdir = options.outdir in
  if List.is_empty options.targets then failwith "Please specify at least one directory";
  let fs = Eio.Stdenv.fs env in
  let stdout = Eio.Stdenv.stdout env in
  let sys_wp =
    Eio.Workpool.create ~sw ~domain_count:Utils.Io.num_systhreads ~domain_concurrency:1
      (Eio.Stdenv.domain_mgr env)
  in
  (* Check current directory *)
  let strings_dir_files =
    let git_dir, strings_dir =
      Fiber.pair
        (fun () -> Utils.Io.directory_exists sys_wp ".git")
        (fun () -> Utils.Io.directory_exists sys_wp outdir)
    in
    if not (git_dir || strings_dir) then failwith "This program must be run from the root of your project";
    match strings_dir with
    | true -> Eio.Path.with_open_dir Eio.Path.(fs / outdir) Eio.Path.read_dir
    | false ->
      Utils.Io.mkdir_p env sys_wp ~dir_name:outdir ~perms:0o751;
      []
  in

  (* Traverse *)
  let time = Utils.Timing.start () in
  let table, counts =
    let table = String.Table.create () in
    let counts = { vue = ref 0; pug = ref 0; html = ref 0; js = ref 0; ts = ref 0 } in
    Switch.run @@ fun sw ->
    let wp =
      Eio.Workpool.create ~sw ~domain_count:Utils.Io.num_cores
        ~domain_concurrency:Utils.Io.traversal_jobs_per_core (Eio.Stdenv.domain_mgr env)
    in
    options.targets
    |> Fiber.List.iter (fun directory ->
         let root_slash, root_noslash =
           match String.chop_suffix ~suffix:"/" directory with
           | Some s -> directory, s
           | None -> sprintf "%s/" directory, directory
         in
         traverse ~fs ~stderr:(Eio.Stdenv.stderr env)
           {
             table;
             counts;
             wp;
             slow_pug = options.slow_pug;
             template_script = options.template_script;
             root = root_slash;
           }
           root_noslash );
    table, counts
  in

  Switch.run (fun sw ->
    (* English *)
    let english =
      let english = Hashtbl.map table ~f:(fun set -> Set.to_array set |> String.concat_array ~sep:", ") in
      let f ext i = sprintf "%d %s file%s" i ext (if i = 1 then "" else "s") in
      let time = Int63.(time `Stop - Atomic.get Quickjs.init_time) in
      Eio.Flow.copy_string
        (sprintf
           !"✅ [%{Int63}ms] Processed %s, %s, %s, %s, and %s\n"
           time (f ".js" !(counts.js)) (f ".ts" !(counts.ts)) (f ".html" !(counts.html))
           (f ".vue" !(counts.vue)) (f ".pug" !(counts.pug)) )
        stdout;
      Fiber.fork ~sw (fun () -> Generate.write_english ~fs ~stdout ~version ~outdir english);
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
          match Utils.Io.stat sys_wp path with
          | { st_kind = S_REG; _ } ->
            let other = Eio.Path.with_open_in Eio.Path.(fs / path) (Parsing.Strings.parse ~path) in
            Generate.write_other ~fs ~stdout ~version ~outdir ~language english other
          | _ -> () )
        | None -> ())
      strings_dir_files );
  Eio.Flow.copy_string (sprintf !"Completed. (%{Int63}ms)\n" (overall_time `Stop)) (Eio.Stdenv.stderr env)

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
    and show_debugging =
      flag "--show-debugging" ~full_flag_required:() no_arg ~doc:"Use this option when reporting bugs."
    in
    { outdir; targets; template_script = (if use_ts then TS else JS); slow_pug; show_debugging }
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
        (* TODO: Revert to Eio_main.run once Eio issue #559 is fixed *)
        Eio_posix.run (fun env ->
          try program env with
          | exn when not common.show_debugging -> handle_system_failure (Eio.Stdenv.stderr env) exn ))
  |> basic ~summary:"Extract i18n strings - https://github.com/okTurtles/strings"
  |> Command_unix.run ~version
