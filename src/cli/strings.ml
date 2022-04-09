module U = Unix
open Core
open Lwt.Infix
open Lwt.Syntax

let version = "1.2.0"

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

let time () =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  fun () ->
    let t1 = Time_now.nanoseconds_since_unix_epoch () in
    let diff = Int63.( - ) t1 t0 in
    sprintf "%sms" Int63.(to_string (diff / of_int 1_000_000))

let pool = Lwt_pool.create 6 (fun () -> Lwt.return_unit)

let read_flags = U.[ O_RDONLY; O_NONBLOCK ]

let write_flags = U.[ O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT ]

let process_file ~root strings count filename ~f =
  Lwt_pool.use pool (fun () ->
      incr count;
      let+ parsed = Lwt_io.with_file ~mode:Input ~flags:read_flags filename f in
      Queue.iter parsed ~f:(fun string ->
          let data = String.chop_prefix filename ~prefix:root |> Option.value ~default:filename in
          String.Table.update strings string ~f:(function
            | None -> String.Set.add String.Set.empty data
            | Some set -> String.Set.add set data)))

let rec traverse ~root ~count_vue ~count_js strings directory =
  let* entries =
    Lwt_pool.use pool (fun () -> Lwt_unix.files_of_directory directory |> Lwt_stream.to_list)
  in
  Lwt_list.iter_p
    (function
      | filename when String.is_prefix ~prefix:"." filename || String.( = ) filename "node_modules" ->
        Lwt.return_unit
      | filename -> (
        let path = sprintf "%s/%s" directory filename in
        Lwt_unix.lstat path >>= function
        | { st_kind = S_REG; _ } when String.is_suffix ~suffix:".vue" filename ->
          process_file ~root strings count_vue path ~f:(Vue.parse ~filename ~f:Vue.extract_strings)
        | { st_kind = S_REG; _ } when String.is_suffix ~suffix:".js" filename ->
          process_file ~root strings count_js path ~f:(fun ic ->
              let+ source = Lwt_io.read ic in
              let parsed = Queue.create () in
              Parsing.Js_ast.strings ~filename:path parsed source;
              parsed)
        | { st_kind = S_DIR; _ } -> traverse ~root ~count_vue ~count_js strings path
        | _ -> Lwt.return_unit))
    entries

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

let write_english english =
  let time = time () in
  let path_strings = "strings/english.strings" in
  let path_json = "strings/english.json" in
  let first = ref true in
  let* () =
    Lwt_io.with_file ~flags:write_flags ~mode:Output path_strings (fun oc_strings ->
        Lwt_io.with_file ~flags:write_flags ~mode:Output path_json (fun oc_json ->
            let* () = Lwt.join [ Lwt_io.write oc_strings header; Lwt_io.write_char oc_json '{' ] in
            let* () =
              String.Table.fold english ~init:Lwt.return_unit ~f:(fun ~key ~data acc ->
                  let fmt_key = fmt key in
                  let output_strings = sprintf "/* %s */\n%s = %s;\n\n" data fmt_key fmt_key in
                  let output_json = json_pair fmt_key fmt_key first in
                  let* () = acc in
                  Lwt_io.write oc_strings output_strings <&> Lwt_io.write oc_json output_json)
            in
            Lwt_io.write oc_json "\n}\n"))
  in
  Lwt_io.printlf "✅ [%s] Generated '%s' and '%s' with:\n- %d unique strings\n" (time ()) path_strings
    path_json (String.Table.length english)

let write_other ~language english other =
  let time = time () in
  let path_strings = sprintf "strings/%s.strings" language in
  let path_json = sprintf "strings/%s.json" language in
  let* n_left, n_right, n_both =
    Lwt_io.with_file ~flags:write_flags ~mode:Output path_strings (fun oc_strings ->
        Lwt_io.with_file ~flags:write_flags ~mode:Output path_json (fun oc_json ->
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
    "✅ [%s] Generated '%s' and '%s' with:\n- %d new strings\n- %d existing strings\n- %d unused strings\n"
    (time ()) path_strings path_json n_left n_both n_right

let directory_exists path =
  let+ stat = Lwt_unix.stat path in
  match stat with
  | { st_kind = S_DIR; _ } -> true
  | { st_kind = _; _ } -> failwithf "%s already exists, but is not a directory" path ()
  | exception _ -> false

let main args =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  match args with
  | [ _; "-v" ]
   |[ _; "--version" ] ->
    let* () = Lwt_io.write_line Lwt_io.stdout (sprintf "Version %s" version) in
    exit 0
  | [ _; "debug"; "pug"; filename ] ->
    let* () = Lwt_io.printlf "Debugging %s" filename in
    Lwt_io.with_file ~flags:read_flags ~mode:Input filename (fun ic ->
        Vue.parse ~filename ic ~f:Vue.debug_pug)
  | [ _ ] -> failwith "At least one argument is required"
  | _ :: directories ->
    (* Check current directory *)
    let* strings_dir_files =
      let git_dir_p = directory_exists ".git" in
      let strings_dir_p = directory_exists "strings" in
      let* git_dir = git_dir_p in
      let* strings_dir = strings_dir_p in
      if not (git_dir || strings_dir)
      then failwith "This program must be run from the root of your project";
      match strings_dir with
      | true -> Lwt_unix.files_of_directory "strings" |> Lwt_stream.to_list
      | false ->
        let+ () = Lwt_unix.mkdir "strings" 0o751 in
        []
    in
    (* English *)
    let* english =
      let english_list = String.Table.create () in
      let count_vue = ref 0 in
      let count_js = ref 0 in
      let time = time () in
      let* () =
        Lwt_list.iter_p
          (fun directory ->
            let root = String.chop_suffix ~suffix:"/" directory |> Option.value ~default:directory in
            traverse ~root:(sprintf "%s/" root) ~count_vue ~count_js english_list root)
          directories
      in
      let english =
        String.Table.map english_list ~f:(fun set ->
            String.Set.to_array set |> String.concat_array ~sep:", ")
      in
      let* () =
        Lwt_io.printlf "✅ [%s] Processed %d .vue files and %d .js files" (time ()) !count_vue !count_js
      in
      let+ () = write_english english in
      english
    in
    (* Other languages *)
    let* () =
      Lwt_list.iter_p
        (fun filename ->
          match String.chop_suffix ~suffix:".strings" filename with
          | Some "english" -> Lwt.return_unit
          | Some language -> (
            let path = sprintf "strings/%s" filename in
            Lwt_unix.stat path >>= function
            | { st_kind = S_REG; _ } ->
              let* other =
                Lwt_io.with_file ~mode:Input ~flags:read_flags path (Parsing.Strings.parse ~filename)
              in
              write_other ~language english other
            | _ -> Lwt.return_unit)
          | None -> Lwt.return_unit)
        strings_dir_files
    in
    let t1 = Time_now.nanoseconds_since_unix_epoch () in
    Lwt_io.write_line Lwt_io.stdout
      (sprintf "Completed. (%sms)" Int63.(to_string ((t1 - t0) / of_int 1_000_000)))
  | _ -> failwith "Expected Unix calling convention"

let () =
  Lwt_main.run
    (Lwt.catch
       (fun () -> main (Sys.get_argv () |> Array.to_list))
       (function
         | (Failure _ as ex)
          |(U.Unix_error _ as ex)
          |(Exn.Reraised _ as ex) ->
           let message = Utils.Exception.human ex in
           let* () = Lwt_io.write_line Lwt_io.stderr (sprintf "❌ An error occured:\n%s" message) in
           exit 1
         | exn -> raise exn))
