open Core_kernel

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    let p1 = Lwt_io.write_line Lwt_io.stderr (sprintf "💀 PLEASE REPORT THIS BUG. UNCAUGHT EXCEPTION: %s" (Utils.Exception.human ex)) in
    let p2 = Lwt_unix.sleep 3. in
    Lwt.join [p1; p2]
    >>= (fun () -> exit 2)
    |> ignore
  )

let pool = Lwt_pool.create 6 (fun () -> Lwt.return_unit)

let process_file ~root (strings, count) filename =
  Lwt_pool.use pool (fun () ->
    incr count;
    let%lwt parsed = Lwt_io.with_file ~mode:Input ~flags:Unix.[O_RDONLY; O_NONBLOCK] filename (Vue.parse filename) in
    Queue.iter parsed ~f:(fun string ->
      let data = String.chop_prefix filename ~prefix:root |> Option.value ~default:filename in
      String.Table.add_multi strings ~key:string ~data
    );
    Lwt.return_unit
  )

let rec traverse ~root strings directory =
  let%lwt entries = Lwt_pool.use pool (fun () -> Lwt_unix.files_of_directory directory |> Lwt_stream.to_list) in
  Lwt_list.iter_p (function
  | filename when String.is_prefix ~prefix:"." filename -> Lwt.return_unit
  | filename ->
    let path = sprintf "%s/%s" directory filename in
    begin match%lwt Lwt_unix.lstat path with
    | { st_kind = S_REG; _ } when String.is_suffix ~suffix:".vue" filename -> process_file ~root strings path
    | { st_kind = S_DIR; _ } -> traverse ~root strings path
    | _ -> Lwt.return_unit
    end
  ) entries

let fmt s = Yojson.Basic.to_string (`String s)

let write_english english count =
  (* Write .strings *)
  let strings_path = "strings/english.strings" in
  let flags = Unix.[O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT] in
  let%lwt () = Lwt_io.with_file ~flags ~mode:Output strings_path (fun oc ->
      String.Table.fold english ~init:Lwt.return_unit ~f:(fun ~key ~data acc ->
        let formatted = fmt key in
        let output = sprintf "/* %s */\n%s = %s;\n\n" (String.concat ~sep:", " data) formatted formatted in
        let%lwt () = acc in
        Lwt_io.write oc output
      )
    )
  in
  sprintf "Processed %d .vue files\n...\nGenerated '%s' with:\n- %d unique strings\n..."
    count strings_path (String.Table.length english)
  |> Lwt_io.write_line Lwt_io.stdout

let write_other ~filename english other =
  Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT] ~mode:Output filename (fun oc ->
    let missing_translation key x =
      let formatted = fmt key in
      let line = sprintf "/* MISSING TRANSLATION - %s */\n%s = %s;\n\n" (String.concat ~sep:", " x) formatted formatted in
      Some (`Left, Lwt_io.write oc line)
    in
    let table = String.Table.merge english other ~f:(fun ~key -> function
      | `Left x -> missing_translation key x
      | `Both (x, y) when String.(key = y) -> missing_translation key x
      | `Both (x, y) ->
        let line = sprintf "/* %s */\n%s = %s;\n\n" (String.concat x) (fmt key) (fmt y) in
        Some (`Both, Lwt_io.write oc line)
      | `Right y when String.(key = y) -> None
      | `Right y ->
        let line = sprintf "/* Not currently used */\n%s = %s;\n\n" (fmt key) (fmt y) in
        Some (`Right, Lwt_io.write oc line)
      )
    in
    let%lwt n_left, n_right, n_both = String.Table.fold table ~init:(Lwt.return (0, 0, 0)) ~f:(fun ~key:_ ~data:(w, p) acc ->
        let%lwt () = p in
        let%lwt x, y, z = acc in
        w |> function
        | `Left -> Lwt.return ((succ x), y, z)
        | `Right -> Lwt.return (x, (succ y), z)
        | `Both -> Lwt.return (x, y, (succ z))
      )
    in
    sprintf "Generated '%s' with:\n- %d new strings\n- %d existing strings\n- %d unused strings\n..."
      filename n_left n_both n_right
    |> Lwt_io.write_line Lwt_io.stdout
  )

let directory_exists path =
  begin match%lwt Lwt_unix.stat path with
  | { st_kind = S_DIR; _ } -> Lwt.return_true
  | { st_kind = _; _ } -> failwithf "%s already exists, but is not a directory" path ()
  | exception _ -> Lwt.return_false
  end


let main args =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  let%lwt directories = begin match args with
  | _::[] -> failwith "At least one argument is required"
  | _::x -> Lwt.return x
  | _ -> failwith "Expected Unix calling convention"
  end
  in
  (* Check current directory *)
  let%lwt strings_dir_files =
    let git_dir_p = directory_exists ".git" in
    let strings_dir_p = directory_exists "strings" in
    let%lwt git_dir = git_dir_p in
    let%lwt strings_dir = strings_dir_p in
    if not (git_dir || strings_dir) then failwith "This program must be run from the root of your project";
    begin match strings_dir with
    | true -> Lwt_unix.files_of_directory "strings" |> Lwt_stream.to_list
    | false ->
      let%lwt () = Lwt_unix.mkdir "strings" 0o751 in
      Lwt.return_nil
    end
  in
  (* English *)
  let%lwt english =
    let english = String.Table.create () in
    let count = ref 0 in
    let%lwt () = Lwt_list.iter_p (fun directory ->
        let root = (String.chop_suffix ~suffix:"/" directory |> Option.value ~default:directory) in
        traverse ~root:(sprintf "%s/" root) (english, count) root
      ) directories
    in
    let%lwt () = write_english english !count in
    Lwt.return english
  in
  (* Other languages *)
  let%lwt () = Lwt_list.iter_p (fun filename ->
      begin match String.chop_suffix ~suffix:".strings" filename with
      | Some "english" -> Lwt.return_unit
      | Some _ ->
        let path = sprintf "strings/%s" filename in
        begin match%lwt Lwt_unix.stat path with
        | { st_kind = S_REG; _ } ->
          let%lwt other = Lwt_io.with_file ~mode:Input ~flags:Unix.[O_RDONLY; O_NONBLOCK] path Parsing.Strings.parse in
          write_other ~filename:path english other
        | _ -> Lwt.return_unit
        end
      | None -> Lwt.return_unit
      end
    ) strings_dir_files
  in
  let t1 = Time_now.nanoseconds_since_unix_epoch () in
  Lwt_io.write_line Lwt_io.stdout (sprintf "Completed. (%sms)" Int63.(to_string ((t1 - t0) / (of_int 1_000_000))))

let () =
  Lwt_main.run (
    try%lwt
      main (Sys.argv |> Array.to_list)
    with
    | (Failure _ as ex) | (Unix.Unix_error _ as ex) | (Exn.Reraised _ as ex) ->
      let message = Utils.Exception.human ex in
      let%lwt () = Lwt_io.write_line Lwt_io.stderr (sprintf "❌ An error occured:\n%s" message) in
      exit 1
  )
