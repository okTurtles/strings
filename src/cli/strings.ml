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

let process_file (strings, count) filename =
  Lwt_pool.use pool (fun () ->
    incr count;
    let%lwt parsed = Lwt_io.with_file ~mode:Input ~flags:Unix.[O_RDONLY; O_NONBLOCK] filename (Vue.parse filename) in
    Queue.iter parsed ~f:(fun string ->
      String.Table.add_multi strings ~key:string ~data:filename
    );
    Lwt.return_unit
  )

let rec traverse strings directory =
  let%lwt entries = Lwt_pool.use pool (fun () -> Lwt_unix.files_of_directory directory |> Lwt_stream.to_list) in
  Lwt_list.iter_p (function
  | filename when String.is_prefix ~prefix:"." filename -> Lwt.return_unit
  | filename ->
    let path = sprintf "%s/%s" directory filename in
    begin match%lwt Lwt_unix.lstat path with
    | { st_kind = S_REG; _ } when String.is_suffix ~suffix:".vue" filename -> process_file strings path
    | { st_kind = S_DIR; _ } -> traverse strings path
    | _ -> Lwt.return_unit
    end
  ) entries

let fmt s = Yojson.Basic.to_string (`String s)

let write_english ~filename english count =
  let%lwt () = Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT] ~mode:Output filename (fun oc ->
      String.Table.fold english ~init:Lwt.return_unit ~f:(fun ~key ~data acc ->
        let formatted = fmt key in
        let output = sprintf "/* %s */\n%s = %s;\n\n" (String.concat ~sep:", " data) formatted formatted in
        let%lwt () = acc in
        Lwt_io.write oc output
      )
    )
  in
  sprintf "Processed %d .vue files\n\nGenerated '%s' with:\n- %d unique strings"
    count filename (String.Table.length english)
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
    sprintf "\nGenerated '%s' with:\n- %d new strings\n- %d existing strings\n- %d unused strings"
      filename n_left n_both n_right
    |> Lwt_io.write_line Lwt_io.stdout
  )

let main args =
  let%lwt directories = begin match args with
  | _::[] -> failwith "At least one argument is required"
  | _::x -> Lwt.return x
  | _ -> failwith "Expected Unix calling convention"
  end
  in
  let%lwt english =
    let english = String.Table.create () in
    let count = ref 0 in
    let%lwt () = Lwt_list.iter_p (fun directory ->
        traverse (english, count) (String.chop_suffix ~suffix:"/" directory |> Option.value ~default:directory)
      ) directories
    in
    let%lwt () = write_english ~filename:"english.strings" english !count in
    Lwt.return english
  in
  let%lwt () = Lwt_stream.iter_p (fun filename ->
      begin match String.chop_suffix ~suffix:".strings" filename with
      | Some "english" -> Lwt.return_unit
      | Some basename ->
        begin match%lwt Lwt_unix.stat filename with
        | { st_kind = S_REG; _ } ->
          let%lwt other = Lwt_io.with_file ~mode:Input ~flags:Unix.[O_RDONLY; O_NONBLOCK] filename Parsing.Strings.parse in
          write_other ~filename:(sprintf "%s.strings" basename) english other
        | _ -> Lwt.return_unit
        end
      | None -> Lwt.return_unit
      end
    ) (Lwt_unix.files_of_directory ".")
  in
  Lwt.return_unit

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
