open Core_kernel

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    let p1 = Lwt_io.write_line Lwt_io.stderr (sprintf "💀 PLEASE REPORT THIS BUG. UNCAUGHT EXCEPTION: %s" (Utils.Exception.human ex)) in
    let p2 = Lwt_unix.sleep 3. in
    Lwt.join [p1; p2]
    >>= (fun () -> exit 2)
    |> ignore
  )

let pool = Lwt_pool.create 10 (fun () -> Lwt.return_unit)

let process_file strings filename =
  Lwt_pool.use pool (fun () ->
    let%lwt parsed = Lwt_io.with_file ~mode:Input ~flags:Unix.[O_RDONLY; O_NONBLOCK] filename (Vue.parse filename) in
    Queue.iter parsed ~f:(fun string ->
      String.Table.add_multi strings ~key:string ~data:filename
    );
    Lwt.return_unit
  )

let rec traverse strings directory =
  Lwt_stream.iter_p (function
  | filename when String.is_prefix ~prefix:"." filename -> Lwt.return_unit
  | filename ->
    let path = sprintf "%s/%s" directory filename in
    begin match%lwt Lwt_unix.stat path with
    | { st_kind = S_REG; _ } when String.is_suffix ~suffix:".vue" filename -> process_file strings path
    | { st_kind = S_DIR; _ } -> traverse strings path
    | _ -> Lwt.return_unit
    end
  ) (Lwt_unix.files_of_directory directory)

let fmt s = Yojson.Basic.to_string (`String s)

let write_english ~filename english =
  let log_p = Lwt_io.write_line Lwt_io.stdout (sprintf "Generating: %s" filename) in
  let file_p = Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT] ~mode:Output filename (fun oc ->
      String.Table.fold english ~init:Lwt.return_unit ~f:(fun ~key ~data acc ->
        let formatted = fmt key in
        let output = sprintf "/* %s */\n%s = %s;\n\n" (String.concat ~sep:", " data) formatted formatted in
        let%lwt () = acc in
        Lwt_io.write oc output
      )
    )
  in
  Lwt.join [log_p; file_p]

let write_other ~filename english other =
  let log_p = Lwt_io.write_line Lwt_io.stdout (sprintf "Generating: %s" filename) in
  let file_p = Lwt_io.with_file ~flags:Unix.[O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT] ~mode:Output filename (fun oc ->
      String.Table.fold english ~init:Lwt.return_unit ~f:(fun ~key ~data:_ acc ->
        let output = begin match String.Table.find other key with
        | None ->
          let formatted = fmt key in
          sprintf "/* MISSING TRANSLATION */\n%s = %s;\n\n" formatted formatted
        | Some translated -> sprintf "%s = %s;\n\n" (fmt key) (fmt translated)
        end
        in
        let%lwt () = acc in
        Lwt_io.write oc output
      )
    )
  in
  Lwt.join [log_p; file_p]

let main args =
  let%lwt directories = begin match args with
  | _::[] -> let%lwt () = Lwt_io.write_line Lwt_io.stderr "At least one argument is required" in exit 1
  | _::x -> Lwt.return x
  | _ -> let%lwt () = Lwt_io.write_line Lwt_io.stderr "Expected Unix calling convention" in exit 1
  end
  in
  let english = String.Table.create () in
  let%lwt () = Lwt_list.iter_p (fun directory ->
      traverse english (String.chop_suffix ~suffix:"/" directory |> Option.value ~default:directory)
    ) directories
  in
  let%lwt () = write_english ~filename:"english.strings" english in

  let%lwt () = Lwt_stream.iter_s (fun filename ->
      begin match String.chop_suffix ~suffix:".translation" filename with
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
      print_endline (sprintf "❌ An error occured:\n%s" message);
      exit 1
  )
