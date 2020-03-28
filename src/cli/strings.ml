open Core_kernel

let () = Lwt_engine.set ~transfer:true ~destroy:true (
    try
      (* MacOS *)
      new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.kqueue ()
    with _ ->
      (* Linux *)
      new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.epoll ()
  )

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

let main args =
  let%lwt directory = begin match args with
  | _::x::[] -> Lwt.return x
  | _::[] -> Lwt.return "."
  | argv ->
    let%lwt () = Lwt_io.write_line Lwt_io.stderr
        (sprintf "Unexpected arguments: %s" (Yojson.Basic.to_string (`List (List.map argv ~f:(fun x -> `String x)))))
    in
    exit 1
  end
  in
  let strings = String.Table.create () in
  let%lwt () = traverse strings (String.chop_suffix ~suffix:"/" directory |> Option.value ~default:directory) in
  let promises = String.Table.fold strings ~init:[] ~f:(fun ~key ~data acc ->
      let formatted = Yojson.Basic.to_string (`String key) in
      let output = sprintf "/* %s */\n%s = %s;\n\n" (String.concat ~sep:", " data) formatted formatted in
      (Lwt_io.write Lwt_io.stdout output)::acc
    )
  in
  Lwt.join promises

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
