open Core_kernel

let () = Lwt_engine.set ~transfer:true ~destroy:true (
    try
      (* Linux *)
      new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.epoll ()
    with _ ->
      (* MacOS *)
      new Lwt_engine.libev ~backend:Lwt_engine.Ev_backend.kqueue ()
  )

let () = Lwt.async_exception_hook := (fun ex ->
    let open Lwt in
    let p1 = Lwt_io.write_line Lwt_io.stderr (sprintf "💀 PLEASE REPORT THIS BUG. UNCAUGHT EXCEPTION: %s" (Utils.Exception.human ex)) in
    let p2 = Lwt_unix.sleep 3. in
    Lwt.join [p1; p2]
    >>= (fun () -> exit 2)
    |> ignore
  )

let process_file strings filename =
  let%lwt parsed = Lwt_io.with_file ~mode:Input ~flags:Unix.[O_RDONLY; O_NONBLOCK] filename (Vue.parse filename) in
  Queue.iter parsed ~f:(fun string ->
    String.Table.add_multi strings ~key:string ~data:filename
  );
  Lwt.return_unit

let main = function
| _::files ->
  let strings = String.Table.create () in
  let%lwt () = Lwt_list.iter_p (process_file strings) files in
  let json = String.Table.fold strings ~init:[] ~f:(fun ~key ~data acc ->
      let json = `Assoc [
          "English", `String key;
          "Files", `List (List.map data ~f:(fun x -> `String x));
        ]
      in
      json::acc
    )
  in
  let%lwt () = Lwt_io.write_line Lwt_io.stdout (Yojson.Basic.pretty_to_string ~std:true (`List json)) in
  Lwt.return_unit
| argv ->
  let%lwt () = Lwt_io.write_line Lwt_io.stderr
      (sprintf "Unexpected arguments: %s" (Yojson.Basic.to_string (`List (List.map argv ~f:(fun x -> `String x)))))
  in
  exit 1

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
