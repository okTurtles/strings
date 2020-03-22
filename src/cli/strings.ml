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

let main = function
| _::files ->
  Lwt_list.iter_s (fun filename ->
    let%lwt () = Lwt_io.write_line Lwt_io.stdout (sprintf "----------\n%s\n----------" filename) in
    let flags = Unix.[O_RDONLY; O_NONBLOCK] in
    let%lwt parsed = Lwt_io.with_file ~mode:Input ~flags filename (fun input_channel ->
        Vue.parse input_channel
      )
    in
    Array.iter parsed ~f:print_endline;
    Lwt.return_unit
  ) files
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
