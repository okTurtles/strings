open Core

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
    >>= (fun () -> Unix.exit_immediately 2)
    |> ignore
  )

let main args =
  begin match args with
  | filename::[] ->
    print_endline filename;
    print_endline "----------";
    let flags = Unix.[O_RDONLY; O_NONBLOCK] in
    let%lwt parsed = Lwt_io.with_file ~mode:Input ~flags filename (fun input_channel ->
        Vue.parse input_channel
      )
    in
    print_endline parsed;
    Lwt.return_unit
  | mode -> failwithf "'%s' is not a valid command" (String.concat ~sep:" " mode) ()
  end

let () =
  let open Command.Param in
  let command = Command.basic
      ~summary:"Extract Strings"
      ~readme:(fun () -> "See README.md")
      begin Command.Param.map (anon (sequence ("commands" %: string))) ~f:(fun mode () ->
          Lwt_main.run (
            try%lwt
              main mode
            with
            | (Failure _ as ex) | (Unix.Unix_error _ as ex) | (Exn.Reraised _ as ex) ->
              let message = Utils.Exception.human ex in
              print_endline (sprintf "❌ An error occured:\n%s" message);
              Unix.exit_immediately 1
          )
        )
      end
  in
  Command.run ~version:"0.0.1" ~build_info:"DEV" command
