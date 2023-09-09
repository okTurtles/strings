open! Core

let flags = `Or_truncate 0o644

let num_js_workers = 4

let num_cores = 4

let traversal_jobs_per_core = 4

let num_systhreads = 2

let stat wp path : Core_unix.stats = Eio.Workpool.submit_exn wp (fun () -> Core_unix.stat path)

let load_flow flow =
  let open Eio in
  (* Taken from [Eio.Path.load] *)
  try
    let size = File.size flow in
    if Optint.Int63.(compare size (of_int Sys.max_string_length)) = 1 then raise @@ Fs.err File_too_large;
    let buf = Cstruct.create (Optint.Int63.to_int size) in
    let rec loop buf got =
      match Flow.single_read flow buf with
      | n -> loop (Cstruct.shift buf n) (n + got)
      | exception End_of_file -> got
    in
    let got = loop buf 0 in
    Cstruct.to_string ~len:got buf
  with
  | Exn.Io _ as ex ->
    let bt = Stdlib.Printexc.get_raw_backtrace () in
    Exn.reraise_with_context ex bt "loading flow"

let directory_exists wp path =
  match stat wp path with
  | { st_kind = S_DIR; _ } -> true
  | { st_kind = _; _ } -> failwithf "%s already exists, but is not a directory" path ()
  | exception _ -> false

let mkdir_p env wp ~dir_name ~perms:perm =
  let (_ : string) =
    Filename.parts dir_name
    |> List.fold ~init:"" ~f:(fun acc part ->
         match acc with
         | "" -> part
         | acc -> (
           let path = Filename.concat acc part in
           directory_exists wp path |> function
           | true -> path
           | false ->
             Eio.Path.mkdir ~perm Eio.Path.(env#fs / path);
             path ) )
  in
  ()
