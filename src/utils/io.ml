open! Core
open Eio.Std

let flags = `Or_truncate 0o644

let parser_chunk_size = 4096

let parser_buffer_size = parser_chunk_size * 4

let num_threads = 4

let pool = Domainslib.Task.setup_pool ~num_domains:num_threads ()

let run_in_pool fn x =
  let result, set_result = Promise.create () in
  let _p =
    Domainslib.Task.async pool (fun () ->
      (try Ok (fn x) with
      | ex -> Error ex)
      |> Promise.resolve set_result )
  in
  Promise.await_exn result

let stat path = run_in_pool (fun path -> Core_unix.stat path) path

let read_flow flow = Eio.Buf_read.(parse_exn ~max_size:Int.max_value take_all flow)

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

let directory_exists path =
  match stat path with
  | { st_kind = S_DIR; _ } -> true
  | { st_kind = _; _ } -> failwithf "%s already exists, but is not a directory" path ()
  | exception _ -> false

let mkdir_p env ~dir_name ~perms:perm =
  let (_ : string) =
    Filename.parts dir_name
    |> List.fold ~init:"" ~f:(fun acc part ->
         match acc with
         | "" -> part
         | acc -> (
           let path = Filename.concat acc part in
           directory_exists path |> function
           | true -> path
           | false ->
             Eio.Path.mkdir ~perm Eio.Path.(env#fs / path);
             path ) )
  in
  ()
