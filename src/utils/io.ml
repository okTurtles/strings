open! Core
open Lwt.Infix
open Lwt.Syntax

let read_flags = Core_unix.[ O_RDONLY; O_NONBLOCK ]

let write_flags = Core_unix.[ O_WRONLY; O_NONBLOCK; O_TRUNC; O_CREAT ]

let directory_exists path =
  Lwt.try_bind
    (fun () -> Lwt_unix.stat path)
    (function
      | { st_kind = S_DIR; _ } -> Lwt.return_true
      | { st_kind = _; _ } -> failwithf "%s already exists, but is not a directory" path ())
    (fun _ -> Lwt.return_false)

let mkdir_p ~dir_name ~perms =
  let+ (_ : string) =
    Filename.parts dir_name
    |> Lwt_list.fold_left_s
         (fun acc part ->
           match acc with
           | "" -> Lwt.return part
           | acc -> (
             let path = Filename.concat acc part in
             Lwt_unix.file_exists path >>= function
             | true -> Lwt.return path
             | false ->
               let+ () = Lwt_unix.mkdir path perms in
               path ))
         ""
  in
  ()
