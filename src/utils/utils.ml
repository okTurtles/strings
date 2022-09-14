open! Core

module Collector = struct
  type t = {
    path: string;
    strings: string Queue.t;
    possible_scripts: string Queue.t;
    file_errors: string Queue.t;
  }
  [@@deriving sexp]

  let create ~path =
    { path; strings = Queue.create (); possible_scripts = Queue.create (); file_errors = Queue.create () }

  let render_errors { file_errors; path; _ } =
    match Queue.length file_errors with
    | 0 -> None
    | 1 ->
      let buf = Buffer.create 256 in
      bprintf buf "\n❌ 1 error in %s: %s" path (Queue.get file_errors 0);
      Some (Buffer.contents buf)
    | len ->
      let buf = Buffer.create 256 in
      bprintf buf "\n❌ %d errors in %s:\n" len path;
      Queue.iter file_errors ~f:(bprintf buf "- %s\n");
      Some (Buffer.contents buf)

  let blit_transfer ~src ~dst =
    Queue.blit_transfer ~src:src.strings ~dst:dst.strings ();
    Queue.blit_transfer ~src:src.possible_scripts ~dst:dst.possible_scripts ();
    Queue.blit_transfer ~src:src.file_errors ~dst:dst.file_errors ()
end

module Exception = struct
  let human = function
  | Failure msg -> msg
  | Lwt.Canceled -> "Timed out."
  | Core_unix.Unix_error (c, n, p) ->
    sprintf {s|System Error "%s" during '%s("%s")'|s} (String.uppercase (Core_unix.Error.message c)) n p
  | unknown -> Exn.to_string unknown

  let full ex = sprintf "%s\n%s" (human ex) (Backtrace.get () |> Backtrace.to_string)
end

let time () =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  fun () ->
    let t1 = Time_now.nanoseconds_since_unix_epoch () in
    Int63.((t1 - t0) / of_int 1_000_000)
