open! Core

module Collector = struct
  type t = {
    filename: string;
    strings: string Queue.t;
    possible_scripts: string Queue.t;
    file_errors: string Queue.t;
  }
  [@@deriving sexp]

  let create ~filename =
    {
      filename;
      strings = Queue.create ();
      possible_scripts = Queue.create ();
      file_errors = Queue.create ();
    }

  let render_errors = function
  | { file_errors; _ } when Queue.is_empty file_errors -> None
  | { file_errors; filename; _ } ->
    let buf = Buffer.create 256 in
    bprintf buf "\nErrors in %s:\n" filename;
    Queue.iter file_errors ~f:(bprintf buf "- %s\n");
    Some (Buffer.contents buf)
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
