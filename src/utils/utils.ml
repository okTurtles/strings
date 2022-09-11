open! Core

module Failed = struct
  type t = {
    filename: string;
    message: string;
  }

  let to_string { filename; message } = sprintf "Parsing error in %s:\n%s" filename message
end

module Parsed = struct
  type t = {
    strings: string array;
    possible_scripts: string array;
  }
  [@@deriving sexp]
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
