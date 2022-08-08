module U = Unix
open Core

module Exception = struct
  let human = function
  | Failure msg -> msg
  | Lwt.Canceled -> "Timed out."
  | U.Unix_error (c, n, p) ->
    sprintf {s|System Error "%s" during '%s("%s")'|s} (String.uppercase (U.error_message c)) n p
  | unknown -> Exn.to_string unknown

  let full ex = sprintf "%s\n%s" (human ex) (Backtrace.get () |> Backtrace.to_string)
end

let time () =
  let t0 = Time_now.nanoseconds_since_unix_epoch () in
  fun () ->
    let t1 = Time_now.nanoseconds_since_unix_epoch () in
    Int63.((t1 - t0) / of_int 1_000_000)
