open! Core

let human = function
| Failure msg -> msg
| Lwt.Canceled -> "Timed out."
| Core_unix.Unix_error (c, n, p) ->
  sprintf {s|System Error "%s" during '%s("%s")'|s} (String.uppercase (Core_unix.Error.message c)) n p
| unknown -> Exn.to_string unknown

let full ex = sprintf !"%s\n%{Backtrace}" (human ex) (Backtrace.get ())
