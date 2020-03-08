open Core

module Exception = struct
  let rec human = function
  | Failure msg ->
    msg

  | Lwt.Canceled ->
    "Timed out."

  | Unix.Unix_error (c, n, p) ->
    sprintf "System Error '%s' during '%s(%s)'" (String.uppercase (Unix.Error.message c)) n p

  | Exn.Reraised (msg, ex) ->
    sprintf "%s\n%s" msg (human ex)

  | unknown ->
    Exn.to_string unknown

  let full ex = sprintf "%s\n%s" (human ex) (Backtrace.get () |> Backtrace.to_string)

end
