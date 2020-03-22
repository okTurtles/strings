open Core_kernel

module Exception = struct
  let human = function
  | Failure msg ->
    msg

  | Lwt.Canceled ->
    "Timed out."

  | Unix.Unix_error (c, n, p) ->
    sprintf "System Error '%s' during '%s(%s)'" (String.uppercase (Unix.error_message c)) n p

  | unknown ->
    Exn.to_string unknown

  let full ex = sprintf "%s\n%s" (human ex) (Backtrace.get () |> Backtrace.to_string)

end
