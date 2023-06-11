open! Core

let rec human = function
| Eio.Io _ as exn -> Format.asprintf "%a" Eio.Exn.pp exn
| Eio.Exn.Multiple ll ->
  let buf = Buffer.create 4096 in
  List.iter ll ~f:(fun (exn, _bt) -> bprintf buf !"%{human}\n\n" exn);
  Buffer.contents buf
| Failure msg -> msg
| Core_unix.Unix_error (c, n, p) ->
  sprintf {s|System Error "%s" during '%s("%s")'|s} (String.uppercase (Core_unix.Error.message c)) n p
| unknown -> Exn.to_string unknown

let full ex = sprintf !"%s\n%{Backtrace}" (human ex) (Backtrace.get ())
