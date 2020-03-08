open Core_kernel

type language =
| Pug of Pug.lines
| Js of Js.raw
| Css of Css.raw
[@@deriving sexp, yojson]

type languages = language list [@@deriving sexp, yojson]

let parse input_channel =
  let open Angstrom in
  let open Basic in

  let languages = choice [
      Pug.parser >>| (fun x -> Pug x);
      Js.parser >>| (fun x -> Js x);
      Css.parser >>| (fun x -> Css x);
    ]
  in
  let parser = lift2 double (sep_by mlws1 languages) (mlws *> take_while (fun _ -> true)) in

  let%lwt _unconsumed, result = Angstrom_lwt_unix.parse parser input_channel in
  begin match result with
  (* | Ok (parsed, "") -> Lwt.return (languages_to_yojson parsed |> Yojson.Safe.pretty_to_string) *)
  | Ok (parsed, "") -> Lwt.return (sexp_of_languages parsed |> Sexp.to_string_hum)
  | Ok (_, unparsed) ->
    failwithf "Could not process data starting at:\n%s"
      (Yojson.Basic.to_string (`String (String.slice unparsed 0 Int.(min 20 (String.length unparsed))))) ()
  | Error err -> failwithf "Syntax Error: %s" err ()
  end
