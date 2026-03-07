open! Core
open Parsing

let%test_unit "js_extraction_basic" =
  let collector = Utils.Collector.create ~path:"test.js" in
  let source = "L('Hello World'); L('Foo Bar');" in
  Js.extract_to_collector collector source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] (List.sort strings ~compare:String.compare) (List.sort ["Hello World"; "Foo Bar"] ~compare:String.compare)

let%test_unit "js_extraction_nested" =
  let collector = Utils.Collector.create ~path:"test.js" in
  let source = "function test() { if (true) { return L('Nested'); } }" in
  Js.extract_to_collector collector source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] strings ["Nested"]

let%test_unit "js_extraction_no_match" =
  let collector = Utils.Collector.create ~path:"test.js" in
  let source = "console.log('Hello');" in
  Js.extract_to_collector collector source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] strings []

let%test_unit "strings_parsing" =
  Lwt_main.run @@ (
  let path = "test.strings" in
  let content = {|
/* Comment */
"Hello" = "Bonjour";
"World" = "Monde";
|} in
  let ic = Lwt_io.of_bytes ~mode:Lwt_io.input @@ Lwt_bytes.of_string content in
  let open Lwt.Syntax in
  let+ table = Strings.parse ~path ic in
  [%test_eq: string option] (Hashtbl.find table "Hello") (Some "Bonjour");
  [%test_eq: string option] (Hashtbl.find table "World") (Some "Monde");
  [%test_eq: string option] (Hashtbl.find table "Missing") None)

let%test_unit "french_strings_parsing" =
  Lwt_main.run @@ (
  let path = "french.strings" in
  let content = {|
/* Accented characters */
"Logout" = "Déconnexion";
"You and {count} others" = "Vous et {count} autres";
"Settings" = "Paramètres";
|} in
  let ic = Lwt_io.of_bytes ~mode:Lwt_io.input @@ Lwt_bytes.of_string content in
  let open Lwt.Syntax in
  let+ table = Strings.parse ~path ic in
  [%test_eq: string option] (Hashtbl.find table "Logout") (Some "Déconnexion");
  [%test_eq: string option] (Hashtbl.find table "You and {count} others") (Some "Vous et {count} autres");
  [%test_eq: string option] (Hashtbl.find table "Settings") (Some "Paramètres"))

let%test_unit "html_extraction" =
  let collector = Utils.Collector.create ~path:"test.html" in
  let source = "<i18n>Hello HTML</i18n>" in
  let on_ok parsed = Parsing.Html.collect collector parsed in
  Parsing.Basic.exec_parser ~on_ok Parsing.Html.parser ~path:"test.html" ~language_name:"HTML" source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] strings ["Hello HTML"]

let%test_unit "pug_extraction" =
  let collector = Utils.Collector.create ~path:"test.pug" in
  let source = "i18n Hello Pug" in
  let string_parsers = Parsing.Basic.make_string_parsers () in
  let on_ok parsed = Parsing.Pug.collect collector parsed in
  Parsing.Basic.exec_parser ~on_ok (Parsing.Pug.parser string_parsers) ~path:"test.pug" ~language_name:"Pug" source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] strings ["Hello Pug"]
