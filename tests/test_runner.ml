open! Core
open Parsing

let%test_unit "js_extraction_basic" =
  let collector = Utils.Collector.create ~path:"test.js" in
  let source = "L('Hello World'); L('Foo Bar');" in
  Js.extract_to_collector collector source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list]
    (List.sort strings ~compare:String.compare)
    (List.sort [ "Hello World"; "Foo Bar" ] ~compare:String.compare)

let%test_unit "js_extraction_nested" =
  let collector = Utils.Collector.create ~path:"test.js" in
  let source = "function test() { if (true) { return L('Nested'); } }" in
  Js.extract_to_collector collector source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] strings [ "Nested" ]

let%test_unit "js_extraction_no_match" =
  let collector = Utils.Collector.create ~path:"test.js" in
  let source = "console.log('Hello');" in
  Js.extract_to_collector collector source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] strings []

let%test_unit "strings_parsing" =
  Lwt_main.run
  @@
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
  [%test_eq: string option] (Hashtbl.find table "Missing") None

let%test_unit "french_strings_parsing" =
  Lwt_main.run
  @@
  let path = "french.strings" in
  let content =
    {|
/* Accented characters */
"Logout" = "Déconnexion";
"You and {count} others" = "Vous et {count} autres";
"Settings" = "Paramètres";
|}
  in
  let ic = Lwt_io.of_bytes ~mode:Lwt_io.input @@ Lwt_bytes.of_string content in
  let open Lwt.Syntax in
  let+ table = Strings.parse ~path ic in
  [%test_eq: string option] (Hashtbl.find table "Logout") (Some "Déconnexion");
  [%test_eq: string option] (Hashtbl.find table "You and {count} others") (Some "Vous et {count} autres");
  [%test_eq: string option] (Hashtbl.find table "Settings") (Some "Paramètres")

let%test_unit "html_extraction" =
  let collector = Utils.Collector.create ~path:"test.html" in
  let source = "<i18n>Hello HTML</i18n>" in
  let on_ok parsed = Parsing.Html.collect collector parsed in
  Parsing.Basic.exec_parser ~on_ok Parsing.Html.parser ~path:"test.html" ~language_name:"HTML" source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] strings [ "Hello HTML" ]

let%test_unit "pug_extraction" =
  let collector = Utils.Collector.create ~path:"test.pug" in
  let source = "i18n Hello Pug" in
  let string_parsers = Parsing.Basic.make_string_parsers () in
  let on_ok parsed = Parsing.Pug.collect collector parsed in
  Parsing.Basic.exec_parser ~on_ok (Parsing.Pug.parser string_parsers) ~path:"test.pug"
    ~language_name:"Pug" source;
  let strings = Queue.to_list collector.strings in
  [%test_eq: string list] strings [ "Hello Pug" ]

let extract_astro source =
  let collector = Utils.Collector.create ~path:"test.astro" in
  let on_ok parsed = Parsing.Astro.collect collector parsed in
  Parsing.Basic.exec_parser ~on_ok (Parsing.Astro.parser ()) ~path:"test.astro" ~language_name:"Astro"
    source;
  collector

let%test_unit "astro_i18n_extraction" =
  let collector = extract_astro "<I18n is:raw>Hello Astro</I18n>" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Hello Astro" ];
  [%test_eq: int] (Queue.length collector.warnings) 0

let%test_unit "astro_lowercase_i18n_extraction" =
  let collector = extract_astro "<i18n>Hello Astro</i18n>" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Hello Astro" ]

let%test_unit "astro_missing_is_raw_warning" =
  let collector = extract_astro "<I18n>Hello {name}!</I18n>" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Hello {name}!" ];
  [%test_eq: int] (Queue.length collector.warnings) 1

let%test_unit "astro_frontmatter_and_expression_scripts" =
  let collector =
    extract_astro "---\nconst a = L('From Frontmatter')\n---\n<h1>{L('From Expression')}</h1>"
  in
  let scripts = Queue.to_list collector.possible_scripts in
  [%test_eq: bool] (List.exists scripts ~f:(String.is_substring ~substring:"L('From Frontmatter')")) true;
  [%test_eq: bool] (List.exists scripts ~f:(String.is_substring ~substring:"L('From Expression')")) true

let%test_unit "astro_args_and_script_block" =
  let collector =
    extract_astro
      "<I18n is:raw args={{ ...LTags(\"strong\") }}>Hi</I18n><script>L('From Script')</script>"
  in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Hi" ];
  let scripts = Queue.to_list collector.possible_scripts in
  [%test_eq: bool] (List.exists scripts ~f:(String.is_substring ~substring:"LTags(\"strong\")")) true;
  [%test_eq: bool] (List.exists scripts ~f:(String.is_substring ~substring:"L('From Script')")) true
