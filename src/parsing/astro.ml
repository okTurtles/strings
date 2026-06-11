open! Core

type i18n_block = {
  tag: string;
  is_raw: bool;
  args: string option;
  text: string option;
}
[@@deriving sexp_of]

type segment =
  | Frontmatter of string
  | Script of string
  | Expression of string
  | I18n of i18n_block
[@@deriving sexp_of]

type t = segment list [@@deriving sexp_of]

(* Astro expressions are wrapped in parentheses before being handed to the TypeScript
   extractor so that object literals such as [args={{ name: x }}] parse as expressions
   instead of block statements *)
let wrap_expression s = sprintf "(%s)" s

let parsers () =
  let open Angstrom in
  let open Basic in
  let buf = Buffer.create 256 in

  (* Character-level scanners. They all append the consumed characters to [buf] so that
     brace matching ignores braces inside strings, template literals, and comments. *)
  let rec braces depth =
    any_char >>= fun c ->
    match c with
    | '}' when depth = 0 -> return ()
    | '}' ->
      Buffer.add_char buf c;
      braces (depth - 1)
    | '{' ->
      Buffer.add_char buf c;
      braces (depth + 1)
    | ('\'' | '"') as q ->
      Buffer.add_char buf c;
      in_string q false >>= fun () -> braces depth
    | '`' ->
      Buffer.add_char buf c;
      in_template false >>= fun () -> braces depth
    | '/' ->
      Buffer.add_char buf c;
      maybe_comment () >>= fun () -> braces depth
    | c ->
      Buffer.add_char buf c;
      braces depth
  and in_string q escaped =
    any_char >>= fun c ->
    Buffer.add_char buf c;
    match c, escaped with
    | '\\', false -> in_string q true
    | c, false when Char.(c = q) -> return ()
    | _ -> in_string q false
  and in_template escaped =
    any_char >>= fun c ->
    Buffer.add_char buf c;
    match c, escaped with
    | '\\', false -> in_template true
    | '`', false -> return ()
    | '$', false -> (
      peek_char >>= function
      | Some '{' ->
        advance 1 >>= fun () ->
        Buffer.add_char buf '{';
        braces 0 >>= fun () ->
        Buffer.add_char buf '}';
        in_template false
      | _ -> in_template false )
    | _ -> in_template false
  and maybe_comment () =
    peek_char >>= function
    | Some '/' -> take_remaining >>| Buffer.add_string buf
    | Some '*' ->
      advance 1 >>= fun () ->
      Buffer.add_char buf '*';
      in_block_comment ()
    | _ -> return ()
  and in_block_comment () =
    any_char >>= fun c ->
    Buffer.add_char buf c;
    match c with
    | '*' -> (
      peek_char >>= function
      | Some '/' -> advance 1 >>| fun () -> Buffer.add_char buf '/'
      | _ -> in_block_comment () )
    | _ -> in_block_comment ()
  in

  let braced_value =
    char '{' >>= fun _ ->
    Buffer.clear buf;
    braces 0 >>| fun () -> Buffer.contents buf
  in
  let template_value =
    char '`' >>= fun _ ->
    Buffer.clear buf;
    Buffer.add_char buf '`';
    in_template false >>| fun () -> Buffer.contents buf
  in
  let take_past stop =
    let rec go () =
      stop *> return ()
      >>| (fun () -> Buffer.contents buf)
      <|> ( any_char >>= fun c ->
            Buffer.add_char buf c;
            go () )
    in
    return () >>= fun () ->
    Buffer.clear buf;
    go ()
  in
  let skip_past stop =
    let rec go () = stop *> return () <|> (any_char >>= fun _ -> go ()) in
    go ()
  in

  let tag_boundary =
    peek_char >>= function
    | Some c when is_identifier c -> fail "Not a tag boundary"
    | _ -> return ()
  in

  (* Leniently consumes the remainder of an open tag up to and including its closing '>'.
     Quoted attribute values are literal text in Astro and are skipped; braced and
     template-literal values are captured as expressions. *)
  let tag_rest =
    let rec go exprs =
      any_char >>= function
      | '>' -> return (List.rev exprs, false)
      | '/' -> (
        peek_char >>= function
        | Some '>' -> advance 1 *> return (List.rev exprs, true)
        | _ -> go exprs )
      | '"' -> take_till (Char.( = ) '"') *> advance 1 *> go exprs
      | '\'' -> take_till (Char.( = ) '\'') *> advance 1 *> go exprs
      | '`' ->
        Buffer.clear buf;
        Buffer.add_char buf '`';
        in_template false >>= fun () -> go (Buffer.contents buf :: exprs)
      | '{' ->
        Buffer.clear buf;
        braces 0 >>= fun () -> go (Buffer.contents buf :: exprs)
      | _ -> go exprs
    in
    go []
  in

  let script_block =
    string "<script" *> tag_boundary *> tag_rest >>= fun (_, self_closing) ->
    match self_closing with
    | true -> return []
    | false -> take_past (string "</script" *> mlws *> char '>') >>| fun body -> [ Script body ]
  in
  let style_block =
    string "<style" *> tag_boundary *> tag_rest >>= fun (_, self_closing) ->
    match self_closing with
    | true -> return []
    | false -> skip_past (string "</style" *> mlws *> char '>') *> return []
  in

  let attr_name =
    take_while1 (fun c -> is_identifier c || Char.(c = ':') || Char.(c = '.') || Char.(c = '@'))
  in
  let plain_quoted q = char q *> take_till (Char.( = ) q) <* advance 1 in
  let attr_value =
    peek_char_fail >>= function
    | '"' -> plain_quoted '"' >>| fun s -> `Literal s
    | '\'' -> plain_quoted '\'' >>| fun s -> `Literal s
    | '{' -> braced_value >>| fun s -> `Expr s
    | '`' -> template_value >>| fun s -> `Expr s
    | _ ->
      take_while1 (fun c -> (not (is_mlws c)) && (not Char.(c = '>')) && not Char.(c = '/')) >>| fun s ->
      `Literal s
  in
  let attr = lift2 Tuple2.create attr_name (maybe (mlws *> char '=' *> mlws *> attr_value)) in

  let i18n_block =
    char '<' *> (string "I18n" <|> string "i18n") <* tag_boundary >>= fun tag ->
    many (mlws1 *> attr) <* mlws >>= fun attrs ->
    let is_raw = List.exists attrs ~f:(fun (name, _) -> String.(name = "is:raw")) in
    let args =
      List.find_map attrs ~f:(function
        | "args", Some (`Expr s) -> Some s
        | _ -> None )
    in
    let extra_exprs =
      List.filter_map attrs ~f:(function
        | "args", _ -> None
        | _, Some (`Expr s) -> Some (Expression s)
        | _ -> None )
    in
    string "/>" *> return (I18n { tag; is_raw; args; text = None } :: extra_exprs)
    <|> ( char '>' *> take_past (string ("</" ^ tag) *> mlws *> char '>') >>| fun text ->
          I18n { tag; is_raw; args; text = Some text } :: extra_exprs )
  in

  let generic_tag =
    char '<'
    *> choice
         [
           char '/' *> take_till (Char.( = ) '>') *> advance 1 *> return [];
           char '!' *> take_till (Char.( = ) '>') *> advance 1 *> return [];
           (satisfy alphanum *> tag_rest >>| fun (exprs, _) -> List.map exprs ~f:(fun s -> Expression s));
         ]
  in

  let element_chunk =
    choice
      [
        string "<!--" *> skip_past (string "-->") *> return [];
        script_block;
        style_block;
        i18n_block;
        generic_tag;
        (braced_value >>| fun s -> [ Expression s ]);
        any_char *> return [];
      ]
  in

  let frontmatter =
    let fence = string "---" in
    let rec go acc =
      fence *> return (String.concat ~sep:"\n" (List.rev acc))
      <|> ( take_remaining >>= fun line ->
            end_of_line *> go (line :: acc)
            <|> end_of_input *> return (String.concat ~sep:"\n" (List.rev (line :: acc))) )
    in
    fence *> ws *> end_of_line *> go []
  in

  let body = many element_chunk >>| List.concat in
  let full =
    lift2
      (fun fm body ->
        match fm with
        | None -> body
        | Some s -> Frontmatter s :: body)
      (mlws *> maybe frontmatter)
      body
    <* end_of_input
  in
  body, full

let parser () = snd (parsers ())

let body_parser () = fst (parsers ())

let collect Utils.Collector.{ strings; possible_scripts; warnings; _ } (segments : t) =
  let enqueue_expression source =
    if not (String.is_empty (String.strip source))
    then Queue.enqueue possible_scripts (wrap_expression source)
  in
  (* Astro expressions may contain JSX (conditional/mapped rendering). The TSX pass over
     the expression finds L() calls, but <I18n> blocks nested in JSX would be lost, so
     expressions are re-scanned with the lenient body parser for nested I18n segments. *)
  let rec scan_expression_for_i18n source =
    if String.is_substring source ~substring:"<I18n" || String.is_substring source ~substring:"<i18n"
    then
      Angstrom.parse_string ~consume:All (body_parser ()) source
      |> Result.iter ~f:(fun nested ->
           List.iter nested ~f:(function
             | I18n block -> handle_i18n block
             | Expression nested_source -> scan_expression_for_i18n nested_source
             | Frontmatter _
              |Script _ ->
               () ) )
  and handle_i18n { tag; is_raw; args; text } =
    Option.iter args ~f:(fun args ->
      enqueue_expression args;
      scan_expression_for_i18n args );
    Option.iter text ~f:(fun text ->
      let key = String.strip text in
      if not (String.is_empty key)
      then (
        Queue.enqueue strings key;
        if (not is_raw) && String.contains key '{'
        then
          Queue.enqueue warnings
            (sprintf
               "<%s> contains placeholders but is missing the is:raw directive: %s. Add it like this: \
                <%s is:raw ...> (otherwise Astro evaluates the placeholders as expressions)"
               tag
               (Yojson.Basic.to_string (`String key))
               tag ) ) )
  in
  List.iter segments ~f:(function
    | Frontmatter source
     |Script source ->
      Queue.enqueue possible_scripts source
    | Expression source ->
      enqueue_expression source;
      scan_expression_for_i18n source
    | I18n block -> handle_i18n block )

(* Tests *)

let test_collect source =
  let collector = Utils.Collector.create ~path:"test.astro" in
  Basic.exec_parser
    ~on_ok:(fun parsed -> collect collector parsed)
    (parser ()) ~path:"test.astro" ~language_name:"Astro" source;
  collector

let%test_unit "astro: i18n slot text" =
  let collector = test_collect "<I18n is:raw>Logout</I18n>" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Logout" ];
  [%test_eq: int] (Queue.length collector.warnings) 0

let%test_unit "astro: lowercase i18n tag" =
  let collector = test_collect "<i18n>Logout</i18n>" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Logout" ]

let%test_unit "astro: multiline slot text is stripped like the HTML path" =
  let collector = test_collect "<I18n is:raw>\n  Hello {name}!\n</I18n>" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Hello {name}!" ];
  [%test_eq: int] (Queue.length collector.warnings) 0

let%test_unit "astro: missing is:raw warning" =
  let collector = test_collect "<I18n>Hello {name}!</I18n>" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Hello {name}!" ];
  [%test_eq: int] (Queue.length collector.warnings) 1

let%test_unit "astro: no warning without placeholders" =
  let collector = test_collect "<I18n>Hello!</I18n>" in
  [%test_eq: int] (Queue.length collector.warnings) 0

let%test_unit "astro: frontmatter" =
  let collector = test_collect "---\nconst title = L('Create a group')\n---\n<h1>ok</h1>" in
  [%test_eq: string list]
    (Queue.to_list collector.possible_scripts)
    [ "const title = L('Create a group')" ]

let%test_unit "astro: body expression" =
  let collector = test_collect "<h1>{L('Welcome')}</h1>" in
  [%test_eq: string list] (Queue.to_list collector.possible_scripts) [ "(L('Welcome'))" ]

let%test_unit "astro: attribute expression" =
  let collector = test_collect "<div title={L('Hi')} class=\"big {not_code}\">x</div>" in
  [%test_eq: string list] (Queue.to_list collector.possible_scripts) [ "(L('Hi'))" ]

let%test_unit "astro: args object with spread and arrow function" =
  let collector =
    test_collect
      "<I18n is:raw args={{ name: groupName, f: () => x, ...LTags(\"strong\") }}>Delete {name}</I18n>"
  in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Delete {name}" ];
  [%test_eq: string list]
    (Queue.to_list collector.possible_scripts)
    [ "({ name: groupName, f: () => x, ...LTags(\"strong\") })" ]

let%test_unit "astro: braces inside template literals and strings" =
  let collector = test_collect "{`a ${ { b: 1 }.b } c`}<p>{'}'}</p>" in
  [%test_eq: string list]
    (Queue.to_list collector.possible_scripts)
    [ "(`a ${ { b: 1 }.b } c`)"; "('}')" ]

let%test_unit "astro: script block" =
  let collector = test_collect "<script>\nconsole.log(L('Hey'))\n</script>" in
  [%test_eq: string list] (Queue.to_list collector.possible_scripts) [ "\nconsole.log(L('Hey'))\n" ]

let%test_unit "astro: style block is ignored" =
  let collector = test_collect "<style>\n.a { color: red }\n</style>" in
  [%test_eq: string list] (Queue.to_list collector.possible_scripts) []

let%test_unit "astro: html comments are ignored" =
  let collector = test_collect "<!-- <I18n>Nope</I18n> {nope} --><I18n>Yes</I18n>" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Yes" ]

let%test_unit "astro: self-closing I18n" =
  let collector = test_collect "<I18n is:raw args={{ a: 1 }} />" in
  [%test_eq: string list] (Queue.to_list collector.strings) [];
  [%test_eq: string list] (Queue.to_list collector.possible_scripts) [ "({ a: 1 })" ]

let%test_unit "astro: full document" =
  let source =
    {|---
import I18n from '../components/I18n.astro'
const title = L('Create a group')
---

<h1>{L('Welcome to Group Income')}</h1>

<I18n is:raw>Logout</I18n>

<I18n is:raw args={{ name: groupName, ...LTags("strong") }}>
  Yes, I want to {strong_}delete {name} permanently{_strong}.
</I18n>
|}
  in
  let collector = test_collect source in
  [%test_eq: string list]
    (List.sort ~compare:String.compare (Queue.to_list collector.strings))
    [ "Logout"; "Yes, I want to {strong_}delete {name} permanently{_strong}." ];
  [%test_eq: int] (Queue.length collector.warnings) 0;
  [%test_eq: bool]
    (List.exists (Queue.to_list collector.possible_scripts) ~f:(fun s ->
       String.is_substring s ~substring:"L('Create a group')" ) )
    true

let%test_unit "astro: I18n nested in a mapped JSX expression" =
  let collector = test_collect "{items.map(item => <I18n is:raw>Mapped string</I18n>)}" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Mapped string" ];
  [%test_eq: int] (Queue.length collector.warnings) 0

let%test_unit "astro: I18n nested in a conditional JSX expression" =
  let collector = test_collect "{cond && <p><I18n>Hello {name}!</I18n></p>}" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Hello {name}!" ];
  [%test_eq: int] (Queue.length collector.warnings) 1

let%test_unit "astro: I18n nested two expression levels deep" =
  let collector = test_collect "{cond && <div>{items.map(() => <i18n>Deep</i18n>)}</div>}" in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Deep" ]

let%test_unit "astro: nested I18n args expression is captured" =
  let collector =
    test_collect "{cond && <I18n is:raw args={{ ...LTags(\"b\") }}>Nested {b_}x{_b}</I18n>}"
  in
  [%test_eq: string list] (Queue.to_list collector.strings) [ "Nested {b_}x{_b}" ];
  [%test_eq: bool]
    (List.exists (Queue.to_list collector.possible_scripts) ~f:(fun s ->
       String.is_substring s ~substring:"LTags(\"b\")" ) )
    true

let%test_unit "astro: expression without I18n is not rescanned into strings" =
  let collector = test_collect "{cond && <p>{L('Inner JSX string')}</p>}" in
  [%test_eq: string list] (Queue.to_list collector.strings) [];
  [%test_eq: string list]
    (Queue.to_list collector.possible_scripts)
    [ "(cond && <p>{L('Inner JSX string')}</p>)" ]
