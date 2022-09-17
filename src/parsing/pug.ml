open! Core

type identifier = { parts: string list } [@@deriving yojson, sexp_of]

type selector =
  | Element of identifier
  | Class   of identifier
  | Id      of identifier
[@@deriving yojson, sexp_of]

type argument = {
  prefix: string option;
  identifier: identifier;
  contents: string option;
}
[@@deriving yojson, sexp_of]

type node = {
  selector: selector;
  arguments: argument list;
  text: string option;
  children: node array;
}
[@@deriving yojson, sexp_of]

type line =
  | Node    of node
  | Text    of string list
  | Comment of string list
[@@deriving yojson, sexp_of]

type lines = (int * line) list [@@deriving yojson, sexp_of]

type t = node array [@@deriving yojson, sexp_of]

let collect Utils.Collector.{ strings; possible_scripts; _ } nodes =
  let rec loop { selector; arguments; text; children } =
    (match text, selector with
    | Some s, Element { parts = "i18n" :: _ } -> Queue.enqueue strings s
    | Some s, _ -> Queue.enqueue possible_scripts s
    | None, _ -> ());
    List.iter arguments ~f:(function
      | { contents = None; _ } -> ()
      | { contents = Some s; _ } -> Queue.enqueue possible_scripts s);
    Array.iter children ~f:loop
  in
  Array.iter nodes ~f:loop

let rollup (lines : lines) =
  let rec loop lvl acc_nodes acc_text = function
    | [] as tail ->
      (* End of file *)
      acc_nodes, acc_text, tail
    | (indent, Node node) :: tail when Int.(indent = lvl) ->
      (* Sibling node *)
      let nodes, text, rest = loop Int.(lvl + 2) [] None tail in
      let wrap = Option.map ~f:(String.concat ~sep:" ") text in
      let new_node =
        {
          node with
          children = Array.of_list_rev nodes;
          text = Option.first_some wrap (Option.filter ~f:(fun x -> String.is_empty x |> not) node.text);
        }
      in
      loop lvl (new_node :: acc_nodes) acc_text rest
    | (indent, Text text) :: tail when Int.(indent = lvl) ->
      (* Sibling Text *)
      loop lvl acc_nodes (Some text) tail
    | (indent, Node node) :: tail when Int.(indent = lvl + 2) ->
      (* Child node *)
      loop lvl (node :: acc_nodes) acc_text tail
    | (_, Comment _) :: tail ->
      (* Any Comments *)
      loop lvl acc_nodes acc_text tail
    | (indent, line) :: _ when Int.(indent > lvl) ->
      (* Invalid indentation *)
      failwithf "Invalid indentation level of %d at %s" indent
        (line |> line_to_yojson |> Yojson.Safe.to_string ~std:true)
        ()
    | whole ->
      (* End of siblings, backtrack *)
      acc_nodes, acc_text, whole
  in
  let lvl = List.hd lines |> Option.value_map ~default:0 ~f:fst in
  loop lvl [] None lines |> fst3 |> Array.of_list_rev

let parser =
  let open Angstrom in
  let open Basic in
  let comments = string "//" *> skip_remaining in
  let blank = sep_by comments ws in
  let _blank1 = sep_by1 comments ws1 in
  let mlblank = sep_by comments mlws in
  let mlblank1 = sep_by1 comments mlws1 in
  let pug_string =
    let single_quoted_string = escapable_string_parser ~escape:'\\' ~separator:'\'' in
    let double_quoted_string = escapable_string_parser ~escape:'\\' ~separator:'"' in
    let unquoted_string = take_while1 is_identifier in
    choice [ single_quoted_string; double_quoted_string; unquoted_string ]
  in
  let symbols ll = ll |> List.map ~f:string |> choice in

  let word = take_while1 alphanum in
  let identifier =
    lift2
      (fun s ll -> { parts = s :: ll })
      word
      (many
         (lift2 (sprintf "%s%s")
            (symbols [ "."; "-"; ":"; "#" ])
            (take_while1 alphanum
            <|> lift3 (sprintf "%c%s%c") (char '[') (take_while1 alphanum) (char ']'))))
  in
  let element_selector = identifier >>| fun s -> Element s in
  let class_selector = char '.' *> identifier >>| fun x -> Class x in
  let id_selector = char '#' *> identifier >>| fun x -> Id x in
  let argument =
    lift3
      (fun prefix identifier contents -> { prefix; identifier; contents })
      (maybe (symbols [ ":$"; ":"; "@"; "#" ]))
      (identifier <* blank)
      (maybe (char '=' *> blank *> pug_string))
  in

  let at_least_indent indent = string (String.make indent ' ') in
  let text_wrap = string "|" *> maybe (char ' ') *> take_remaining in
  let comment_start = symbols [ "//-"; "//" ] *> take_remaining in
  let node =
    lift3
      (fun selector arguments text -> { selector; arguments; text; children = [||] })
      (choice [ class_selector; id_selector; element_selector ])
      (maybe (char '(' *> mlblank *> sep_by mlblank1 argument <* mlblank <* char ')')
      >>| Option.value ~default:[])
      (maybe (blank *> take_remaining) >>| function
       | None
        |Some "" ->
         None
       | Some x when String.is_prefix ~prefix:"//" x -> None
       | Some _ as x -> x)
  in
  let line =
    take_while is_ws >>| String.length >>= fun lvl ->
    choice
      [
        (node >>= function
         | { text = Some "."; _ } as node ->
           let indent = at_least_indent Int.(lvl + 2) in
           many (many1 end_of_line *> (indent *> take_remaining)) >>| fun ll ->
           let text = Some (String.concat ~sep:"\n" ll) in
           Node { node with text }
         | x -> return (Node x));
        ( text_wrap >>= fun s ->
          let indent = at_least_indent lvl in
          many (many1 end_of_line *> (indent *> text_wrap)) >>| fun ll ->
          Text (List.filter ~f:(fun x -> String.is_empty x |> not) (s :: ll)) );
        ( comment_start >>= fun s ->
          let indent = at_least_indent Int.(lvl + 2) in
          many (many1 end_of_line *> (indent *> take_remaining)) >>| fun ll -> Comment (s :: ll) );
      ]
    >>| fun x -> lvl, x
  in
  let lines : lines t = sep_by1 (many1 end_of_line) line in

  blank *> many end_of_line *> (lines >>| rollup) <* mlblank <* end_of_input
