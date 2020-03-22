open Core_kernel

type identifier = {
  name: string;
} [@@deriving yojson, sexp]

type selector_part =
| Element of identifier
| Class of identifier
| Id of identifier
[@@deriving yojson, sexp]

type argument = {
  prefix: string option;
  identifier: identifier;
  contents: string option;
} [@@deriving yojson, sexp]

type node = {
  parts: selector_part list;
  arguments: argument list;
  text: string option;
  children: node list;
} [@@deriving yojson, sexp]

type line =
| Node of node
| Text of string list
| Comment of string list
[@@deriving yojson, sexp]

type lines = (int * line) list [@@deriving yojson, sexp]
type nodes = node list [@@deriving yojson, sexp]

let rollup (lines : lines) =
  let rec loop lvl acc_nodes acc_text = function
  | ([] as tail) ->
    (* End of file *)
    acc_nodes, acc_text, tail

  | (indent, Node node)::tail when Int.(indent = lvl) ->
    (* Sibling node *)
    let nodes, text, rest = loop Int.(lvl + 2) [] None tail in
    let wrap = Option.map ~f:(String.concat ~sep:" ") text in
    let new_node = { node with
      children = List.rev nodes;
      text = (Option.first_some wrap (Option.filter ~f:(fun x -> String.is_empty x |> not) node.text));
    }
    in
    loop lvl (new_node::acc_nodes) acc_text rest

  | (indent, Text text)::tail when Int.(indent = lvl) ->
    (* Sibling Text *)
    loop lvl acc_nodes (Some text) tail

  | (indent, Node node)::tail when Int.(indent = (lvl + 2)) ->
    (* Child node *)
    loop lvl (node::acc_nodes) acc_text tail

  | (_, Comment _)::tail ->
    (* Any Comments *)
    loop lvl acc_nodes acc_text tail

  | (indent, line)::_ when Int.(indent > lvl) ->
    (* Invalid indentation *)
    failwithf "Invalid indentation level of %d at %s"
      indent (line |> line_to_yojson |> Yojson.Safe.to_string ~std:true) ()

  | whole ->
    (* End of siblings, backtrack *)
    acc_nodes, acc_text, whole
  in
  let lvl = List.hd lines |> Option.value_map ~default:0 ~f:fst in
  loop lvl [] None lines |> fst3 |> List.rev

let parser =
  let open Angstrom in
  let open Basic in

  let comments = string "//" *> skip_remaining in
  let blank = (sep_by comments ws) in
  let _blank1 = (sep_by1 comments ws1) in
  let mlblank = (sep_by comments mlws) in
  let mlblank1 = (sep_by1 comments mlws1) in
  let single_quoted_string = char '\'' *> (escapable_string_parser ~escape:'\\' ~separator:'\'') <* char '\'' in
  let symbols ll = ll |> List.map ~f:string |> choice in

  let word = take_while1 alphanum in
  let identifier =
    lift2 double word
      (many (lift2 (sprintf "%s%s")
            (symbols ["."; "-"; ":"; "#"])
            ((take_while1 alphanum) <|> (lift3 (sprintf "%c%s%c") (char '[') (take_while1 alphanum) (char ']')))
          ))
    >>| (fun (s, ll) -> { name = String.concat (s::ll) })
  in
  let element_selector = identifier >>| (fun s -> Element s) in
  let class_selector = char '.' *> identifier >>| (fun x -> Class x) in
  let id_selector = char '#' *> identifier >>| (fun x -> Id x) in
  let argument = (lift3 triple
      (maybe (symbols [":$"; ":"; "@"; "#"]))
      (identifier)
      (maybe (char '=' *> blank *> single_quoted_string))
  )
    >>| (fun (prefix, identifier, contents) -> { prefix; identifier; contents })
  in

  let at_least_indent indent = string (String.make indent ' ') in
  let text_wrap = string "|" *> maybe (char ' ') *> take_remaining in
  let comment_start = symbols ["//-"; "//"] *> take_remaining in
  let node = (
    (lift3 triple
        (many1 (choice [class_selector; id_selector; element_selector]))
        (maybe (char '(' *> mlblank *> (sep_by mlblank1 argument) <* mlblank <* char ')') >>| (Option.value ~default:[]))
        (maybe (blank *> take_remaining)
          >>| function
          | None | Some "" -> None
          | Some x when String.is_prefix ~prefix:"//" x -> None
          | (Some _ as x) -> x
        )
    )
  )
    >>| (fun (parts, arguments, text) -> { parts; arguments; text; children = [] })
  in
  let line = (take_while is_ws >>| String.length) >>= (fun lvl ->
      choice [
        node >>= (function
        | ({ text = Some "."; _} as node) ->
          let indent = at_least_indent Int.(lvl + 2) in
          many ((many1 end_of_line) *> (indent *> take_remaining))
          >>| (fun ll ->
            let text = Some (String.concat ~sep: "\n" ll) in
            Node { node with text }
          )
        | x -> return (Node x)
        );
        text_wrap >>= (fun s ->
          let indent = at_least_indent lvl in
          many ((many1 end_of_line) *> (indent *> text_wrap))
          >>| (fun ll -> Text (List.filter ~f:(fun x -> String.is_empty x |> not) (s::ll)))
        );
        comment_start >>= (fun s ->
          let indent = at_least_indent Int.(lvl + 2) in
          many ((many1 end_of_line) *> (indent *> take_remaining))
          >>| (fun ll -> Comment (s::ll))
        )
      ]
      >>| (fun x -> lvl, x)
    )
  in
  let lines : lines t = sep_by1 (many1 end_of_line) line in

  let pug_begin = (string {s|<template lang="pug">|s} <|> string {s|<template lang='pug'>|s}) <* blank <* (many end_of_line) in
  let pug_end = string {s|</template>|s} in
  pug_begin *> (lines >>| rollup) <* mlblank <* pug_end
