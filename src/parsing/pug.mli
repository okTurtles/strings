open! Core

type identifier = { parts: string list } [@@deriving yojson, sexp]

type selector =
  | Element of identifier
  | Class   of identifier
  | Id      of identifier
[@@deriving yojson, sexp]

type argument = {
  prefix: string option;
  identifier: identifier;
  contents: string option;
}
[@@deriving yojson, sexp]

type node = {
  selector: selector;
  arguments: argument list;
  text: string option;
  children: node array;
}
[@@deriving yojson, sexp]

type t = node array [@@deriving yojson, sexp]

val parser : node array Angstrom.t
