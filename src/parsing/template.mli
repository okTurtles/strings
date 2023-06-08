open! Core

type raw =
  | HTML of string
  | PUG of string
[@@deriving sexp_of]

val parser : Buffer.t -> raw Angstrom.t
