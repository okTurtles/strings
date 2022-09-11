open! Core

type raw =
  | JS of string
  | TS of string
[@@deriving sexp_of]

val parser : Buffer.t -> raw Angstrom.t
