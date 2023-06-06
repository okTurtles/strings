open! Core

type t [@@deriving sexp_of]

val collect : Utils.Collector.t -> t -> unit

val parser : Basic.string_parsers -> t Angstrom.t
