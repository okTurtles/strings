open! Core

module type Parser = sig
  type t [@@deriving sexp_of]

  val collect : Utils.Collector.t -> t -> unit

  val parser : t Angstrom.t
end
