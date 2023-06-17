open! Core

type t

val create :
  sw:Eio.Switch.t ->
  num_workers:int ->
  worker_limit:int ->
  ?capacity:int ->
  ?transient:bool ->
  #Eio.Domain_manager.t ->
  t

val run : t -> f:(unit -> 'a) -> ('a, exn) result

val run_exn : t -> f:(unit -> 'a) -> 'a

val terminate : t -> unit

val is_terminating : t -> bool

val is_terminated : t -> bool
