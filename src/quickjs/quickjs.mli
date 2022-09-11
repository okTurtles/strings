open! Core

type kind =
  | Typescript
  | Pug

val init_time : Base.Int63.t ref

val extract : kind -> string -> (string array * string array, string) Result.t Lwt.t

val extract_to_collector : Utils.Collector.t -> kind -> string -> unit Lwt.t
