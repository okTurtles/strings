open! Core

type kind =
  | Typescript
  | Pug

val init_time : Base.Int63.t Atomic.t

val extract :
  kind -> needle_names:string * string -> source:string -> (string array * string array, string) Result.t

val extract_to_collector : Utils.Collector.t -> kind -> source:string -> unit
