open! Core

val lowercase : char -> bool

val alphanum : char -> bool

val is_identifier : char -> bool

val is_ws : char -> bool

val is_mlws : char -> bool

val ws : unit Angstrom.t

val ws1 : unit Angstrom.t

val mlws : unit Angstrom.t

val mlws1 : unit Angstrom.t

val take_remaining : string Angstrom.t

val skip_remaining : unit Angstrom.t

val maybe : 'a Angstrom.t -> 'a option Angstrom.t

val make_sq_string : unit -> string Angstrom.t

val make_dq_string : unit -> string Angstrom.t

type string_parsers = {
  sq_string: string Angstrom.t;
  dq_string: string Angstrom.t;
}

val make_string_parsers : unit -> string_parsers

val boundary_parsers : string -> (string, string option) Core.Tuple2.t list Angstrom.t * string Angstrom.t

val block_parser :
  (unit -> 'a Angstrom.t * 'b Angstrom.t) -> Buffer.t -> f:(string -> 'a -> 'c) -> 'c Angstrom.t

val default_error_handler :
  path:string -> language_name:string -> ?unparsed:string -> msg:string -> unit -> 'a

val default_syntax_error_handler : path:string -> language_name:string -> msg:string -> 'a

val exec_parser :
  on_ok:('a -> 'b) ->
  ?on_error:(msg:string -> 'b) ->
  'a Angstrom.t ->
  path:string ->
  language_name:string ->
  string ->
  'b

val exec_parser_eio :
  on_ok:('a -> 'b) ->
  ?on_error:(?unparsed:string -> msg:string -> unit -> 'b) ->
  'a Angstrom.t ->
  path:string ->
  language_name:string ->
  #Eio.Flow.source ->
  'b
