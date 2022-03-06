open Span

type error
type error_data

val error_filename : error -> string option
val error_text : error_data -> string
val error_title : error_data -> string

val render_error : error -> string option -> string

exception Austral_error of error

val err : string -> 'a
val raise_parse_error : span -> 'a
val raise_type_mismatch_error : string -> string -> 'a

val adorn_error_with_span : span -> (unit -> 'a) -> 'a
