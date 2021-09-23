open Identifier

val string_explode : string -> char list

val string_implode : char list -> string

val read_file_to_string : string -> string

val write_string_to_file : string -> string -> unit

val remove_char : string -> char -> string

val replace_char : string -> char -> string -> string

val remove_leading : string -> int -> string

val parse_hex : string -> int

val parse_bin : string -> int

val parse_oct : string -> int

val parse_ascii_char : string -> int

val process_triple_string : string -> string

(* Check whether two lists of identifiers are the same set. *)
val ident_set_eq : identifier list -> identifier list -> bool
