open Identifier

val string_explode : string -> char list

val string_implode : char list -> string

val read_file_to_string : string -> string

val remove_char : string -> char -> string

val replace_char : string -> char -> string -> string

(* Check whether two lists of identifiers are the same set. *)
val ident_set_eq : identifier list -> identifier list -> bool
