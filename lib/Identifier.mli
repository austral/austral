type identifier
[@@deriving eq]

val make_ident : string -> identifier
val ident_string : identifier -> string

type module_name
[@@deriving eq]

val make_mod_name : string -> module_name
val mod_name_string : module_name -> string

type qident

(* Make a qualified identifier from the module name, original name, and
   nickname. *)
val make_qident : module_name * identifier * identifier -> qident

val source_module_name : qident -> module_name

val original_name : qident -> identifier

val local_name : qident -> identifier

val qident_debug_name : qident -> string

(* Equality for qualified identifiers only considers the original rather than
   local names *)
val equal_qident : qident -> qident -> bool
