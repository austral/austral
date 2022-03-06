(** The type of Austral identifiers. *)
type identifier
[@@deriving eq, show]

(** Create an identifier from a string.

    TODO: Verify the identifier satisfies the regex. *)
val make_ident : string -> identifier

(** Return the string representation of an identifier. *)
val ident_string : identifier -> string

(** The type of Austral module names. *)
type module_name
[@@deriving eq, show]

(** Create a module name from a string.

    TODO: Check the string satisfies the regex. *)
val make_mod_name : string -> module_name

(** The string representation of a module name. *)
val mod_name_string : module_name -> string

(** A sourced identifier is a module name plus an identifier from that
    module. *)
type sident

(** Make a sourced identifier. *)
val make_sident : module_name -> identifier -> sident

(** The module the sourced identifier comes from. *)
val sident_module_name : sident -> module_name

(** The sourced identifier's name component. *)
val sident_name : sident -> identifier

(** A qualified identifier is:

    1. The name of the module the identifier was imported from (or the name of
       the current module, if it's a local identifier).

    2. The identifier's original name.

    3. The identifier's local import nickname.
*)
type qident
[@@deriving show]

(** Make a qualified identifier from the module name, original name, and
    nickname. *)
val make_qident : module_name * identifier * identifier -> qident

(** The name of the module this qualified identifier was imported from. *)
val source_module_name : qident -> module_name

(** The qualified identifier's original name, before import nicknames. *)
val original_name : qident -> identifier

(** The qualified identifier's import nickname. *)
val local_name : qident -> identifier

(** A debugging representation of this qualified identifier. *)
val qident_debug_name : qident -> string

(** Equality for qualified identifiers only considers the original rather than
    local names *)
val equal_qident : qident -> qident -> bool

(** Convert a qualified identifier to a sourced identifier. *)
val qident_to_sident : qident -> sident
