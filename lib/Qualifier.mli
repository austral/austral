open Identifier
open Imports

(* Turn an identifier into a qualified identifier, given the import map. *)
val qualify_identifier : import_map -> identifier -> qident

(*val qualify_typespec : import_map -> typespec -> qtypespec*)
