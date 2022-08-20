(** Like TypeBindings, but the values are monomorphic types. *)
open MonoType
open TypeParameter

(** A map from type parameters to monomorphic types. *)
type mono_type_bindings

val empty_mono_bindings : mono_type_bindings

val mono_bindings_as_list : mono_type_bindings -> (type_parameter * mono_ty) list
