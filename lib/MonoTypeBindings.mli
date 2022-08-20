(** Like TypeBindings, but the values are monomorphic types. *)

(** A map from type parameters to monomorphic types. *)
type mono_type_bindings

val empty_mono_bindings : mono_type_bindings
