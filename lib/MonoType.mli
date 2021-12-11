(** The monomorphic type system. *)
open Identifier
open Type

(** The ID of a monomorph. *)
type mono_id
[@@deriving eq]

(** Represents whether a monomorph has been instantiated. *)
type mono_status =
  | NotInstantiated
  | Instantiated

(** A monomorphic, or concrete, type. *)
type mono_ty =
  | MonoUnit
  | MonoBoolean
  | MonoInteger of signedness * integer_width
  | MonoSingleFloat
  | MonoDoubleFloat
  | MonoNamedType of qident * mono_id
  (** A monomorphic instance of a generic type, identified by the name of the
     type, its monomorphic arguments, and a monomorph ID. *)
  | MonoArray of mono_ty
  | MonoReadRef of mono_ty
  | MonoWriteRef of mono_ty
  | MonoRawPointer of mono_ty
[@@deriving eq]

(** The table of monomorphs associates a generic type, function, or method's
   name and list of monomorphic type arguments to:

1. Its monomorph ID.

2. A {!mono_status} value that indicates whether the monomorph has been
   instantiated.

*)
type mono_tbl

(** An empty table of monomorphs. *)
val empty_mono_tbl : mono_tbl

(** Retrieve the ID of the monomorph for the given name and argument list. *)
val get_monomorph_id : mono_tbl -> qident -> mono_ty list -> mono_id option

(** Add a new monomorph to the table, returning a tuple of the monomorph's ID
   and the updated table. By default, the status of the new monomorph is
   {!NotInstantiated}.

   Throws an error if it already exists. *)
val add_monomorph : mono_tbl -> qident -> mono_ty list -> (mono_id * mono_tbl)
