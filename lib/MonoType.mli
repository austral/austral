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

(** A stripped type specifier is the same as a type specifier, but the region
   and type variable cases have been removed. *)
type stripped_ty

(** Strip a type specifier. *)
val strip_type : ty -> stripped_ty option

(** Monomorphize a type.

   This function works bottom up, looking for invocations of `NamedType` with
   monomorphic arguments. When it finds one, it adds it to the table of
   monomorphs and replaces it with an instance of `MonoNamedType` with a fresh
   monomorph ID.

   To illustrate how it works, consider this type specifier:

   {[
   Map[Int, Pair[String, Option[Array[Int]]]]
   ]}

   At each step in recursive monomorphization, the type specifier and the table
   of monomorphs looks like this:

   {[

                      Expression                 |             Table
       ------------------------------------------|---------------------------------
                                                 |
        Map[Int, Pair[String, Option[Mono{0}]]]  |  (Array,  [Int],             0)
                                                 |
       ------------------------------------------|---------------------------------
                                                 |
        Map[Int, Pair[String, Mono{1}]]          |  (Array,  [Int],             0)
                                                 |  (Option, [Mono{0}],         1)
                                                 |
       ------------------------------------------|---------------------------------
                                                 |
        Map[Int, Mono{2}],                       |  (Array,  [Int],             0)
                                                 |  (Option, [Mono{0}],         1)
                                                 |  (Pair,   [String, Mono{1}], 2)
                                                 |
       ------------------------------------------|---------------------------------
                                                 |
        Mono{3}                                  |  (Array,  [Int],             0)
                                                 |  (Option, [Mono{0}],         1)
                                                 |  (Pair,   [String, Mono{1}], 2)
                                                 |  (Map,    [Int, Mono{2}],    3)

   ]}

   For simplicity, the instantiation state is elided, since it's always
   {!NotInstantiated} when a new monomorph is added to the table.

 *)
val monomorphize_type : mono_tbl -> stripped_ty -> (mono_ty * mono_tbl)
