(** Code for monomorphizing types and ASTs. *)
open Type
open MonoType
open Tast
open Mtast

(** A stripped type specifier is the same as a type specifier, but the region
   and type variable cases have been removed. *)
type stripped_ty

(** Strip a type specifier. *)
val strip_type : ty -> stripped_ty

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

(** Monomorphize an expression. *)
val monomorphize_expr : mono_tbl -> texpr -> (mexpr * mono_tbl)

(** Monomorphize a statement. *)
val monomorphize_stmt : mono_tbl -> tstmt -> (mstmt * mono_tbl)
