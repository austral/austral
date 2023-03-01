(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

(** Like TypeBindings, but the values are monomorphic types. *)
open MonoType
open TypeParameter

(** A map from type parameters to monomorphic types. *)
type mono_type_bindings

val empty_mono_bindings : mono_type_bindings

val mono_bindings_as_list : mono_type_bindings -> (type_parameter * mono_ty) list

val mono_bindings_from_list : (type_parameter * mono_ty) list -> mono_type_bindings

val equal_mono_bindings : mono_type_bindings -> mono_type_bindings -> bool

val show_mono_type_bindings : mono_type_bindings -> string

val get_mono_binding : mono_type_bindings -> type_parameter -> mono_ty option
