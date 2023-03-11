(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Type
open MonoType
open TypeParameter

let typaram_as_string (tp: type_parameter): string =
  let name: string = ident_string (typaram_name tp)
  and uni: string = show_universe (typaram_universe tp)
  and src: string = qident_debug_name (typaram_source tp)
  and cs: string = String.concat "," (List.map show_sident (typaram_constraints tp))
  in
  name ^ ";" ^ uni ^ ";" ^ src ^ ";" ^ cs

module BindingsMap =
  Map.Make(
      struct
        type t = type_parameter
        let compare (a: type_parameter) (b: type_parameter): int =
          compare (typaram_as_string a) (typaram_as_string b)
      end
    )

type mono_type_bindings = MonoTypeBindings of mono_ty BindingsMap.t

let empty_mono_bindings = MonoTypeBindings BindingsMap.empty

let mono_bindings_as_list (MonoTypeBindings m) =
  (BindingsMap.bindings m)

let mono_bindings_from_list lst =
  MonoTypeBindings (BindingsMap.of_seq (List.to_seq lst))

let equal_mono_bindings (MonoTypeBindings a) (MonoTypeBindings b) =
  BindingsMap.equal equal_mono_ty a b

let show_mono_type_bindings (MonoTypeBindings m) =
  let show_binding (tp, t) =
    (show_type_parameter tp) ^ " => " ^ (show_mono_ty t)
  in
  "MonoTypeBindings {" ^ (String.concat ", " (List.map show_binding (BindingsMap.bindings m))) ^ "}"

let get_mono_binding (MonoTypeBindings m) tp =
  BindingsMap.find_opt tp m
