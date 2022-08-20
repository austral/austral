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
