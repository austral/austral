open Type
open TypeSystem
open Appearances

(* True when a universe is Linear or Type. *)
let universe_linear_ish = function
  | LinearUniverse -> true
  | TypeUniverse -> true
  | FreeUniverse -> false
  | RegionUniverse -> false

(* The position index of a parameter. *)
let param_pos: int = 0

let appearances_from_params (params: value_parameter list): appear_tbl =
  (* Start with the empty table. *)
  let tbl: appear_tbl = empty_appearances in
  (* Iterate over the parameter list. *)
  let folder (tbl: appear_tbl) (param: value_parameter): appear_tbl =
    let (ValueParameter (name, ty)) = param in
    (* If a parameter has a linear type, add it to the table. *)
    if universe_linear_ish (type_universe ty) then
      register_var tbl name param_pos []
    else
      (* If it's not linear skip to the next parameter. *)
      tbl
  in
  List.fold_left folder tbl params
