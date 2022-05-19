open Identifier
open Type
open TypeSystem
open LifetimeTables
open Tast
open Ptast
open Error

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

let rec extract_variables (tbl: appear_tbl) (expr: texpr): (identifier * appear_kind) list =
  match expr with
  | TNilConstant ->
     []
  | TBoolConstant _ ->
     []
  | TIntConstant _ ->
     []
  | TFloatConstant _ ->
     []
  | TStringConstant _ ->
     []
  | TConstVar _ ->
     (* Constants can't be linear so we don't need to extract them. *)
     []
  | TParamVar (name, _) ->
     (* If the name is mentioned in the table, extract it. *)
     if tbl_has_name tbl name then
       [(name, AppearConsume)]
     else
       []
  | TLocalVar (name, _) ->
     (* If the name is mentioned in the table, extract it. *)
     if tbl_has_name tbl name then
       [(name, AppearConsume)]
     else
       []
  | TArithmetic (_, lhs, rhs) ->
     let lhs' = extract_variables tbl lhs in
     let rhs' = extract_variables tbl rhs in
     lhs' @ rhs'
  | TFuncall (_, _, args, _, _) ->
     let args' = List.map (extract_variables tbl) args in
     List.concat args'
  | TMethodCall (_, _, _, args, _, _) ->
     let args' = List.map (extract_variables tbl) args in
     List.concat args'
  | TCast (expr, _) ->
     extract_variables tbl expr
  | TComparison (_, lhs, rhs) ->
     let lhs' = extract_variables tbl lhs in
     let rhs' = extract_variables tbl rhs in
     lhs' @ rhs'
  | TConjunction (lhs, rhs) ->
     let lhs' = extract_variables tbl lhs in
     let rhs' = extract_variables tbl rhs in
     lhs' @ rhs'
  | TDisjunction (lhs, rhs) ->
     let lhs' = extract_variables tbl lhs in
     let rhs' = extract_variables tbl rhs in
     lhs' @ rhs'
  | TNegation expr ->
     extract_variables tbl expr
  | TIfExpression (c, t, f) ->
     let c' = extract_variables tbl c
     and t' = extract_variables tbl t
     and f' = extract_variables tbl f
     in
     c' @ t' @ f'
  | TRecordConstructor (_, args) ->
      let args' = List.map (fun (_, e) -> extract_variables tbl e) args in
      List.concat args'
  | TUnionConstructor (_, _, args) ->
      let args' = List.map (fun (_, e) -> extract_variables tbl e) args in
      List.concat args'
  | TTypeAliasConstructor (_, e) ->
     extract_variables tbl e
  | TPath { head; elems; _ } ->
     let head' =
       (match head with
        (* If the head is a variable, extract is with AppearPath. *)
        | TParamVar (name, _) ->
           if tbl_has_name tbl name then
             [(name, AppearPath)]
           else
             []
        | TLocalVar (name, _) ->
           if tbl_has_name tbl name then
             [(name, AppearPath)]
           else
             []
        | _ ->
           (* Otherwise, just extract the variables inside. *)
           extract_variables tbl head)
     and elems' = List.map (extract_path_variables tbl) elems
     in
     head' @ (List.concat elems')
  | TEmbed (_, _, args) ->
     let args' = List.map (extract_variables tbl) args in
     List.concat args'
  | TDeref e ->
     extract_variables tbl e
  | TSizeOf _ ->
     []
  | TBorrowExpr (_, name, _, _) ->
     if tbl_has_name tbl name then
       [(name, AppearBorrow)]
     else
       []

and extract_path_variables (tbl: appear_tbl) (elem: typed_path_elem): (identifier * appear_kind) list =
  match elem with
  | TSlotAccessor _ ->
     []
  | TPointerSlotAccessor _ ->
     []
  | TArrayIndex (e, _) ->
     extract_variables tbl e

(** Given a table and statement's position and loop context, register a list of appearances. *)
let register_appears (tbl: appear_tbl) (pos: pos) (loop_ctx: loop_context) (lst: (identifier * appear_kind) list): appear_tbl =
  List.fold_left (fun tbl (name, kind) -> register_appear tbl name pos kind loop_ctx) tbl lst

let rec record_appearances (tbl: appear_tbl) (stmt: pstmt): appear_tbl =
  record_appearances' tbl [] stmt

and record_appearances' (tbl: appear_tbl) (loop_ctx: loop_context) (stmt: pstmt): appear_tbl =
  match stmt with
  | PSkip _ ->
     tbl
  | PLet (pos, _, name, _, value, body) ->
     (* Register appearances in the right side. *)
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl value) in
     (* Register the variable. *)
     let tbl = register_var tbl name pos loop_ctx in
     record_appearances' tbl loop_ctx body
  | PLetBorrow { body; _ } ->
     record_appearances' tbl loop_ctx body
  | _ ->
     err "Not implemented"
