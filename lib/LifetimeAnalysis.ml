open Identifier
open Type
open TypeSystem
open LifetimeTables
open Tast
open Ptast
open RegionSet

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
     and elem_vars' = extract_all_path_variables tbl elems
     in
     head' @ elem_vars'
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

and extract_all_path_variables (tbl:appear_tbl) (elems: typed_path_elem list): (identifier * appear_kind) list =
  List.concat (List.map (extract_path_variables tbl) elems)

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

let rec register_variables (tbl: appear_tbl) (stmt: pstmt): appear_tbl =
  register_variables' tbl [] stmt

and register_variables' (tbl: appear_tbl) (loop_ctx: loop_context) (stmt: pstmt): appear_tbl =
  match stmt with
  | PSkip _ ->
     tbl
  | PLet (pos, _, name, ty, _, body) ->
     (* If the variable is linear-ish, register it. *)
     let tbl =
       if universe_linear_ish (type_universe ty) then
         register_var tbl name pos loop_ctx
       else
         tbl
     in
     register_variables' tbl loop_ctx body
  | PLetBorrow _ ->
     (* We don't have to do anything here, since borrows are not linear. *)
     tbl
  | PDestructure (pos, _, bindings, _, body) ->
     (* Register the variables which are linear. *)
     let tbl = register_bindings tbl pos bindings in
     register_variables' tbl loop_ctx body
  | PAssign _ ->
     tbl
  | PIf (_, _, _, tb, fb) ->
     let tbl = register_variables' tbl loop_ctx tb in
     let tbl = register_variables' tbl loop_ctx fb in
     tbl
  | PCase (_, _, _, whens) ->
     let folder (tbl: appear_tbl) (pwhen: pwhen): appear_tbl =
       let (PWhen (pos, _, bindings, body)) = pwhen in
       (* Register any linear bindings. *)
       let tbl = register_bindings tbl pos (List.map (fun (ValueParameter (n, t)) -> (n, t)) bindings) in
       let tbl = register_variables' tbl loop_ctx body in
       tbl
     in
     List.fold_left folder tbl whens
  | PWhile (_, _, _, body) ->
     register_variables' tbl (CtxWhile :: loop_ctx) body
  | PFor (_, _, _, _, _, _, body) ->
     register_variables' tbl (CtxFor :: loop_ctx) body
  | PBorrow _ ->
     tbl
  | PBlock (_, a, b) ->
     let tbl = register_variables' tbl loop_ctx a in
     let tbl = register_variables' tbl loop_ctx b in
     tbl
  | PDiscarding _ ->
     tbl
  | PReturn _ ->
     tbl

and register_bindings (tbl: appear_tbl) (pos: pos) (bindings: (identifier * ty) list): appear_tbl =
  let folder (tbl: appear_tbl) (binding: (identifier * ty)): appear_tbl =
    let (name, ty) = binding in
    (* If the binding has a linear type, add it to the table. *)
    if universe_linear_ish (type_universe ty) then
      register_var tbl name pos []
    else
      (* If it's not linear skip to the next binding. *)
      tbl
  in
  List.fold_left folder tbl bindings

let rec record_appearances (tbl: appear_tbl) (stmt: pstmt): appear_tbl =
  record_appearances' tbl [] stmt

and record_appearances' (tbl: appear_tbl) (loop_ctx: loop_context) (stmt: pstmt): appear_tbl =
  match stmt with
  | PSkip _ ->
     tbl
  | PLet (pos, _, _, _, value, body) ->
     (* Register appearances in the right side. *)
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl value) in
     record_appearances' tbl loop_ctx body
  | PLetBorrow { name; pos; body; _ } ->
     (* Register the borrow *)
     let tbl = register_appear tbl name pos AppearBorrow loop_ctx in
     record_appearances' tbl loop_ctx body
  | PDestructure (pos, _, _, expr, body) ->
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl expr) in
     record_appearances' tbl loop_ctx body
  | PAssign (_, pos, lvalue, pos', expr) ->
     let (TypedLValue (_, elems)) = lvalue in
     let tbl = register_appears tbl pos loop_ctx (extract_all_path_variables tbl elems) in
     let tbl = register_appears tbl pos' loop_ctx (extract_variables tbl expr) in
     tbl
  | PIf (pos, _, expr, tb, fb) ->
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl expr) in
     let tbl = record_appearances' tbl loop_ctx tb in
     let tbl = record_appearances' tbl loop_ctx fb in
     tbl
  | PCase (pos, _, expr, whens) ->
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl expr) in
     let folder (tbl: appear_tbl) (pwhen: pwhen): appear_tbl =
       let (PWhen (_, _, _, body)) = pwhen in
       let tbl = record_appearances' tbl loop_ctx body in
       tbl
     in
     List.fold_left folder tbl whens
  | PWhile (pos, _, expr, body) ->
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl expr) in
     record_appearances' tbl (CtxWhile :: loop_ctx) body
  | PFor (_, _, pos, init, pos', final, body) ->
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl init) in
     let tbl = register_appears tbl pos' loop_ctx (extract_variables tbl final) in
     record_appearances' tbl (CtxFor :: loop_ctx) body
  | PBorrow _ ->
     (* TODO *)
     tbl
  | PBlock (_, a, b) ->
     let tbl = record_appearances' tbl loop_ctx a in
     let tbl = record_appearances' tbl loop_ctx b in
     tbl
  | PDiscarding (pos, _, expr) ->
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl expr) in
     tbl
  | PReturn (pos, _, expr) ->
     let tbl = register_appears tbl pos loop_ctx (extract_variables tbl expr) in
     tbl

let rec type_regions (ty: ty): RegionSet.t =
  let e = RegionSet.empty in
  match ty with
  | Unit -> e
  | Boolean -> e
  | Integer _ -> e
  | SingleFloat -> e
  | DoubleFloat -> e
  | NamedType (_, args, _) ->
     List.fold_left RegionSet.union e (List.map type_regions args)
  | StaticArray ty ->
     type_regions ty
  | RegionTy r ->
     RegionSet.singleton r
  | ReadRef (ty, r) ->
     RegionSet.union (type_regions ty) (type_regions r)
  | WriteRef (ty, r) ->
     RegionSet.union (type_regions ty) (type_regions r)
  | TyVar _ ->
     e
  | Address ty ->
     type_regions ty
  | Pointer ty ->
     type_regions ty
  | MonoTy _ ->
     e
