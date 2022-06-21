open Tast
open Type
open TypeBindings

let rec replace_tyvars_expr (bindings: type_bindings) (expr: texpr): texpr =
  match expr with
  | TNilConstant ->
     TNilConstant
  | TBoolConstant b ->
     TBoolConstant b
  | TIntConstant i ->
     TIntConstant i
  | TFloatConstant f ->
     TFloatConstant f
  | TStringConstant s ->
     TStringConstant s
  | TConstVar (name, ty) ->
     let ty = replace_variables bindings ty in
     TConstVar (name, ty)
  | TParamVar (name, ty) ->
     let ty = replace_variables bindings ty in
     TParamVar (name, ty)
  | TLocalVar (name, ty) ->
     let ty = replace_variables bindings ty in
     TLocalVar (name, ty)
  | TArithmetic (oper, lhs, rhs) ->
     let lhs = replace_tyvars_expr bindings lhs
     and rhs = replace_tyvars_expr bindings rhs in
     TArithmetic (oper, lhs, rhs)
  | TFuncall (id, name, args, rt, substs) ->
     let args = List.map (replace_tyvars_expr bindings) args
     and rt = replace_variables bindings rt
     and substs = List.map (fun (n, t) -> (n, replace_variables bindings t)) substs in
     TFuncall (id, name, args, rt, substs)
  | TMethodCall (meth_id, name, instance, args, rt, substs) ->
     let args = List.map (replace_tyvars_expr bindings) args
     and rt = replace_variables bindings rt
     and substs = List.map (fun (n, t) -> (n, replace_variables bindings t)) substs in
     TMethodCall (meth_id, name, instance, args, rt, substs)
  | TVarMethodCall { source_module_name; typeclass_id; params; method_name; args; dispatch_ty; rt; bindings; } ->
     (* FIXME: Do we have to replace the params and the bindings? *)
     let args = List.map (replace_tyvars_expr bindings) args
     and dispatch_ty = replace_variables bindings dispatch_ty
     and rt = replace_variables bindings rt
     in
     TVarMethodCall { source_module_name; typeclass_id; params; method_name; args; dispatch_ty; rt; bindings; }
  | TCast (expr, ty) ->
     let expr = replace_tyvars_expr bindings expr
     and ty = replace_variables bindings ty in
     TCast (expr, ty)
  | TComparison (oper, lhs, rhs) ->
     let lhs = replace_tyvars_expr bindings lhs
     and rhs = replace_tyvars_expr bindings rhs in
     TComparison (oper, lhs, rhs)
  | TConjunction (lhs, rhs) ->
     let lhs = replace_tyvars_expr bindings lhs
     and rhs = replace_tyvars_expr bindings rhs in
     TConjunction (lhs, rhs)
  | TDisjunction (lhs, rhs) ->
     let lhs = replace_tyvars_expr bindings lhs
     and rhs = replace_tyvars_expr bindings rhs in
     TDisjunction (lhs, rhs)
  | TNegation expr ->
     let expr = replace_tyvars_expr bindings expr in
     TNegation expr
  | TIfExpression (c, t, f) ->
     let c = replace_tyvars_expr bindings c
     and t = replace_tyvars_expr bindings t
     and f = replace_tyvars_expr bindings f in
     TIfExpression (c, t, f)
  | TRecordConstructor (ty, args) ->
     let ty = replace_variables bindings ty
     and args = List.map (fun (n, e) -> (n, replace_tyvars_expr bindings e)) args in
     TRecordConstructor (ty, args)
  | TUnionConstructor (ty, case_name, args) ->
     let ty = replace_variables bindings ty
     and args = List.map (fun (n, e) -> (n, replace_tyvars_expr bindings e)) args in
     TUnionConstructor (ty, case_name, args)
  | TTypeAliasConstructor (ty, expr) ->
     let ty = replace_variables bindings ty
     and expr = replace_tyvars_expr bindings expr in
     TTypeAliasConstructor (ty, expr)
  | TPath { head; elems; ty } ->
     let head = replace_tyvars_expr bindings head
     and elems = List.map (replace_tyvars_path bindings) elems
     and ty = replace_variables bindings ty in
    TPath { head = head; elems = elems; ty = ty }
  | TEmbed (ty, fmt, args) ->
     let ty = replace_variables bindings ty
     and args = List.map (replace_tyvars_expr bindings) args in
     TEmbed (ty, fmt, args)
  | TDeref expr ->
     let expr = replace_tyvars_expr bindings expr in
     TDeref expr
  | TSizeOf ty ->
     let ty = replace_variables bindings ty in
     TSizeOf ty
  | TBorrowExpr (mode, name, region, ty) ->
     let ty = replace_variables bindings ty in
     TBorrowExpr (mode, name, region, ty)

and replace_tyvars_path (bindings: type_bindings) (elem: typed_path_elem): typed_path_elem =
  match elem with
  | TSlotAccessor (name, ty) ->
     let ty = replace_variables bindings ty in
     TSlotAccessor (name, ty)
  | TPointerSlotAccessor (name, ty) ->
     let ty = replace_variables bindings ty in
     TPointerSlotAccessor (name, ty)
  | TArrayIndex (idx, ty) ->
     let idx = replace_tyvars_expr bindings idx
     and ty = replace_variables bindings ty in
     TArrayIndex (idx, ty)

let rec replace_tyvars_stmt (bindings: type_bindings) (stmt: tstmt): tstmt =
  match stmt with
  | TSkip span ->
     TSkip span
  | TLet (span, name, ty, value, body) ->
     let ty = replace_variables bindings ty
     and value = replace_tyvars_expr bindings value
     and body = replace_tyvars_stmt bindings body in
     TLet (span, name, ty, value, body)
  | TDestructure (span, params, value, body) ->
     let params = List.map (fun (n, ty) -> (n, replace_variables bindings ty)) params
     and value = replace_tyvars_expr bindings value
     and body = replace_tyvars_stmt bindings body in
     TDestructure (span, params, value, body)
  | TAssign (span, lvalue, value) ->
     let lvalue = replace_tyvars_lvalue bindings lvalue
     and value = replace_tyvars_expr bindings value in
     TAssign (span, lvalue, value)
  | TIf (span, c, t, f) ->
     let c = replace_tyvars_expr bindings c
     and t = replace_tyvars_stmt bindings t
     and f = replace_tyvars_stmt bindings f in
     TIf (span, c, t, f)
  | TCase (span, value, whens) ->
     let value = replace_tyvars_expr bindings value
     and whens = List.map (replace_tyvars_when bindings) whens in
     TCase (span, value, whens)
  | TWhile (span, value, body) ->
      let value = replace_tyvars_expr bindings value
      and body = replace_tyvars_stmt bindings body in
      TWhile (span, value, body)
  | TFor (span, name, initial, final, body) ->
     let initial = replace_tyvars_expr bindings initial
     and final = replace_tyvars_expr bindings final
     and body = replace_tyvars_stmt bindings body in
     TFor (span, name, initial, final, body)
  | TBorrow { span; original; rename; region; orig_type; ref_type; body; mode } ->
     let orig_type = replace_variables bindings orig_type
     and ref_type = replace_variables bindings ref_type
     and body = replace_tyvars_stmt bindings body in
     TBorrow {
         span = span;
         original = original;
         rename = rename;
         region = region;
         orig_type = orig_type;
         ref_type = ref_type;
         body = body;
         mode = mode
       }
  | TBlock (span, a, b) ->
     let a = replace_tyvars_stmt bindings a
     and b = replace_tyvars_stmt bindings b in
     TBlock (span, a, b)
  | TDiscarding (span, expr) ->
     let expr = replace_tyvars_expr bindings expr in
     TDiscarding (span, expr)
  | TReturn (span, expr) ->
     let expr = replace_tyvars_expr bindings expr in
     TReturn (span, expr)

and replace_tyvars_lvalue (bindings: type_bindings) (lvalue: typed_lvalue): typed_lvalue =
  let (TypedLValue (name, elems)) = lvalue in
  let elems = List.map (replace_tyvars_path bindings) elems in
  TypedLValue (name, elems)

and replace_tyvars_when (bindings: type_bindings) (twhen: typed_when): typed_when =
  let (TypedWhen (name, params, body)) = twhen in
  let params = List.map (fun (ValueParameter (n, t)) -> ValueParameter (n, replace_variables bindings t)) params
  and body = replace_tyvars_stmt bindings body in
  TypedWhen (name, params, body)
