(** Transform typed statements to serialized statements. *)
open Id
open Identifier
open Tast
open AcmFile
open Env
open EnvExtras
open Error

let get_instance_ref (env: env) (id: decl_id): ins_ref =
  match get_decl_by_id env id with
  | Some (Instance { mod_id; typeclass_id; argument; _ }) ->
     (match get_decl_by_id env typeclass_id with
      | Some (TypeClass { name; _ }) ->
         let typeclass_name = name in
         (match get_module_by_id env mod_id with
          | Some (ModRec { name; _ }) ->
             InsRef { module_name=name; typeclass_name; argument }
          | _ ->
            internal_err "Not in env")
      | _ ->
        internal_err "Not in env")
  | _ ->
     internal_err "Not in env"

let get_method_ref (env: env) (id: ins_meth_id): ins_meth_ref =
  match get_instance_method env id with
  | Some (InsMethRec { instance_id; name; _ }) ->
     InsMethRef (get_instance_ref env instance_id, name)
  | None ->
     internal_err "Not in env"

let get_func_ref (env: env) (id: decl_id): named_decl_ref =
  match get_decl_by_id env id with
  | Some (Function { mod_id; name; _ }) ->
     DeclRef {
         module_name = get_module_name_or_die env mod_id;
         decl_name = name;
       }
  | Some _ ->
     err "Not a function"
  | None ->
     internal_err "Not in env"

let rec ser_expr (env: env) (expr: texpr): ser_expr =
  let se = ser_expr env in
  match expr with
  | TNilConstant ->
     SNilConstant
  | TBoolConstant b ->
     SBoolConstant b
  | TIntConstant i ->
     SIntConstant i
  | TFloatConstant f ->
     SFloatConstant f
  | TStringConstant s ->
     SStringConstant s
  | TConstVar (name, ty) ->
     SConstVar (name, ty)
  | TParamVar (name, ty) ->
     SParamVar (name, ty)
  | TLocalVar (name, ty) ->
     SLocalVar (name, ty)
  | TFunVar (id, ty, bindings) ->
     SFunVar (get_func_ref env id, ty, bindings)
  | TArithmetic (op, lhs, rhs) ->
     SArithmetic (op, se lhs, se rhs)
  | TFuncall (_, name, values, ty, bindings) ->
     SFuncall (name, List.map se values, ty, bindings)
  | TMethodCall (ins_meth_id, _, typarams, args, ty, bindings) ->
     let meth_ref = get_method_ref env ins_meth_id in
     SMethodCall (meth_ref, typarams, List.map se args, ty, bindings)
  | TVarMethodCall { method_name; args; dispatch_ty; rt; _ } ->
     SVarMethodCall {
         method_name = qident_to_sident method_name;
         args = List.map se args;
         dispatch_ty = dispatch_ty;
         rt = rt;
       }
  | TFptrCall (name, args, ty) ->
     SFptrCall (name, List.map se args, ty)
  | TCast (e, ty) ->
     SCast (se e, ty)
  | TComparison (op, lhs, rhs) ->
     SComparison (op, se lhs, se rhs)
  | TConjunction (lhs, rhs) ->
     SConjunction (se lhs, se rhs)
  | TDisjunction (lhs, rhs) ->
     SDisjunction (se lhs, se rhs)
  | TNegation e ->
     SNegation (se e)
  | TIfExpression (c, t, f) ->
     SIfExpression (se c, se t, se f)
  | TRecordConstructor (ty, values) ->
     SRecordConstructor (ty, List.map (fun (n, e) -> (n, se e)) values)
  | TUnionConstructor (ty, name, values) ->
     SUnionConstructor (ty, name, List.map (fun (n, e) -> (n, se e)) values)
  | TPath { head; elems; ty } ->
     SPath { head = se head; elems = List.map (ser_path_elem env) elems; ty; }
  | TEmbed (ty, name, values) ->
     SEmbed (ty, name, List.map se values)
  | TDeref e ->
     SDeref (se e)
  | TSizeOf ty ->
     SSizeOf ty
  | TBorrowExpr (mode, name, region, ty) ->
     SBorrowExpr (mode, name, region, ty)

and ser_path_elem env elem =
  match elem with
  | TSlotAccessor (name, ty) -> SerSlotAccessor (name, ty)
  | TPointerSlotAccessor (name, ty) -> SerPointerSlotAccessor (name, ty)
  | TArrayIndex (value, ty) -> SerArrayIndex (ser_expr env value, ty)

let ser_lvalue env (TypedLValue (name, elems)) =
    SerLValue (name, List.map (ser_path_elem env) elems)

let rec ser_stmt (env: env) (stmt: tstmt): ser_stmt =
  let se = ser_expr env in
  let ss = ser_stmt env in
  match stmt with
  | TSkip span ->
     SSkip span
  | TLet (span, name, ty, value, body) ->
     SLet (span, name, ty, se value, ss body)
  | TDestructure (span, bindings, value, body) ->
     SDestructure (span, bindings, se value, ss body)
  | TAssign (span, lvalue, value) ->
     SAssign (span, ser_lvalue env lvalue, se value)
  | TIf (span, cond, tb, fb) ->
     SIf (span, se cond, ss tb, ss fb)
  | TCase (case, value, cases) ->
     SCase (case, se value, List.map (ser_when env) cases)
  | TWhile (span, cond, body) ->
     SWhile (span, se cond, ss body)
  | TFor (span, name, initial, final, body) ->
     SFor (span, name, se initial, se final, ss body)
  | TBorrow { span; original; rename; region; orig_type; ref_type; body; mode; } ->
     SBorrow { span; original; rename; region; orig_type; ref_type; body=ss body; mode; }
  | TBlock (span, a, b) ->
     SBlock (span, ss a, ss b)
  | TDiscarding (span, e) ->
     SDiscarding (span, se e)
  | TReturn (span, e) ->
     SReturn (span, se e)

and ser_when (env: env) (TypedWhen (name, bindings, body)) =
  SerWhen (name, bindings, ser_stmt env body)
