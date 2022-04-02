open Identifier
open Env
open Type
open TypeStripping
open MonoType2
open Tast
open Mtast
open Error

let rec monomorphize_ty (env: env) (ty: stripped_ty): (mono_ty * env) =
  match ty with
  | SUnit ->
    (MonoUnit, env)
  | SBoolean ->
    (MonoBoolean, env)
  | SInteger (s, w) ->
    (MonoInteger (s, w), env)
  | SSingleFloat ->
    (MonoSingleFloat, env)
  | SDoubleFloat ->
    (MonoDoubleFloat, env)
  | SArray elem_ty ->
    let (elem_ty, env) = monomorphize_ty env elem_ty in
    (MonoArray elem_ty, env)
  | SReadRef ty ->
    let (ty, env) = monomorphize_ty env ty in
    (MonoReadRef ty, env)
  | SWriteRef ty ->
    let (ty, env) = monomorphize_ty env ty in
    (MonoWriteRef ty, env)
  | SRawPointer ty ->
    let (ty, env) = monomorphize_ty env ty in
    (MonoRawPointer ty, env)
  | SNamedType (name, args) ->
    let (args, env) = monomorphize_ty_list env args in
    (match get_decl_by_name env (qident_to_sident name) with
     | Some decl ->
       (match decl with
        | TypeAlias { id; _} ->
          let (env, mono_id) = add_or_get_type_alias_monomorph env id args in
          (MonoNamedType mono_id, env)
        | Record { id; _ } ->
          let (env, mono_id) = add_or_get_record_monomorph env id args in
          (MonoNamedType mono_id, env)
        | Union { id; _ } ->
          let (env, mono_id) = add_or_get_union_monomorph env id args in
          (MonoNamedType mono_id, env)
        | _ ->
          err "internal: named type points to something that isn't a type")
     | None ->
       err "internal")

and monomorphize_ty_list (env: env) (tys: stripped_ty list): (mono_ty list * env) =
  match tys with
  | first::rest ->
     let (first, env) = monomorphize_ty env first in
     let (rest, env) = monomorphize_ty_list env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

let strip_and_mono (env: env) (ty: ty): (mono_ty * env) =
  let ty = strip_type ty in
  monomorphize_ty env ty

let rec monomorphize_expr (env: env) (expr: texpr): (mexpr * env) =
  match expr with
  | TNilConstant ->
     (MNilConstant, env)
  | TBoolConstant b ->
     (MBoolConstant b, env)
  | TIntConstant i ->
     (MIntConstant i, env)
  | TFloatConstant f ->
     (MFloatConstant f, env)
  | TStringConstant s ->
     (MStringConstant s, env)
  | TVariable (name, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MVariable (name, ty), env)
  | TArithmetic (oper, lhs, rhs) ->
     let (lhs, env) = monomorphize_expr env lhs in
     let (rhs, env) = monomorphize_expr env rhs in
     (MArithmetic (oper, lhs, rhs), env)
  | TFuncall (decl_id, name, args, rt, substs) ->
     (* Monomorphize the return type. *)
     let (rt, env) = strip_and_mono env rt in
     (* Monomorphize the arglist *)
     let (args, env) = monomorphize_expr_list env args in
     (* Does the funcall have a substitution list? *)
     if List.length substs > 0 then
       (* The function is generic. *)
       (* Monomorphize the tyargs *)
       let tyargs = List.map (fun (_, ty) -> strip_type ty) substs in
       let (tyargs, env) = monomorphize_ty_list env tyargs in
       let (env, mono_id) = add_or_get_function_monomorph env decl_id tyargs in
       (MGenericFuncall (mono_id, args, rt), env)
     else
       (* The function is concrete. *)
       (MConcreteFuncall (name, args, rt), env)
  | TMethodCall (ins_meth_id, name, typarams, args, rt, substs) ->
     (* Monomorphize the return type. *)
     let (rt, env) = strip_and_mono env rt in
     (* Monomorphize the arglist *)
     let (args, env) = monomorphize_expr_list env args in
     (* Does the funcall have a list of type params? *)
     if List.length typarams > 0 then
       (* The instance is generic. *)
       (* Monomorphize the tyargs *)
       let tyargs = List.map (fun (_, ty) -> strip_type ty) substs in
       let (tyargs, env) = monomorphize_ty_list env tyargs in
       let (env, mono_id) = add_or_get_instance_method_monomorph env ins_meth_id tyargs in
       (MGenericFuncall (mono_id, args, rt), env)
     else
       (* The instance is concrete. *)
       (MConcreteFuncall (name, args, rt), env)
  | TCast (expr, ty) ->
     let (ty, env) = strip_and_mono env ty in
     let (expr, env) = monomorphize_expr env expr in
     (MCast (expr, ty), env)
  | TComparison (oper, lhs, rhs) ->
     let (lhs, env) = monomorphize_expr env lhs in
     let (rhs, env) = monomorphize_expr env rhs in
     (MComparison (oper, lhs, rhs), env)
  | TConjunction (lhs, rhs) ->
     let (lhs, env) = monomorphize_expr env lhs in
     let (rhs, env) = monomorphize_expr env rhs in
     (MConjunction (lhs, rhs), env)
  | TDisjunction (lhs, rhs) ->
     let (lhs, env) = monomorphize_expr env lhs in
     let (rhs, env) = monomorphize_expr env rhs in
     (MDisjunction (lhs, rhs), env)
  | TNegation expr ->
     let (expr, env) = monomorphize_expr env expr in
     (MNegation expr, env)
  | TIfExpression (c, t, f) ->
     let (c, env) = monomorphize_expr env c in
     let (t, env) = monomorphize_expr env t in
     let (f, env) = monomorphize_expr env f in
     (MIfExpression (c, t, f), env)
  | TRecordConstructor (ty, args) ->
     let (ty, env) = strip_and_mono env ty in
     let (args, env) = monomorphize_named_expr_list env args in
     (MRecordConstructor (ty, args), env)
  | TUnionConstructor (ty, case_name, args) ->
     let (ty, env) = strip_and_mono env ty in
     let (args, env) = monomorphize_named_expr_list env args in
     (MUnionConstructor (ty, case_name, args), env)
  | TTypeAliasConstructor (ty, expr) ->
     let (ty, env) = strip_and_mono env ty in
     let (expr, env) = monomorphize_expr env expr in
     (MTypeAliasConstructor (ty, expr), env)
  | TPath { head; elems; ty } ->
     let (ty, env) = strip_and_mono env ty in
     let (head, env) = monomorphize_expr env head in
     let (elems, env) = monomorphize_path_elems env elems in
     (MPath { head = head; elems = elems; ty = ty }, env)
  | TEmbed (ty, fmt, args) ->
     let (ty, env) = strip_and_mono env ty in
     let (args, env) = monomorphize_expr_list env args in
     (MEmbed (ty, fmt, args), env)
  | TDeref expr ->
     let (expr, env) = monomorphize_expr env expr in
     (MDeref expr, env)
  | TSizeOf ty ->
     let (ty, env) = strip_and_mono env ty in
     (MSizeOf ty, env)

and monomorphize_expr_list (env: env) (exprs: texpr list): (mexpr list * env) =
  match exprs with
  | first::rest ->
     let (first, env) = monomorphize_expr env first in
     let (rest, env) = monomorphize_expr_list env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

and monomorphize_named_expr_list (env: env) (exprs: (identifier * texpr) list): ((identifier * mexpr) list * env) =
  match exprs with
  | (name, first)::rest ->
     let (first, env) = monomorphize_expr env first in
     let (rest, env) = monomorphize_named_expr_list env rest in
     ((name, first) :: rest, env)
  | [] ->
     ([], env)

and monomorphize_path_elems (env: env) (elems: typed_path_elem list): (mtyped_path_elem list * env) =
  match elems with
  | first::rest ->
     let (first, env) = monomorphize_path_elem env first in
     let (rest, env) = monomorphize_path_elems env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

and monomorphize_path_elem (env: env) (elem: typed_path_elem): (mtyped_path_elem * env) =
  match elem with
  | TSlotAccessor (name, ty) ->
     let ty = strip_type ty in
     let (ty, env) = monomorphize_ty env ty in
     (MSlotAccessor (name, ty), env)
  | TPointerSlotAccessor (name, ty) ->
     let ty = strip_type ty in
     let (ty, env) = monomorphize_ty env ty in
     (MPointerSlotAccessor (name, ty), env)
  | TArrayIndex (idx, ty) ->
     let ty = strip_type ty in
     let (ty, env) = monomorphize_ty env ty in
     let (idx, env) = monomorphize_expr env idx in
     (MArrayIndex (idx, ty), env)
