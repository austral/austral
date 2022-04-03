open Identifier
open Env
open Type
open TypeStripping
open MonoType2
open Tast
open Mtast
open Error

(* Monomorphize type specifiers *)

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

(* Monomorphize expressions *)

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

(* Monomorphize statements *)

let rec monomorphize_stmt (env: env) (stmt: tstmt): (mstmt * env) =
  match stmt with
  | TSkip _ ->
     (MSkip, env)
  | TLet (_, name, ty, value, body) ->
     let (ty, env) = strip_and_mono env ty in
     let (value, env) = monomorphize_expr env value in
     let (body, env) = monomorphize_stmt env body in
     (MLet (name, ty, value, body), env)
  | TDestructure (_, bindings, value, body) ->
     let (bindings, env) = monomorphize_named_ty_list env (List.map (fun (n, t) -> (n, strip_type t)) bindings) in
     let (value, env) = monomorphize_expr env value in
     let (body, env) = monomorphize_stmt env body in
     (MDestructure (bindings, value, body), env)
  | TAssign (_, lvalue, value) ->
     let (lvalue, env) = monomorphize_lvalue env lvalue in
     let (value, env) = monomorphize_expr env value in
     (MAssign (lvalue, value), env)
  | TIf (_, c, t, f) ->
     let (c, env) = monomorphize_expr env c in
     let (t, env) = monomorphize_stmt env t in
     let (f, env) = monomorphize_stmt env f in
     (MIf (c, t, f), env)
  | TCase (_, value, whens) ->
     let (value, env) = monomorphize_expr env value in
     let (whens, env) = monomorphize_whens env whens in
     (MCase (value, whens), env)
  | TWhile (_, value, body) ->
     let (value, env) = monomorphize_expr env value in
     let (body, env) = monomorphize_stmt env body in
     (MWhile (value, body), env)
  | TFor (_, name, start, final, body) ->
     let (start, env) = monomorphize_expr env start in
     let (final, env) = monomorphize_expr env final in
     let (body, env) = monomorphize_stmt env body in
     (MFor (name, start, final, body), env)
  | TBorrow { span; original; rename; region; orig_type; ref_type; body; mode } ->
     let _ = span in
     let (orig_type, env) = strip_and_mono env orig_type in
     let (ref_type, env) = strip_and_mono env ref_type in
     let (body, env) = monomorphize_stmt env body in
     (MBorrow { original = original; rename = rename; region = region; orig_type = orig_type; ref_type = ref_type; body = body; mode = mode }, env)
  | TBlock (_, a, b) ->
     let (a, env) = monomorphize_stmt env a in
     let (b, env) = monomorphize_stmt env b in
     (MBlock (a, b), env)
  | TDiscarding (_, value) ->
     let (value, env) = monomorphize_expr env value in
     (MDiscarding value, env)
  | TReturn (_, value) ->
     let (value, env) = monomorphize_expr env value in
     (MReturn value, env)

and monomorphize_lvalue (env: env) (lvalue: typed_lvalue): (mtyped_lvalue * env) =
  match lvalue with
  | TypedLValue (name, elems) ->
     let (elems, env) = monomorphize_path_elems env elems in
     (MTypedLValue (name, elems), env)

and monomorphize_whens (env: env) (whens: typed_when list): (mtyped_when list * env) =
  match whens with
  | first::rest ->
     let (first, env) = monomorphize_when env first in
     let (rest, env) = monomorphize_whens env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

and monomorphize_when (env: env) (w: typed_when): (mtyped_when * env) =
  let (TypedWhen (name, params, body)) = w in
  let (params, env) = monomorphize_params env params in
  let (body, env) = monomorphize_stmt env body in
  (MTypedWhen (name, params, body), env)

and monomorphize_params (env: env) (params: value_parameter list): (mvalue_parameter list * env) =
  match params with
  | first::rest ->
     let (first, env) = monomorphize_param env first in
     let (rest, env) = monomorphize_params env rest in
     (first :: rest, env)
  | [] ->
     ([], env)

and monomorphize_param (env: env) (param: value_parameter): (mvalue_parameter * env) =
  let (ValueParameter (name, ty)) = param in
  let (ty, env) = strip_and_mono env ty in
  (MValueParameter (name, ty), env)

and monomorphize_named_ty_list (env: env) (tys: (identifier * stripped_ty) list): ((identifier * mono_ty) list * env) =
  match tys with
  | (name, first)::rest ->
     let (first, env) = monomorphize_ty env first in
     let (rest, env) = monomorphize_named_ty_list env rest in
     ((name, first) :: rest, env)
  | [] ->
     ([], env)

(* Monomorphize declarations *)

let monomorphize_decl (env: env) (decl: typed_decl): (mdecl option * env) =
  match decl with
  | TConstant (id, _, name, ty, value, _) ->
    (* Constant are intrinsically monomorphic, and can be monomorphized
       painlessly. *)
    let (ty, env) = strip_and_mono env ty in
    let (value, env) = monomorphize_expr env value in
    let decl = MConstant (id, name, ty, value) in
    (Some decl, env)
  | TTypeAlias (id, _, name, typarams, _, ty, _) ->
    (* Concrete (i.e., no type parameters) type aliases can be monomorphized
       painlessly. Generic ones are monomorphized on demand. *)
    (match typarams with
     | [] ->
       let (ty, env) = strip_and_mono env ty in
       let decl = MTypeAlias (id, name, ty) in
       (Some decl, env)
     | _ ->
       (None, env))
  | _ ->
    err "not implemented yet"
