open Identifier
open Env
open EnvTypes
open EnvUtils
open Type
open TypeStripping
open MonoType
open TypeBindings
open TypeReplace
open TypeParameters
open TypeMatch
open Tast
open TypingPass
open Mtast
open Linked
open Id
open Reporter
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
  | SRegionTy r ->
     (MonoRegionTy r, env)
  | SStaticArray elem_ty ->
     let (elem_ty, env) = monomorphize_ty env elem_ty in
     (MonoStaticArray elem_ty, env)
  | SReadRef (ty, region) ->
     let (ty, env) = monomorphize_ty env ty in
     let (region, env) = monomorphize_ty env region in
     (MonoReadRef (ty, region), env)
  | SWriteRef (ty, region) ->
     let (ty, env) = monomorphize_ty env ty in
     let (region, env) = monomorphize_ty env region in
     (MonoWriteRef (ty, region), env)
  | SAddress ty ->
     let (ty, env) = monomorphize_ty env ty in
     (MonoAddress ty, env)
  | SPointer ty ->
     let (ty, env) = monomorphize_ty env ty in
     (MonoPointer ty, env)
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
  | SMonoTy id ->
     (MonoNamedType id, env)
  | SRegionTyVar (name, source) ->
     (MonoRegionTyVar (name, source), env)

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

let strip_and_mono_list (env: env) (tys: ty list): (mono_ty list * env) =
  let tys = List.map strip_type tys in
  monomorphize_ty_list env tys

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
  | TConstVar (name, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MConstVar (name, ty), env)
  | TParamVar (name, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MParamVar (name, ty), env)
  | TLocalVar (name, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MLocalVar (name, ty), env)
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
       (MConcreteFuncall (decl_id, name, args, rt), env)
  | TMethodCall (ins_meth_id, name, typarams, args, rt, substs) ->
     with_frame "Monomorphizing method call"
       (fun _ ->
         (* Monomorphize the return type. *)
         let (rt, env) = strip_and_mono env rt in
         (* Monomorphize the arglist *)
         let (args, env) = monomorphize_expr_list env args in
         (* Does the funcall have a list of type params? *)
         if typarams_size typarams > 0 then
           (* The instance is generic. *)
           (* Monomorphize the tyargs *)
           let _ = ps ("Substitutions", "[" ^ (String.concat ", " (List.map (fun (n, t) -> (ident_string n) ^ " : " ^ (type_string t)) substs)) ^ "]") in
           let tyargs = List.map (fun (_, ty) -> strip_type ty) substs in
           let (tyargs, env) = monomorphize_ty_list env tyargs in
           ps ("Type arguments", String.concat ", " (List.map show_mono_ty tyargs));
           let (env, mono_id) = add_or_get_instance_method_monomorph env ins_meth_id tyargs in
           (MGenericMethodCall (ins_meth_id, mono_id, args, rt), env)
         else
           (* The instance is concrete. *)
           (MConcreteMethodCall (ins_meth_id, name, args, rt), env))
  | TVarMethodCall { dispatch_ty; typeclass_id; source_module_name; params; args; bindings; method_name; rt; } ->
     (* Continue the instance resolution process we halted in the typing pass. *)
     let (instance, instance_bindings): decl * type_bindings =
       (match get_instance env source_module_name dispatch_ty typeclass_id with
        | Some (i, b) -> (i, b)
        | None ->
           err ("Typeclass resolution failed. Dispatch type: "
                ^ (type_string dispatch_ty)))
     in
     let params' = List.map (fun (ValueParameter (n, t)) -> ValueParameter (n, replace_variables instance_bindings t)) params in
     let arguments' = cast_arguments instance_bindings params' args in
     let typarams = (match instance with
                     | Instance { typarams; _ } -> typarams
                     | _ -> err "Internal")
     in
     let instance_id: decl_id = decl_id instance in
     let meth_id: ins_meth_id =
       (match get_instance_method_from_instance_id_and_method_name env instance_id (original_name method_name) with
        | Some (InsMethRec { id; _ }) -> id
        | None -> err "Internal")
     in
     ps ("Bindings", show_bindings bindings);
     let substs = make_substs instance_bindings typarams in
     let _ = TMethodCall (meth_id, method_name, typarams, arguments', rt, substs) in
     internal_err "derp"
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
  | TBorrowExpr (mode, name, region, ty) ->
     let (ty, env) = strip_and_mono env ty in
     (MBorrowExpr (mode, name, region, ty), env)

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

let rec monomorphize_decl (env: env) (decl: typed_decl): (mdecl option * env) =
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
        immediately. Generic ones are monomorphized on demand. *)
     if (typarams_size typarams) = 0 then
       let (ty, env) = strip_and_mono env ty in
       let decl = MTypeAlias (id, name, ty) in
       (Some decl, env)
     else
       (None, env)
  | TRecord (id, _, name, typarams, _, slots, _) ->
     (* Concrete records are monomorphized immediately. Generic records are
        monomorphized on demand. *)
     if (typarams_size typarams) = 0 then
       let (env, slots) = monomorphize_slots env slots in
       let decl = MRecord (id, name, slots) in
       (Some decl, env)
     else
       (None, env)
  | TUnion (id, _, name, typarams, _, cases, _) ->
     (* Concrete unions are monomorphized immediately. Generic unions are
        monomorphized on demand. *)
     if (typarams_size typarams) = 0 then
       let (env, cases) = Util.map_with_context (fun (e, c) -> monomorphize_case e c) env cases in
       let decl = MUnion (id, name, cases) in
       (Some decl, env)
     else
       (None, env)
  | TFunction (id, _, name, typarams, value_params, rt, body, _) ->
     (* Concrete functions are monomorphized immediately. Generic functions are
        monomorphized on demand. *)
     with_frame ("Monomorphizing function: " ^ (ident_string name))
       (fun _ ->
         if (typarams_size typarams) = 0 then
           let (env, params) = monomorphize_params env value_params in
           let (rt, env) = strip_and_mono env rt in
           let (body, env) = monomorphize_stmt env body in
           let decl = MFunction (id, name, params, rt, body) in
           (Some decl, env)
         else
           (None, env))
  | TForeignFunction (id, _, name, params, rt, underlying, _) ->
     (* Foreign functions are intrinsically monomorphic. *)
     let (env, params) = monomorphize_params env params in
     let (rt, env) = strip_and_mono env rt in
     let decl = MForeignFunction (id, name, params, rt, underlying) in
     (Some decl, env)
  | TTypeClass _ ->
     (* Type classes are purely "informative" declarations: they have no physical
        existence in the code. *)
     (None, env)
  | TInstance (decl_id, _, name, typarams, argument, methods, _) ->
     (* Concrete instances can be monomorphized immediately. *)
     if (typarams_size typarams) = 0 then
       let (argument, env) = strip_and_mono env argument in
       let (env, methods) = monomorphize_methods env methods in
       let decl = MConcreteInstance (decl_id, name, argument, methods) in
       (Some decl, env)
     else
       (None, env)

and monomorphize_slot (env: env) (slot: typed_slot): (env * mono_slot) =
  let (TypedSlot (name, ty)) = slot in
  let (ty, env) = strip_and_mono env ty in
  (env, MonoSlot (name, ty))

and monomorphize_slots (env: env) (slots: typed_slot list): (env * mono_slot list) =
  Util.map_with_context (fun (e, s) -> monomorphize_slot e s) env slots

and monomorphize_case (env: env) (case: linked_case): (env * mono_case) =
  let (LCase (_, name, slots)) = case in
  let (env, slots) = monomorphize_slots env slots in
  (env, MonoCase (name, slots))

and monomorphize_param (env: env) (param: value_parameter): (env * mvalue_parameter) =
  let (ValueParameter (name, ty)) = param in
  let (ty, env) = strip_and_mono env ty in
  (env, MValueParameter (name, ty))

and monomorphize_params (env: env) (params: value_parameter list): (env * mvalue_parameter list) =
  Util.map_with_context (fun (e, p) -> monomorphize_param e p) env params

and monomorphize_methods (env: env) (methods: typed_method_def list): (env * concrete_method list) =
  Util.map_with_context (fun (e, m) -> monomorphize_method e m) env methods

and monomorphize_method (env: env) (meth: typed_method_def): (env * concrete_method) =
  let (TypedMethodDef (id, name, params, rt, body)) = meth in
  let (env, params) = monomorphize_params env params in
  let (rt, env) = strip_and_mono env rt in
  let (body, env) = monomorphize_stmt env body in
  (env, MConcreteMethod (id, name, params, rt, body))

(* Monomorphize modules *)

let rec monomorphize (env: env) (m: typed_module): (env * mono_module) =
  with_frame "Monomorphizing module"
    (fun _ ->
      (* Monomorphize what we can: concrete definitions. *)
      let (TypedModule (module_name, decls)) = m in
      let (env, declopts) =
        Util.map_with_context (fun (e, d) -> let (d, e) = monomorphize_decl e d in (e, d)) env decls in
      let decls: mdecl list = List.filter_map (fun x -> x) declopts in
      (* Recursively collect and instantiate monomorphs until everything's instantiated. *)
      let (env, decls'): (env * mdecl list) = instantiate_monomorphs_until_exhausted env in
      (env, MonoModule (module_name, decls @ decls')))

and instantiate_monomorphs_until_exhausted (env: env): (env * mdecl list) =
  with_frame "Instantiating monomorphs until exhausted"
    (fun _ ->
      (* Get uninstantiated monomorphs from the environment. *)
      let monos: monomorph list = get_uninstantiated_monomorphs env in
      match monos with
      | first::rest ->
         (* If there are uninstantiated monomorphs, instantite them, and repeat the
            process. *)
         let (env, decls): (env * mdecl list) = instantiate_monomorphs env (first::rest) in
         let (env, decls') : (env * mdecl list) = instantiate_monomorphs_until_exhausted env in
         (env, decls @ decls')
      | [] ->
         (* If there are no uninstantiated monomorphs, we're done. *)
         (env, []))

and instantiate_monomorphs (env: env) (monos: monomorph list): (env * mdecl list) =
  (* Instantiate a list of monomorphs. *)
  Util.map_with_context (fun (e, m) -> instantiate_monomorph e m) env monos

and instantiate_monomorph (env: env) (mono: monomorph): (env * mdecl) =
  let qname = get_mono_qname env mono in
  match mono with
  | MonoTypeAliasDefinition { id; type_id; tyargs; _ } ->
     (* Find the type alias declaration and extract the type parameters and the
        definition. *)
     let (typarams, ty) = get_type_alias_definition env type_id in
     (* Search/replace the type variables in `def` with the type arguments from
        this monomorph. *)
     let ty: ty = replace_type_variables typarams qname tyargs ty in
     (* Strip and monomorphize the type. *)
     let (ty, env) = strip_and_mono env ty in
     (* Store the monomorphic type in the environment. *)
     let env = store_type_alias_monomorph_definition env id ty in
     (* Construct a monomorphic type alias decl. *)
     let decl: mdecl = MTypeAliasMonomorph (id, ty) in
     (* Return the new environment and the declaration. *)
     (env, decl)
  | MonoRecordDefinition { id; type_id; tyargs; _ } ->
     with_frame "Instantiating record monomorph"
       (fun _ ->
         ps ("Monomorph ID", show_mono_id id);
         ps ("Type arguments", String.concat ", " (List.map show_mono_ty tyargs));
         (* Find the record definition and extract the type parameters and the slot
            list. *)
         let (typarams, slots) = get_record_definition env type_id in
         ps ("Type parameters", String.concat ", " (List.map show_type_parameter (typarams_as_list typarams)));
         (* Search/replace the type variables in the slot list with the type
            arguments from this monomorph. *)
         let slots: typed_slot list =
           List.map
             (fun (TypedSlot (name, ty)) ->
               (* TODO: extract decl name, make it a qname, pass that here *)
               TypedSlot (name, replace_type_variables typarams qname tyargs ty))
             slots
         in
         (* Strip and monomorphize the slot list. *)
         let (env, slots): (env * mono_slot list) = monomorphize_slot_list env slots in
         (* Store the monomorphic slot list in the environment. *)
         let env = store_record_monomorph_definition env id slots in
         (* Construct a monomorphic record decl. *)
         let decl: mdecl = MRecordMonomorph (id, slots) in
         (* Return the new environment and the declaration. *)
         (env, decl))
  | MonoUnionDefinition { id; type_id; tyargs; _ } ->
     (* Find the list of type parameters from the union definition. *)
     let typarams = get_union_typarams env type_id in
     (* Find the list of cases from the env. *)
     let cases: typed_case list = get_union_typed_cases env type_id in
     (* Search/replace the type variables in the case list with the type
        arguments from this monomorph. *)
     let cases: typed_case list =
       List.map
         (fun (TypedCase (name, slots)) ->
           let slots: typed_slot list =
             List.map
               (fun (TypedSlot (name, ty)) ->
                 TypedSlot (name, replace_type_variables typarams qname tyargs ty))
               slots
           in
           TypedCase (name, slots))
         cases
     in
     (* Strip and monomorphize the case list. *)
     let (env, cases): (env * mono_case list) = monomorphize_case_list env cases in
     (* Store the monomorphic slot list in the environment. *)
     let env = store_union_monomorph_definition env id cases in
     (* Construct a monomorphic record decl. *)
     let decl: mdecl = MUnionMonomorph (id, cases) in
     (* Return the new environment and the declaration. *)
     (env, decl)
  | MonoFunction { id; function_id; tyargs; _ } ->
     (* Find the function's type parameters, value parameters, return type, and
        body. *)
     let (typarams, params, rt, body) = get_function_definition env function_id in
     (* Search/replace the type variables in the parameter list with the type
        arguments. *)
     let params: value_parameter list =
       List.map
         (fun (ValueParameter (name, ty)) ->
           ValueParameter (name, replace_type_variables typarams qname tyargs ty))
         params
     in
     (* Monomorphize the parameter list. *)
     let (env, params) = monomorphize_params env params in
     (* Search/replace the type variables in the return type. *)
     let rt: ty = replace_type_variables typarams qname tyargs rt in
     (* Monomorphize the return type. *)
     let (rt, env) = strip_and_mono env rt in
     (* Search/replace the type variables in the body with the type
        arguments. *)
     let bindings = make_bindings typarams qname tyargs in
     let body = replace_tyvars_stmt bindings body in
     (* Monomorphize the body *)
     let (body, env) = monomorphize_stmt env body in
     (* Store the monomorphic body in the environment. *)
     let env = store_function_monomorph_definition env id body in
     (* Construct a monomorphic record decl. *)
     let decl: mdecl = MFunctionMonomorph (id, params, rt, body) in
     (* Return the new environment and the declaration. *)
     (env, decl)
  | MonoInstanceMethod { id; method_id; tyargs; _ } ->
     with_frame "Instantiating instance method monomorph"
       (fun _ ->
         (* Find the methods's type parameters, value parameters, return type, and
            body. *)
         let (typarams, params, rt, body) = get_method_definition env method_id in
         (* Search/replace the type variables in the parameter list with the type
            arguments. *)
         let params: value_parameter list =
           List.map
             (fun (ValueParameter (name, ty)) ->
               ValueParameter (name, replace_type_variables typarams qname tyargs ty))
             params
         in
         (* Monomorphize the parameter list. *)
         let (env, params) = monomorphize_params env params in
         (* Search/replace the type variables in the return type. *)
         let rt = replace_type_variables typarams qname tyargs rt in
         (* Monomorphize the return type. *)
         let (rt, env) = strip_and_mono env rt in
         (* Search/replace the type variables in the body with the type
            arguments. *)
         let bindings = make_bindings typarams qname tyargs in
         let body = replace_tyvars_stmt bindings body in
         (* Monomorphize the body *)
         let (body, env) = monomorphize_stmt env body in
         (* Store the monomorphic body in the environment. *)
         let env = store_instance_method_monomorph_definition env id body in
         (* Construct a monomorphic record decl. *)
         let decl: mdecl = MMethodMonomorph (id, params, rt, body) in
         (* Return the new environment and the declaration. *)
         (env, decl))

(* Utils *)

(** Get the qualfied identifier of a monomorph's declaration, the same one
    that's used for the type parameter list. *)
and get_mono_qname (env: env) (mono: monomorph): qident =
  let get_module_name (id: mod_id): module_name =
    match get_module_by_id env id with
    | Some (ModRec { name; _ }) -> name
    | None -> err "internal"
  in
  match mono with
  | MonoTypeAliasDefinition { type_id; _ } ->
     (match get_decl_by_id env type_id with
      | Some (TypeAlias { name; mod_id; _ }) ->
         make_qident (get_module_name mod_id, name, name)
      | _ ->
         err "internal")
  | MonoRecordDefinition { type_id; _ } ->
     (match get_decl_by_id env type_id with
      | Some (Record { name; mod_id; _ }) ->
         make_qident (get_module_name mod_id, name, name)
      | _ ->
         err "internal")
  | MonoUnionDefinition { type_id; _ } ->
     (match get_decl_by_id env type_id with
      | Some (Union { name; mod_id; _ }) ->
         make_qident (get_module_name mod_id, name, name)
      | _ ->
         err "internal")
  | MonoFunction { function_id; _ } ->
     (match get_decl_by_id env function_id with
      | Some (Function { name; mod_id; _ }) ->
         make_qident (get_module_name mod_id, name, name)
      | _ ->
         err "internal")
  | MonoInstanceMethod { method_id;  _ } ->
     (match get_instance_method env method_id with
      | Some (InsMethRec { instance_id; _ }) ->
         (match get_decl_by_id env instance_id with
          | Some (Instance { mod_id; typeclass_id; _ }) ->
             (match get_decl_by_id env typeclass_id with
              | (Some (TypeClass { name; _ })) ->
                 make_qident (get_module_name mod_id, name, name)
              | _ ->
                 err "internal")
          | _ ->
             err "internal")
      | _ ->
         err "internal")

and get_type_alias_definition (env: env) (id: decl_id): (typarams * ty) =
  match get_decl_by_id env id with
  | Some (TypeAlias { typarams; def; _ }) ->
     (typarams, def)
  | _ ->
     err "internal"

and get_record_definition (env: env) (id: decl_id): (typarams * typed_slot list) =
  match get_decl_by_id env id with
  | Some (Record { typarams; slots; _ }) ->
     (typarams, slots)
  | _ ->
     err "internal"

and get_union_typarams (env: env) (id: decl_id): typarams =
  match get_decl_by_id env id with
  | Some (Union { typarams; _ }) ->
     typarams
  | _ ->
     err "internal"

and get_union_typed_cases (env: env) (union_id: decl_id): typed_case list =
  let cases: decl list = get_union_cases env union_id in
  let mapper (decl: decl): typed_case =
    match decl with
    | UnionCase { name; slots; _ } ->
       TypedCase (name, slots)
    | _ ->
       err "Internal: not a union"
  in
  List.map mapper cases

and get_function_definition (env: env) (id: decl_id): (typarams * value_parameter list * ty * tstmt) =
  match get_decl_by_id env id with
  | Some (Function { typarams; value_params; rt; body; _ }) ->
     (match body with
      | Some body ->
         (typarams, value_params, rt, body)
      | None ->
         err "internal: function has no body")
  | _ ->
     err "internal"

and get_method_definition (env: env) (id: ins_meth_id): (typarams * value_parameter list * ty * tstmt) =
  match get_instance_method env id with
  | Some (InsMethRec { instance_id; value_params; rt; body; _ }) ->
     (match get_decl_by_id env instance_id with
      | Some (Instance { typarams; _ }) ->
         (match body with
          | Some body ->
             (typarams, value_params, rt, body)
          | None ->
             err "internal: method has no body")
      | _ ->
         err "internal: not an instance")
  | _ ->
     err "internal"

and monomorphize_slot_list (env: env) (slots: typed_slot list): (env * mono_slot list) =
  let names: identifier list = List.map (fun (TypedSlot (n, _)) -> n) slots in
  let (tys, env): (mono_ty list * env) = strip_and_mono_list env (List.map (fun (TypedSlot (_, t)) -> t) slots) in
  let (slots: mono_slot list) = List.map2 (fun name ty -> MonoSlot (name, ty)) names tys in
  (env, slots)

and monomorphize_case_list (env: env) (cases: typed_case list): (env * mono_case list) =
  Util.map_with_context
    (fun (env, TypedCase (name, slots)) ->
      let (env, slots) = monomorphize_slot_list env slots in
      (env, MonoCase (name, slots)))
    env
    cases

and replace_type_variables (typarams: typarams) (source: qident) (args: mono_ty list) (ty: ty): ty =
  with_frame "Replacing type variables"
    (fun _ ->
      ps ("Type parameters", String.concat ", " (List.map show_type_parameter (typarams_as_list typarams)));
      ps ("Type arguments", "[" ^ (String.concat ", " (List.map show_mono_ty args)) ^ "]");
      ps ("Type", show_ty ty);
      let bindings: type_bindings = make_bindings typarams source args in
      replace_variables bindings ty)

and make_bindings (typarams: typarams) (source: qident) (args: mono_ty list): type_bindings =
  with_frame "Make bindings"
    (fun _ ->
      (* Given a list of type parameters, a list of monomorphic type arguments (of
         qthe same length), return a type bindings object (implicitly converting the
         `mono_ty` into a `ty`).

         Ideally we shouldn't need to bring the type parameters, rather, monomorphs
         should be stored in the environment with an `(identifier, mono_ty)` map
         rather than as a bare list of monomorphic type arguments. *)
      let is_not_region (TypeParameter (_, u, _, _)): bool =
        u <> RegionUniverse
      in
      let typarams = List.filter is_not_region (typarams_as_list typarams) in
      if (List.length typarams) = (List.length args) then
        let triples: (identifier * qident * ty) list =
          List.map2
            (fun typaram mty ->
              let (TypeParameter (name, _, _, _)) = typaram in
              (name, source, mono_to_ty mty))
            typarams
            args
        in
        let _ = ps ("Triples", String.concat ", " (List.map (fun (n, q, t) -> "(" ^ (ident_string n) ^ ", " ^ (qident_debug_name q) ^ ", " ^ (show_ty t) ^ ")") triples)) in
        let b = bindings_from_list triples in
        ps ("Bindings", show_bindings b);
        b
      else
        err ("Parameter list and argument list don't have the same length:\n\nparameters:\n"
             ^ (String.concat ", " (List.map show_type_parameter typarams))
             ^ "\narguments:\n"
             ^ (String.concat ", " (List.map show_mono_ty args))))

and mono_to_ty (ty: mono_ty): ty =
  let r = mono_to_ty in
  match ty with
  | MonoUnit -> Unit
  | MonoBoolean -> Boolean
  | MonoInteger (s, w) -> Integer (s, w)
  | MonoSingleFloat -> SingleFloat
  | MonoDoubleFloat -> DoubleFloat
  | MonoNamedType mono_id ->
     (* SPECIAL CASE *)
     MonoTy mono_id
  | MonoStaticArray elem_ty ->
     StaticArray (r elem_ty)
  | MonoRegionTy r ->
     RegionTy r
  | MonoReadRef (ty, region) ->
     ReadRef (r ty, r region)
  | MonoWriteRef (ty, region) ->
     WriteRef (r ty, r region)
  | MonoAddress ty ->
     Address (r ty)
  | MonoPointer ty ->
     Pointer (r ty)
  | MonoRegionTyVar (name, source) ->
     TyVar (TypeVariable (name, RegionUniverse, source, []))
