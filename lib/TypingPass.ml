(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Identifier
open Span
open Common
open Type
open TypeSystem
open TypeBindings
open TypeMatch
open TypeParser
open TypeParameter
open TypeParameters
open Region
open RegionMap
open LexEnv
open Env
open EnvTypes
open EnvUtils
open Stages.AstDB
open Stages.Tast
open TastUtil
open Stages.Linked
open Util
open Reporter
open Error

module Errors = TypeErrors

type stmt_ctx = StmtCtx of module_name * env * region_map * typarams * lexenv * ty

let update_lexenv (StmtCtx (mn, menv, rm, typarams, _, rt)) (lexenv: lexenv): stmt_ctx =
  StmtCtx (mn, menv, rm, typarams, lexenv, rt)

let update_rm (StmtCtx (mn, menv, _, typarams, lexenv, rt)) (rm: region_map): stmt_ctx =
  StmtCtx (mn, menv, rm, typarams, lexenv, rt)

(* Since the extraction pass has already happened, we can simplify the call to
   `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (env: env) (rm: region_map) (typarams: typarams) (ty: qtypespec): ty =
  parse_type env [] rm typarams ty

let augment_expr (module_name: module_name) (env: env) (rm: region_map) (typarams: typarams) (lexenv: lexenv) (asserted_ty: ty option) (expr: aexpr): texpr =
  let ctx: TypeCheckExpr.expr_ctx = TypeCheckExpr.make_ctx module_name env rm typarams lexenv in
  TypeCheckExpr.augment_expr ctx asserted_ty expr

(** Copied from TypeCheckExpr *)
let get_record_definition (env: env) (name: qident) (ty: ty): (module_name * type_vis * typarams * typed_slot list) =
  match get_decl_by_name env (qident_to_sident name) with
  | (Some (Record { mod_id; vis; typarams; slots; _ })) ->
     let mod_name: module_name = module_name_from_id env mod_id in
     (mod_name, vis, typarams, slots)
  | Some _ ->
     Errors.path_not_record ty
  | None ->
     err ("No record with this name: " ^ (ident_string (original_name name)))

let stmt_kind (stmt: astmt): string =
  match stmt with
  | ASkip _ -> "skip"
  | ALet _ -> "let"
  | ADestructure _ -> "let (destructure)"
  | AAssign _ -> "assign"
  | AIf _ -> "if"
  | AWhen _ -> "if (no else)"
  | ACase _ -> "case"
  | AWhile _ -> "while"
  | AFor _ -> "for"
  | ABorrow _ -> "borrow"
  | ABlock _ -> "block"
  | ADiscarding _ -> "discard"
  | AReturn _ -> "return"
  | LetTmp _ -> "let"
  | AssignTmp _ -> "assign"
  | AAssignVar _ -> "assign"
  | AInitialAssign _ -> "assign"

let is_boolean = function
  | Boolean -> true
  | _ -> false

let is_compatible_with_index_type = function
  | TIntConstant _ ->
     true
  | e ->
     (get_type e) = index_type

type case_mode =
  | NormalCaseMode
  | ReadRefCaseMode of ty
  | WriteRefCaseMode of ty

let check_var_doesnt_collide_with_decl (ctx: stmt_ctx) (name: identifier) =
  let (StmtCtx (module_name, env, _, _, _, _)) = ctx in
  let sname: sident = make_sident module_name name in
  match get_decl_by_name env sname with
  | Some _ ->
     Errors.var_collides_with_decl name
  | None ->
     ()

let rec augment_stmt (ctx: stmt_ctx) (stmt: astmt): tstmt =
  with_frame ("Augment statement: " ^ (stmt_kind stmt))
    (fun _ ->
      let (StmtCtx (module_name, env, rm, typarams, lexenv, rt)) = ctx in
      match stmt with
      | ASkip span ->
         TSkip span
      | ALet (span, mut, name, ty, body) ->
         pi ("Name", name);
         adorn_error_with_span span
           (fun _ ->
             let _ = check_var_doesnt_collide_with_decl ctx name in
             let ty = parse_typespec env rm typarams ty in
             pt ("Type", ty);
             let lexenv' = push_var lexenv name ty (VarLocal mut) in
             let body' = augment_stmt (update_lexenv ctx lexenv') body in
             remove_var lexenv name;
             TLet (span, mut, name, ty, body'))
      | LetTmp (name, value) ->
         let value = augment_expr module_name env rm typarams lexenv None value in
         let ty = get_type value in
         let _ = push_var lexenv name ty (VarLocal Mutable) in
         TLetTmp (name, ty, value)
      | AssignTmp (name, value) ->
         let value = augment_expr module_name env rm typarams lexenv None value in
         begin
           match get_var lexenv name with
           | Some (ty, _) ->
              let _ = match_type (env, module_name) ty (get_type value) in
              TAssignTmp (name, value)
           | None ->
              internal_err "Unknown temporary."
         end
      | ADestructure (span, mut, bindings, value, body) ->
         adorn_error_with_span span
           (fun _ ->
             let _ =
               List.map (fun (Stages.Ast.QBinding { rename; _ }) -> check_var_doesnt_collide_with_decl ctx rename) bindings
             in
             let value' = augment_expr module_name env rm typarams lexenv None value in
             (* Check: the value must be a public record type *)
             let rec_ty = get_type value' in
             (match rec_ty with
              | (NamedType (name, _, u)) ->
                 let (source_module, vis, record_typarams, slots) = get_record_definition env name rec_ty in
                 let orig_type = NamedType (
                                     make_qident (source_module, original_name name, original_name name),
                                     List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list record_typarams),
                                     u
                                   )
                 in
                 let typebindings = match_type (env, module_name) orig_type rec_ty in
                 if (vis = TypeVisPublic) || (module_name = source_module) then
                   (* Find the set of slot names and the set of binding names, and compare them *)
                   let binding_names = List.map (fun (Stages.Ast.QBinding { name; _}) -> name) bindings
                   and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
                   if ident_set_eq binding_names slot_names then
                     let bindings': (identifier * qtypespec * ty * identifier) list =
                       group_bindings_slots bindings slots
                     in
                     let bindings'': (identifier * ty * ty * identifier) list
                       = List.map (fun (n, ty, actual, rename) -> (n, parse_typespec env rm typarams ty, replace_variables typebindings actual, rename)) bindings'
                     in
                     let newvars: (identifier * ty * var_source) list =
                       List.map (fun (_, ty, actual, rename) ->
                           let _ = match_type (env, module_name) ty actual in
                           (rename, ty, VarLocal mut))
                         bindings''
                     in
                     let lexenv' = push_vars lexenv newvars in
                     let body' = augment_stmt (update_lexenv ctx lexenv') body in
                     remove_vars lexenv' (List.map (fun (n, _, _) -> n) newvars);
                     TDestructure (
                         span,
                         mut,
                         List.map (fun (name, _, ty, rename) -> TypedBinding { name; ty; rename; }) bindings'',
                         value',
                         body'
                       )
                   else
                     Errors.destructure_wrong_slots rec_ty
                 else
                   Errors.destructure_not_public rec_ty
              | _ ->
                 Errors.destructure_non_record rec_ty))
      | AAssign (span, lvalue, rvalue) ->
         adorn_error_with_span span
           (fun _ ->
             (* Augment both sides. *)
             let lvalue: texpr = augment_expr module_name env rm typarams lexenv None lvalue
             and rvalue: texpr = augment_expr module_name env rm typarams lexenv None rvalue in
             (* The type of the lvalue must be a read reference. *)
             let lvalue_ty: ty = get_type lvalue in
             let pointed_ty: ty = begin
                 match lvalue_ty with
                 | WriteRef (ty, _) ->
                    ty
                 | _ ->
                    err ("Can't assign: lvalue is not a mutable reference: " ^ (type_string lvalue_ty))
               end
             in
             (* The pointed to type and the type of the rvalue must match. *)
             let _ = match_type_with_value (env, module_name) pointed_ty rvalue in
             (* The pointed-to type must be in the free universe. *)
             let _ =
               if (type_universe pointed_ty) = FreeUniverse then
                 ()
               else
                 austral_raise LinearityError [
                     Text "Cannot assign to a path that ends in a type, ";
                     Type pointed_ty;
                     Text ", not in the Free universe."
                   ]
             in
             (* Construct the assignment. *)
             TAssign (span, lvalue, rvalue))
      | AAssignVar (span, name, rvalue) ->
         adorn_error_with_span span
           (fun _ ->
             (* Augment the rvalue. *)
             let rvalue: texpr = augment_expr module_name env rm typarams lexenv None rvalue in
             (* Get the type of the lvalue. *)
             let lvalue_ty: ty = begin
                 let name: identifier = original_name name in
               match get_var lexenv name with
               | (Some (orig_ty, src)) -> begin
                   let _ =
                     match src with
                     | VarLocal Mutable ->
                        ()
                     | VarLocal Immutable ->
                        Errors.cant_assign_to_immutable_var name
                     | VarConstant ->
                        Errors.cannot_assign_to_constant name
                     | VarParam ->
                        Errors.cannot_assign_to_parameter name
                   in
                   orig_ty
                 end
               | None ->
                  Errors.unknown_name ~kind:"variable" ~name:name
               end
             in
             (* Types must match. *)
             let _ = match_type_with_value (env, module_name) lvalue_ty rvalue in
             (* Construct the assignment. *)
             TAssignVar (span, name, rvalue))
      | AInitialAssign (name, ty, rvalue) ->
         let rvalue: texpr = augment_expr module_name env rm typarams lexenv None rvalue in
         let ty = parse_typespec env rm typarams ty in
         let _ = match_type_with_value (env, module_name) ty rvalue in
         TInitialAssign (name, rvalue)
      | AIf (span, c, t, f) ->
         adorn_error_with_span span
           (fun _ ->
             let c' = augment_expr module_name env rm typarams lexenv None c in
             if is_boolean (get_type c') then
               TIf (span, c', augment_stmt ctx t, augment_stmt ctx f)
             else
               Errors.condition_not_boolean
                 ~kind:"if"
                 ~form:"statement"
                 ~ty:(get_type c'))
      | AWhen (span, c, t) ->
         adorn_error_with_span span
           (fun _ ->
             let c = augment_expr module_name env rm typarams lexenv None c in
             if is_boolean (get_type c) then
               TIf (span, c, augment_stmt ctx t, TSkip span)
             else
               Errors.condition_not_boolean
                 ~kind:"if"
                 ~form:"statement"
                 ~ty:(get_type c))
      | ACase (span, expr, whens) ->
         augment_case ctx span expr whens
      | AWhile (span, c, body) ->
         adorn_error_with_span span
           (fun _ ->
             let c' = augment_expr module_name env rm typarams lexenv None c in
             if is_boolean (get_type c') then
               TWhile (span, c', augment_stmt ctx body)
             else
               Errors.condition_not_boolean
                 ~kind:"while"
                 ~form:"statement"
                 ~ty:(get_type c'))
      | AFor { span; name; initial; final; body; } ->
         adorn_error_with_span span
           (fun _ ->
             let i' = augment_expr module_name env rm typarams lexenv None initial
             and f' = augment_expr module_name env rm typarams lexenv None final in
             if is_compatible_with_index_type i' then
               if is_compatible_with_index_type f' then
                 let lexenv' = push_var lexenv name index_type (VarLocal Immutable) in
                 let b' = augment_stmt (update_lexenv ctx lexenv') body in
                 remove_var lexenv name;
                 TFor (span, name, i', f', b')
               else
                 Errors.for_bounds_non_integral ~bound:"final" ~ty:(get_type f')
             else
               Errors.for_bounds_non_integral ~bound:"initial" ~ty:(get_type i'))
      | ABorrow { span; original; rename; region; body; mode; } ->
         adorn_error_with_span span
           (fun _ ->
             let _ = check_var_doesnt_collide_with_decl ctx rename in
             (match get_var lexenv original with
              | (Some (orig_ty, src)) ->
                 (* Check we can borrow the variable. *)
                 let _ =
                   match (src, mode) with
                   | (_, Read) ->
                      (* Anything can be borrowed immutably. *)
                      ()
                   | (VarLocal Immutable, Write) ->
                      (* Immutable variables cannot be borrowed mutably. *)
                      Errors.cannot_borrow_immutable_var_mutably original
                   | (VarParam, Write) ->
                      (* Parameters cannot be borrowed mutably, unless they're mutable references. *)
                      let is_mut = begin
                        match orig_ty with
                        | WriteRef _ -> true
                        | _ -> false
                      end
                      in
                      if is_mut then
                        ()
                      else
                        Errors.cannot_borrow_param_mutably original
                   | _ ->
                      (* And anything else is fine. *)
                      ()
                 in
                 let region_obj = fresh_region () in
                 let refty =
                   (match mode with
                    | Read ->
                       ReadRef (orig_ty, RegionTy region_obj)
                    | Write ->
                       WriteRef (orig_ty, RegionTy region_obj)
                    | Reborrow ->
                       let pointed_ty = begin
                           match orig_ty with
                           | WriteRef (pointed_ty, _) ->
                              pointed_ty
                           | _ ->
                              err ("Cannot reborrow something that is not a mutable reference: " ^ (type_string orig_ty))
                         end
                       in
                       WriteRef (pointed_ty, RegionTy region_obj))
                 in
                 let lexenv' = push_var lexenv rename refty (VarLocal Immutable) in
                 let rm' = add_region rm region region_obj in
                 let ctx' = update_lexenv ctx lexenv' in
                 let ctx''= update_rm ctx' rm' in
                 let body = augment_stmt ctx'' body in
                 remove_var lexenv rename;
                 TBorrow {
                     span=span;
                     original=original;
                     rename=rename;
                     region=region;
                     orig_type=orig_ty;
                     ref_type=refty;
                     body=body;
                     mode=mode
                   }
              | None ->
                 Errors.unknown_name ~kind:"variable" ~name:original))
      | ABlock (span, f, r) ->
         let a = augment_stmt ctx f in
         let b = augment_stmt ctx r in
         TBlock (span, a, b)
      | ADiscarding (span, e) ->
         adorn_error_with_span span
           (fun _ ->
             let e' = augment_expr module_name env rm typarams lexenv None e in
             ps ("Expression", show_texpr e');
             let u = type_universe (get_type e') in
             if ((u = LinearUniverse) || (u = TypeUniverse)) then
               Errors.discard_linear u
             else
               TDiscarding (span, e'))
      | AReturn (span, e) ->
         adorn_error_with_span span
           (fun _ ->
             let e' = augment_expr module_name env rm typarams lexenv (Some rt) e in
             pt ("Type", get_type e');
             let _ = match_type_with_value (env, module_name) rt e' in
             TReturn (span, e')))

and augment_case (ctx: stmt_ctx) (span: span) (expr: aexpr) (whens: abstract_when list): tstmt =
  let (StmtCtx (module_name, env, rm, typarams, lexenv, _)) = ctx in
  (* Type checking a case statement:

     1. Ensure the value is of a union type (or reference to a union).
     2. Ensure the union type is public or it is defined in this module.
     3. Ensure the set of case names in the case statement equals the set of cases in the union definition.
     4. Iterate over the cases, and ensure the bindings are correct.
   *)
  adorn_error_with_span span
    (fun _ ->
      let expr' = augment_expr module_name env rm typarams lexenv None expr in
      let ty = get_type expr' in
      let ty, mode =
        (match ty with
         | NamedType _ ->
            (ty, NormalCaseMode)
         | ReadRef (ty, r) ->
            (ty, ReadRefCaseMode r)
         | WriteRef (ty, r) ->
            (ty, WriteRefCaseMode r)
         | _ ->
            austral_raise TypeError [
                Text "The type for the expression in a case statement must be a union or a reference to a union, but I got ";
                Type ty
        ])
      in
      let case_ref: case_ref =
        (match mode with
         | NormalCaseMode -> CasePlain
         | ReadRefCaseMode _ -> CaseRef
         | WriteRefCaseMode _ -> CaseRef)
      in
      pt ("Case type", ty);
      let (union_ty, cases) = get_union_type_definition module_name env ty in
      let typebindings = match_type (env, module_name) union_ty ty in
      let case_names = List.map (fun (TypedCase (n, _)) -> n) (List.map union_case_to_typed_case cases) in
      let when_names = List.map (fun (AbstractWhen (n, _, _)) -> n) whens in
      if ident_set_eq case_names when_names then
        (* Group the cases and whens *)
        let whens' = group_cases_whens cases whens in
        let whens'' = List.map (fun (c, w) -> augment_when ctx typebindings w c mode) whens' in
        TCase (span, expr', whens'', case_ref)
      else
        Errors.case_non_exhaustive ())

and get_union_type_definition (importing_module: module_name) (env: env) (ty: ty): (ty * decl list) =
  let name: qident = (match ty with
                      | NamedType (n, _, _) ->
                         n
                      | _ ->
                         Errors.case_non_union ty) in
  match get_decl_by_name env (qident_to_sident name) with
  | Some (Union { id; vis; name; mod_id; typarams; universe; _ }) ->
     let module_name = module_name_from_id env mod_id in
     if (vis = TypeVisPublic) || (importing_module = module_name) then
       let n = make_qident (module_name, name, name)
       and args = List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams)
       in
       let ty = NamedType (n, args, universe)
       and cases = get_union_cases env id in
       (ty, cases)
     else
       Errors.case_non_public ()
  | _ ->
     Errors.case_non_union ty

and group_cases_whens (cases: decl list) (whens: abstract_when list): (typed_case * abstract_when) list =
  List.map (fun (TypedCase (n, s)) -> (TypedCase (n, s), List.find (fun (AbstractWhen (n', _, _)) -> n = n') whens))
    (List.map union_case_to_typed_case cases)

and group_bindings_slots (bindings: qbinding list) (slots: typed_slot list): (identifier * qtypespec * ty * identifier) list =
  let f (binding: qbinding): (identifier * qtypespec * ty * identifier) =
    let Stages.Ast.QBinding { name; ty; rename; } = binding in
    let (TypedSlot (_, ty')) = List.find (fun (TypedSlot (n', _)) -> equal_identifier name n') slots in
    (name, ty, ty', rename)
  in
  List.map f bindings

and augment_when (ctx: stmt_ctx) (typebindings: type_bindings) (w: abstract_when) (c: typed_case) (mode: case_mode): typed_when =
  with_frame "Augment when"
    (fun _ ->
      let (StmtCtx (_, menv, rm, typarams, lexenv, _)) = ctx in
      let (AbstractWhen (name, bindings, body)) = w
      and (TypedCase (_, slots)) = c in
      pi ("Case name", name);
      (* Check the set of binding names is the same as the set of slots *)
      let binding_names = List.map (fun (Stages.Ast.QBinding { name; _ }) -> name) bindings
      and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
      if ident_set_eq binding_names slot_names then
        (* Check the type of each binding matches the type of the slot *)
        let bindings' = group_bindings_slots bindings slots in
        let bindings'' = List.map (fun (n, ty, actual, rename) -> (n, parse_typespec menv rm typarams ty, replace_variables typebindings actual, rename)) bindings' in
        let newvars = List.map (fun (_, user_ty, decl_ty, rename) ->
                          (* Depending on what 'mode' of case statement this is,
                             we may have to modify the slot type. If we're just
                             accessing a union by value, we don't have to do
                             anything, but if we're accessing a union by
                             reference, we don't have access to the slot
                             _values_, we instead get _references_ to those
                             values. Just like when doing `val->x` with `val`
                             having a reference-to-record type gives us a
                             reference to the field `x`, case statements on
                             references give us references to their interior
                             values. *)
                          let decl_ty =
                            (match mode with
                             | NormalCaseMode ->
                                decl_ty
                             | ReadRefCaseMode r ->
                                ReadRef (decl_ty, r)
                             | WriteRefCaseMode r ->
                                WriteRef (decl_ty, r))
                          in
                          if equal_ty decl_ty user_ty then
                            let _ = pi ("Binding name", rename)
                            in
                            pt ("Binding type",  decl_ty);
                            (rename, decl_ty, VarLocal Immutable)
                          else
                            Errors.slot_wrong_type
                              ~name:rename
                              ~expected:decl_ty
                              ~actual:user_ty)
                        bindings'' in
        let lexenv' = push_vars lexenv newvars in
        let body' = augment_stmt (update_lexenv ctx lexenv') body in
        remove_vars lexenv (List.map (fun (n, _, _) -> n) newvars);
        TypedWhen (name, List.map (fun (name, _, ty, rename) -> TypedBinding { name; ty; rename}) bindings'', body')
      else
        Errors.case_wrong_slots ())

let rec validate_constant_expression (expr: texpr): unit =
  if is_constant expr then
    ()
  else
    Errors.expression_not_constant ()

and is_constant = function
  | TNilConstant ->
     true
  | TBoolConstant _ ->
     true
  | TIntConstant _ ->
     true
  | TFloatConstant _ ->
     true
  | TStringConstant _ ->
     true
  | TConstVar _ ->
     true
  | TParamVar _ ->
     false
  | TLocalVar _ ->
     false
  | TTemporary _ ->
     false
  | TFunVar _ ->
     true
  | TFuncall _ ->
     false
  | TMethodCall _ ->
     false
  | TVarMethodCall _ ->
     false
  | TFptrCall _ ->
     false
  | TCast _ ->
     true
  | TComparison (_, lhs, rhs) ->
     (is_constant lhs) && (is_constant rhs)
  | TConjunction (lhs, rhs) ->
     (is_constant lhs) && (is_constant rhs)
  | TDisjunction (lhs, rhs) ->
     (is_constant lhs) && (is_constant rhs)
  | TNegation e ->
     is_constant e
  | TIfExpression (c, t, f) ->
     (is_constant c) && (is_constant t) && (is_constant f)
  | TRecordConstructor (_, values) ->
     List.for_all (fun (_, v) -> is_constant v) values
  | TUnionConstructor (_, _, values) ->
     List.for_all (fun (_, v) -> is_constant v) values
  | TEmbed _ ->
     true
  | TDeref _ ->
     false
  | TSizeOf _ ->
     true
  | TSlotAccessor _ ->
     true
  | TPointerSlotAccessor _ ->
     true
  | TArrayIndex (_, idx, _) ->
     is_constant idx

let rec augment_decl (module_name: module_name) (kind: module_kind) (env: env) (decl: linked_definition): typed_decl =
  with_frame "Augment declaration"
    (fun _ ->
      let rm = empty_region_map in
      match decl with
      | LConstant (decl_id, vis, name, ty, expr, doc) ->
         ps ("Kind", "Constant");
         let expr' = augment_expr module_name env rm empty_typarams (empty_lexenv ()) (Some ty) expr in
         let _ = match_type_with_value (env, module_name) ty expr' in
         let _ = validate_constant_expression expr' in
         TConstant (decl_id, vis, name, ty, expr', doc)
      | LRecord (decl_id, vis, name, typarams, universe, slots, doc) ->
         ps ("Kind", "Record");
         TRecord (decl_id, vis, name, typarams, universe, slots, doc)
      | LUnion (decl_id, vis, name, typarams, universe, cases, doc) ->
         ps ("Kind", "Union");
         TUnion (decl_id, vis, name, typarams, universe, cases, doc)
      | LFunction (decl_id, vis, name, typarams, params, rt, body, doc, pragmas) ->
         ps ("Kind", "Function");
         pi ("Name", name);
         let ctx = StmtCtx (module_name, env, rm, typarams, (lexenv_from_params params), rt) in
         let _ =
           List.map (fun (ValueParameter (name, _)) -> check_var_doesnt_collide_with_decl ctx name) params
         in
         (match pragmas with
          | [ForeignImportPragma s] ->
             if typarams_size typarams = 0 then
               if kind = UnsafeModule then
                 TForeignFunction (decl_id, vis, name, params, rt, s, doc)
               else
                 Errors.foreign_in_safe_module ()
             else
               Errors.foreign_type_parameters ()
          | [ForeignExportPragma _] ->
             let body' = augment_stmt ctx body in
             TFunction (decl_id, vis, name, typarams, params, rt, body', doc)
          | [] ->
             let body' = augment_stmt ctx body in
             TFunction (decl_id, vis, name, typarams, params, rt, body', doc)
          | _ ->
             Errors.fun_invalid_pragmas ())
      | LTypeclass (decl_id, vis, name, typaram, methods, doc) ->
         ps ("Kind", "Typeclass");
         TTypeClass (decl_id, vis, name, typaram, List.map (augment_method_decl env rm typaram) methods, doc)
      | LInstance (decl_id, vis, name, typarams, arg, methods, doc) ->
         ps ("Kind", "Instance");
         (* TODO: the universe of the type parameter matches the universe of the type argument *)
         (* TODO: Check the methods in the instance match the methods in the class *)
         TInstance (decl_id, vis, name, typarams, arg, List.map (augment_method_def module_name env rm typarams) methods, doc))

and lexenv_from_params (params: value_parameter list): lexenv =
  match params with
  | (ValueParameter (n, t))::rest ->
     push_var (lexenv_from_params rest) n t VarParam
  | [] ->
     empty_lexenv ()

and augment_method_decl _ _ _ (LMethodDecl (decl_id, name, params, rt, _)) =
  TypedMethodDecl (decl_id, name, params, rt)

and augment_method_def module_name menv rm typarams (LMethodDef (ins_meth_id, name, params, rt, _, body)) =
  let ctx = StmtCtx (module_name, menv, rm, typarams, (lexenv_from_params params), rt) in
  let _ =
    List.map (fun (ValueParameter (name, _)) -> check_var_doesnt_collide_with_decl ctx name) params
  in
  let body' = augment_stmt ctx body in
  TypedMethodDef (ins_meth_id, name, params, rt, body')

let augment_module menv (LinkedModule { name; decls; kind; _ }) =
  with_frame "Typing pass"
    (fun _ ->
      let decls' = List.map (augment_decl name kind menv) decls in
      TypedModule (name, decls'))
