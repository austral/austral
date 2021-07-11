open Identifier
open Type
open TypeSystem
open TypeBindings
open TypeMatch
open TypeVarSet
open TypeParser
open Region
open LexEnv
open ModuleSystem
open Ast
open Tast
open Combined
open Semantic
open Util
open Error

let rec augment_expr (module_name: module_name) (menv: menv) (lexenv: lexenv) (asserted_ty: ty option) (expr: aexpr): texpr =
  let aug = augment_expr module_name menv lexenv None in
  match expr with
  | NilConstant ->
     TNilConstant
  | BoolConstant b ->
     TBoolConstant b
  | IntConstant i ->
     TIntConstant i
  | FloatConstant f ->
     TFloatConstant f
  | StringConstant s ->
     TStringConstant s
  | Variable name ->
     (match get_var lexenv name with
      | Some ty ->
         TVariable (name, ty)
      | None ->
         err ("I can't find the variable named " ^ (ident_string name)))
  | FunctionCall (name, args) ->
     augment_call module_name menv asserted_ty name (augment_arglist module_name menv lexenv None args)
  | ArithmeticExpression (op, lhs, rhs) ->
     let lhs' = aug lhs
     and rhs' = aug rhs in
     if (get_type lhs') = (get_type rhs') then
       (* If the types are the same type, check it is a numeric type. *)
       if (is_numeric (get_type lhs')) then
         TArithmetic (op, lhs', rhs')
       else
         err "Both operands to an arithmetic expression must be comparable types."
     else
       (* If the types are different, check if at least one operator is a constant.*)
       let are_int_constants = (is_int_constant lhs) || (is_int_constant rhs)
       and are_float_constants = (is_float_constant lhs) || (is_float_constant rhs) in
       if (are_int_constants || are_float_constants) then
         (* If either operand is a constant, let it pass *)
         TArithmetic (op, lhs', rhs')
       else
         err "Both operands to an arithmetic expression must be of the same type"
  | Comparison (op, lhs, rhs) ->
     let lhs' = aug lhs
     and rhs' = aug rhs in
     if (get_type lhs') = (get_type rhs') then
       (* If the types are the same type, check it is a comparable type. *)
       if (is_comparable (get_type lhs')) then
         TComparison (op, lhs', rhs')
       else
         err "Both operands to a comparison expression must be comparable types."
     else
       (* If the types are different, check if at least one operator is a constant.*)
       let are_int_constants = (is_int_constant lhs) || (is_int_constant rhs)
       and are_float_constants = (is_float_constant lhs) || (is_float_constant rhs) in
       if (are_int_constants || are_float_constants) then
         (* If either operand is a constant, let it pass *)
         TComparison (op, lhs', rhs')
       else
         err "Both operands to a comparison expression must be of the same type"
  | Conjunction (lhs, rhs) ->
     let lhs' = aug lhs
     and rhs' = aug rhs in
     if ((is_bool lhs') && (is_bool rhs')) then
       TConjunction (lhs', rhs')
     else
       err "Both operands to a logical expression must be boolean-typed expressions."
  | Disjunction (lhs, rhs) ->
     let lhs' = aug lhs
     and rhs' = aug rhs in
     if ((is_bool lhs') && (is_bool rhs')) then
       TDisjunction (lhs', rhs')
     else
       err "Both operands to a logical expression must be boolean-typed expressions."
  | Negation e ->
     let e' = aug e in
     if is_bool e' then
       TNegation e'
     else
       err "The operand to the negation operator must be an expression of boolean type."
  | IfExpression (c, t, f) ->
     let c' = aug c
     and t' = aug t
     and f' = aug f in
     if is_bool c' then
       if (get_type t') = (get_type f') then
         TIfExpression (c', t', f')
       else
         err "Both branches of an if expression must be the same type."
     else
       err "The type of the condition in an if expression must be a boolean."
  | Path (e, elems) ->
     let e' = aug e in
     TPath (e', augment_path menv module_name (get_type e') elems)

and is_bool e =
  match get_type e with
  | Boolean -> true
  | _ -> false

and is_int_constant e =
  match e with
  | IntConstant _ -> true
  | _ -> false

and is_float_constant e =
  match e with
  | FloatConstant _ -> true
  | _ -> false

and augment_arglist (module_name: module_name) (menv: menv) (lexenv: lexenv) (asserted_ty: ty option) (args: abstract_arglist): typed_arglist =
  let aug = augment_expr module_name menv lexenv asserted_ty in
  match args with
  | Positional args' ->
     TPositionalArglist (List.map aug args')
  | Named pairs ->
     TNamedArglist (List.map (fun (n, v) -> (n, aug v)) pairs)

and augment_path (menv: menv) (module_name: module_name) (head_ty: ty) (elems: path_elem list): typed_path_elem list =
  match elems with
  | [elem] ->
     [augment_path_elem menv module_name head_ty elem]
  | elem::rest ->
     let elem' = augment_path_elem menv module_name head_ty elem in
     let rest' = augment_path menv module_name (path_elem_type elem') rest in
     elem' :: rest'
  | [] ->
     err "Path is empty"

and augment_path_elem (menv: menv) (module_name: module_name) (head_ty: ty) (elem: path_elem): typed_path_elem =
  match head_ty with
  | (NamedType (name, args, _)) ->
     (match elem with
      | SlotAccessor slot_name ->
         (* Check: e' is a public record type *)
         let (source_module, vis, typarams, slots) = get_record_definition menv name in
         if (vis = TypeVisPublic) || (module_name = source_module) then
           (* Check: the given slot name must exist in this record type. *)
           let (TypedSlot (_, slot_ty)) = get_slot_with_name slots slot_name in
           let bindings = match_typarams typarams args in
           let slot_ty' = replace_variables bindings slot_ty in
           TSlotAccessor (slot_name, slot_ty')
         else
           err "Trying to read a slot from a non-public record")
  | _ ->
     err "Not a record type"

and get_record_definition menv name =
  match get_decl menv name with
  | (Some (SRecordDefinition (mod_name, vis, _, typarams, _, slots))) ->
     (mod_name, vis, typarams, slots)
  | _ ->
     err "No record with this name"

and get_slot_with_name slots slot_name =
  match List.find_opt (fun (TypedSlot (n, _)) -> n = slot_name) slots with
  | Some s -> s
  | None -> err "No slot with this name"

and augment_call (module_name: module_name) (menv: menv) (asserted_ty: ty option) (name: qident) (args: typed_arglist): texpr =
  match get_callable menv module_name name with
  | Some callable ->
     augment_callable module_name menv name callable asserted_ty args
  | None ->
     err "No callable with this name"

and augment_callable (module_name: module_name) (menv: menv) (name: qident) (callable: callable) (asserted_ty: ty option) (args: typed_arglist) =
  match callable with
  | FunctionCallable (typarams, params, rt) ->
     augment_function_call name typarams params rt asserted_ty args
  | TypeAliasCallable (typarams, universe, ty) ->
     augment_typealias_callable name typarams universe asserted_ty ty args
  | RecordConstructor (typarams, universe, slots) ->
     augment_record_constructor name typarams universe slots asserted_ty args
  | UnionConstructor { type_name; type_params; universe; case } ->
     augment_union_constructor type_name type_params universe case asserted_ty args
  | MethodCallable { type_class_name; type_class_type_parameter; value_parameters; return_type; _ } ->
     augment_method_call menv module_name type_class_name type_class_type_parameter name value_parameters return_type asserted_ty args

and augment_function_call name typarams params rt asserted_ty args =
  (* For simplicity, and to reduce duplication of code, we convert the argument
     list to a positional list. *)
  let param_names = List.map (fun (ValueParameter (n, _)) -> n) params in
  let arguments = arglist_to_positional (args, param_names) in
  (* Check the list of params against the list of arguments *)
  let bindings = check_argument_list params arguments in
  (* Use the bindings to get the effective return type *)
  let rt' = replace_variables bindings rt in
  let (bindings', rt'') = handle_return_type_polymorphism typarams rt' asserted_ty in
  (* Check: the set of bindings equals the set of type parameters *)
  check_bindings typarams (merge_bindings bindings bindings');
  TFuncall (name, arguments, rt'')

and augment_typealias_callable name typarams universe asserted_ty definition_ty args =
  (* Check: the argument list is a positional list with a single argument *)
  let arg = (match args with
             | TPositionalArglist [a] ->
                a
             | _ ->
                err "The argument list to a type alias constructor call must be a single positional argument.")
  in
  (* Check a synthetic list of params against the list of arguments *)
  let params = [ValueParameter (make_ident "synthetic", definition_ty)] in
  let bindings = check_argument_list params [arg] in
  (* Use the bindings to get the effective return type *)
  let rt = NamedType (
               name,
               List.map (fun (TypeParameter (n, u)) -> TyVar (TypeVariable (n, u))) typarams,
               universe
             )
  in
  let rt' = replace_variables bindings rt in
  let (bindings', rt'') = handle_return_type_polymorphism typarams rt' asserted_ty in
  (* Check: the set of bindings equals the set of type parameters *)
  check_bindings typarams (merge_bindings bindings bindings');
  TCast (arg, rt'')

and augment_record_constructor (name: qident) (typarams: type_parameter list) (universe: universe) (slots: typed_slot list) (asserted_ty: ty option) (args: typed_arglist) =
  (* Check: the argument list must be named *)
  let args' = (match args with
               | TPositionalArglist l ->
                  if l = [] then
                    []
                  else
                    err "Arguments to a record constructor must be named"
               | TNamedArglist a ->
                  a) in
  (* Check: the set of slots matches the set of param names *)
  let slot_names: identifier list = List.map (fun (TypedSlot (name, _)) -> name) slots in
  let argument_names: identifier list = List.map (fun (n, _) -> n) args' in
  if not (ident_set_eq slot_names argument_names) then
    err "Slot names don't match argument names"
  else
    (* Convert args to positional *)
    let arguments = arglist_to_positional (args, slot_names) in
    (* Check the list of params against the list of arguments *)
    let params = List.map (fun (TypedSlot (n, t)) -> ValueParameter (n, t)) slots in
    let bindings = check_argument_list params arguments in
    (* Use the bindings to get the effective return type *)
    let rt = NamedType (name,
                        List.map (fun (TypeParameter (n, u)) -> TyVar (TypeVariable (n, u))) typarams,
                        universe)
    in
    let rt' = replace_variables bindings rt in
    let (bindings', rt'') = handle_return_type_polymorphism typarams rt' asserted_ty in
    (* Check: the set of bindings equals the set of type parameters *)
    check_bindings typarams (merge_bindings bindings bindings');
    (* Check the resulting type is in the correct universe *)
    if universe_compatible universe (type_universe rt'') then
      TRecordConstructor (rt'', List.map2 (fun a b -> (a, b)) slot_names arguments)
    else
      err "Universe mismatch"

and augment_union_constructor (type_name: qident) (typarams: type_parameter list) (universe: universe) (case: typed_case) (asserted_ty: ty option) (args: typed_arglist) =
  (* Check: the argument list must be named *)
  let args' = (match args with
               | TPositionalArglist l ->
                  if l = [] then
                    []
                  else
                    err "Arguments to a union constructor must be named"
               | TNamedArglist a ->
                  a) in
  (* Check: the set of slots matches the set of param names *)
  let (TypedCase (case_name, slots)) = case in
  let slot_names: identifier list = List.map (fun (TypedSlot (name, _)) -> name) slots in
  let argument_names: identifier list = List.map (fun (n, _) -> n) args' in
  if not (ident_set_eq slot_names argument_names) then
    err "Slot names don't match argument names"
  else
    (* Convert args to positional *)
    let arguments = arglist_to_positional (args, slot_names) in
    (* Check the list of params against the list of arguments *)
    let params = List.map (fun (TypedSlot (n, t)) -> ValueParameter (n, t)) slots in
    let bindings = check_argument_list params arguments in
    (* Use the bindings to get the effective return type *)
    let rt = NamedType (type_name,
                        List.map (fun (TypeParameter (n, u)) -> TyVar (TypeVariable (n, u))) typarams,
                        universe)
    in
    let rt' = replace_variables bindings rt in
    let (bindings', rt'') = handle_return_type_polymorphism typarams rt' asserted_ty in
    (* Check: the set of bindings equals the set of type parameters *)
    check_bindings typarams (merge_bindings bindings bindings');
    (* Check the resulting type is in the correct universe *)
    if universe_compatible universe (type_universe rt'') then
      TUnionConstructor (rt'', case_name, List.map2 (fun a b -> (a, b)) slot_names arguments)
    else
      err "Universe mismatch"

and augment_method_call menv source_module_name type_class_name typaram callable_name params rt asserted_ty args =
  (* At this point, we know the method's name and the typeclass it belongs
     to. We pull the parameter list from the typeclass, and compare that against
     the argument list. *)
  (* For simplicity, and to reduce duplication of code, we convert the argument
     list to a positional list. *)
  let param_names = List.map (fun (ValueParameter (n, _)) -> n) params in
  let arguments = arglist_to_positional (args, param_names) in
  (* Check the list of params against the list of arguments *)
  let bindings = check_argument_list params arguments in
  (* Use the bindings to get the effective return type *)
  let rt' = replace_variables bindings rt in
  let (bindings', rt'') = handle_return_type_polymorphism [typaram] rt' asserted_ty in
  (* Check: the set of bindings equals the set of type parameters *)
  check_bindings [typaram] (merge_bindings bindings bindings');
  let (TypeParameter (type_parameter_name, _)) = typaram in
  match get_binding bindings type_parameter_name with
  | (Some dispatch_ty) ->
     let instance = get_instance menv source_module_name dispatch_ty type_class_name in
     TMethodCall (callable_name, instance, arguments, rt'')
  | None ->
     err "Internal: couldn't extract dispatch type."

and get_instance (menv: menv) (source_module_name: module_name) (dispatch_ty: ty) (type_class_name: qident): semantic_instance =
  (* This comment describes the process of finding an instance of the typeclass
     given the argument list.

     Suppose we have a typeclass:

         interface Printable(T: Free) is
             method Print(t: T): Unit;
         end;

     And an instance:

         implementation Printable(Integer_32) is
             method Print(t: Integer_32) is
                 ...
             end;
         end;

     Then, if we have a call like `Print(30)`, we know which typeclass it's
     coming from. As part of the type matching process, we match the parameter
     list from the typeclass against the argument list in the method call, and
     get a set of bindings that maps `T` to `Integer_32`.

     We ask the binding map for the value of the binding with the name of the
     typeclass parameter. In this case, the typeclass parameter is `T`, so we
     get `Integer_32`. This is called the dispatch type. We then iterate over
     all visible instances of this typeclass, and find one where the instance
     argument matches the dispatch type.

     Note: the types have to _match_, not be equal, that is, the matching
     process produces bindings. This is important when finding generic
     instances.

         generic (U: Free)
         implementation Printable(List[U]) is
             method Print(t: List[U]) is
                 ...
             end;
         end;

     Here, a call like `print(listOfInts)` will yield a binding map that maps
     `T` to `List[Integer_32]`. When we search the list of visible instances,
     we'll find `List[U]`, and matching these types will produce another binding
     map that maps `U` to `Integer_32`.

     We use this second binding map to replace the variables in the dispatch
     type: in our case, `List[U]` becomes `List[Integer_32]`. *)
  let m = (match get_module menv source_module_name with
           | (Some m) -> m
           | None -> err "Internal: current module does not exist in the menv.") in
  let finder (STypeClassInstance (_, name, _, arg, _)) =
    if name = (original_name type_class_name) then
      try
        let _ = match_type arg dispatch_ty in
        (* TODO: the rest of the process described above. *)
        true
      with
        Type_match_error _ ->
      (* Does not match, just skip to the next instance, *)
        false
    else
      false
  in
  match List.find_opt finder (visible_instances m) with
  | (Some i) -> i
  | None -> err "No instance for this method call"


(* Given a list of type parameters, and a list of arguments, check that the
   lists have the same length and each argument satisfies each corresponding
   parameter. Return the resulting set of type bindings. *)
and check_argument_list (params: value_parameter list) (args: texpr list): type_bindings =
  (* Check that the number of parameters is the same as the number of
     arguments. *)
  check_arity params args;
  (* Check arguments against parameters *)
  check_argument_list' empty_bindings params args

and check_arity (params: value_parameter list) (args: texpr list): unit =
  let nparams = List.length params
  and nargs = List.length args in
  if nparams = nargs then
    ()
  else
    err ("The function expects "
         ^ (string_of_int nparams)
         ^ " arguments, but was called with "
         ^ (string_of_int nargs)
         ^ ".")

(* Precondition: both lists have the same length. *)
and check_argument_list' (bindings: type_bindings) (params: value_parameter list) (args: texpr list): type_bindings =
  match (params, args) with
  | ((first_param::rest_params), (first_arg::rest_args)) ->
     let bindings' = merge_bindings bindings (match_parameter first_param first_arg) in
     check_argument_list' bindings' rest_params rest_args
  | ([], []) ->
     bindings
  | _ ->
     err "Internal"

and match_parameter (param: value_parameter) (arg: texpr): type_bindings =
  let (ValueParameter (_, ty)) = param in
  match_type ty (get_type arg)

and handle_return_type_polymorphism (typarams: type_parameter list) (rt: ty) (asserted_ty: ty option): (type_bindings * ty) =
  if is_return_type_polymorphic typarams rt then
    match asserted_ty with
    | (Some asserted_ty') ->
       let bindings = match_type rt asserted_ty' in
       (bindings, replace_variables bindings rt)
    | None -> err "Callable is polymorphic in the return type but has no asserted type."
  else
    (empty_bindings, rt)

(* Given a set of type parameters, check if the return type of a function is polymorphic. *)
and is_return_type_polymorphic (typarams: type_parameter list) (rt: ty): bool =
  (* Take the set of type variables in the return type. Remove those that are type parameters. If there are any left, the function is polymorphic in its return type. *)
  let rt_type_vars = type_variables rt in
  let vars = TypeVarSet.elements rt_type_vars in
  let vars_without_parameters =
    List.filter (fun (TypeVariable (n, _)) -> List.exists (fun (TypeParameter (n', _)) -> n = n') typarams) vars in
  List.length vars_without_parameters > 0

(* Check that there are as many bindings as there are type parameters, and that
   every type parameter is satisfied. *)
and check_bindings (typarams: type_parameter list) (bindings: type_bindings): unit =
  if (List.length typarams) = (binding_count bindings) then
    let check (TypeParameter (n, u)): unit =
      (match get_binding bindings n with
       | Some ty ->
          if universe_compatible u (type_universe ty) then
            ()
          else
            err "Mismatched universes"
       | None ->
          err "No binding for this parameter")
    in
    let _ = List.map check typarams in
    ()
  else
    err "Not the same number of bindings and parameters"

(* Since the semantic extraction pass has already happened, we can simplify the
   call to `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (menv: menv) (rm: region_map) (typarams: type_parameter list) (ty: qtypespec): ty =
  parse_type menv [] rm typarams ty

let is_boolean = function
  | Boolean -> true
  | _ -> false

let is_compatible_with_size_type = function
  | TIntConstant _ ->
     true
  | e ->
     (get_type e) = size_type

type stmt_ctx = module_name * menv * region_map * type_parameter list * lexenv

let update_lexenv (mn, menv, rm, typarams, _) lexenv =
  (mn, menv, rm, typarams, lexenv)

let rec augment_stmt (ctx: stmt_ctx) (stmt: astmt): tstmt =
  let (module_name, menv, rm, typarams, lexenv) = ctx in
  match stmt with
  | ASkip ->
     TSkip
  | ALet (name, ty, value, body) ->
     let ty' = parse_typespec menv rm typarams ty in
     let value' = augment_expr module_name menv lexenv (Some ty') value in
     if equal_ty ty' (get_type value') then
       let lexenv' = push_var lexenv name ty' in
       let body' = augment_stmt (update_lexenv ctx lexenv') body in
       TLet (name, ty', value', body')
     else
       err ("let: type mismatch, expected:\n\n"
            ^ (show_ty ty')
            ^ "\n\nbut got\n\n"
            ^ (show_ty (get_type value')))
  | AAssign (name, value) ->
     let value' = augment_expr module_name menv lexenv None value in
     (match get_var lexenv name with
      | Some ty ->
         if ty = (get_type value') then
           TAssign (name, value')
         else
           err "assignment: type mismatch"
      | None ->
         err "No var with this name")
  | AIf (c, t, f) ->
     let c' = augment_expr module_name menv lexenv None c in
     if is_boolean (get_type c') then
       TIf (c', augment_stmt ctx t, augment_stmt ctx f)
     else
       err "The type of the condition in an if statement must be a boolean."
  | ACase (expr, whens) ->
     (* Type checking a case statement:

        1. Ensure the value is of a union type.
        2. Ensure the union type is public or it is defined in this module.
        3. Ensure the set of case names in the case statement equals the set of cases in the union definition.
        4. Iterate over the cases, and ensure the bindings are correct.
      *)
     let expr' = augment_expr module_name menv lexenv None expr in
     let cases = get_union_type_definition module_name menv (get_type expr') in
     let case_names = List.map (fun (TypedCase (n, _)) -> n) cases in
     let when_names = List.map (fun (AbstractWhen (n, _, _)) -> n) whens in
     if ident_set_eq case_names when_names then
       (* Group the cases and whens *)
       let whens' = group_cases_whens cases whens in
       let whens'' = List.map (fun (c, w) -> augment_when ctx w c) whens' in
       TCase (expr', whens'')
     else
       err "Non-exhaustive case statement."
  | AWhile (c, body) ->
     let c' = augment_expr module_name menv lexenv None c in
     if is_boolean (get_type c') then
       TWhile (c', augment_stmt ctx body)
     else
       err "The type of the condition in a while loop must be a boolean"
  | AFor { name; initial; final; body } ->
     let i' = augment_expr module_name menv lexenv None initial
     and f' = augment_expr module_name menv lexenv None final in
     if is_compatible_with_size_type i' then
       if is_compatible_with_size_type f' then
         let lexenv' = push_var lexenv name size_type in
         let b' = augment_stmt (update_lexenv ctx lexenv') body in
         TFor (name, i', f', b')
       else
         err "The type of the final value in a for loop must be an integer type."
     else
       err "The type of the initial value in a for loop must be an integer type."
  | ABlock (f, r) ->
     TBlock (augment_stmt ctx f,
             augment_stmt ctx r)
  | ADiscarding e ->
     let e' = augment_expr module_name menv lexenv None e in
     TDiscarding e'
  | AReturn e ->
     let e' = augment_expr module_name menv lexenv None e in
     (* TODO: check type of e' matches return context *)
     TReturn e'

and get_union_type_definition (importing_module: module_name) (menv: menv) (ty: ty) =
  let name = (match ty with
              | NamedType (n, _, _) ->
                 n
              | _ ->
                 err "Not a named type") in
  match get_decl menv name with
  | (Some (SUnionDefinition (module_name, vis, _, _, _, cases))) ->
     if (vis = TypeVisPublic) || (importing_module = module_name) then
       cases
     else
       err "Union must be public or from the same module to be used in a case statement."
  | _ ->
     err "Not a union type"

and group_cases_whens (cases: typed_case list) (whens: abstract_when list): (typed_case * abstract_when) list =
  List.map (fun (TypedCase (n, s)) -> (TypedCase (n, s), List.find (fun (AbstractWhen (n', _, _)) -> n = n') whens)) cases

and group_bindings_slots (bindings: (identifier * qtypespec) list) (slots: typed_slot list): (identifier * qtypespec * ty) list =
  List.map (fun (n, t) -> (n, t, let (TypedSlot (_, ty')) = List.find (fun (TypedSlot (n', _)) -> n = n') slots in ty')) bindings

and augment_when (ctx: stmt_ctx) (w: abstract_when) (c: typed_case): typed_when =
  let (_, menv, rm, typarams, lexenv) = ctx in
  let (AbstractWhen (name, bindings, body)) = w
  and (TypedCase (_, slots)) = c in
  (* Check the set of binding names is the same as the set of slots *)
  let binding_names = List.map (fun (n, _) -> n) bindings
  and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
  if ident_set_eq binding_names slot_names then
    (* Check the type of each binding matches the type of the slot *)
    let bindings' = group_bindings_slots bindings slots in
    let bindings'' = List.map (fun (n, ty, actual_ty) ->
                         let ty' = parse_typespec menv rm typarams ty in
                         if ty' <> actual_ty then
                           err "Slot type mismatch"
                         else
                           (n, ty'))
                       bindings' in
    let lexenv' = push_vars lexenv bindings'' in
    let body' = augment_stmt (update_lexenv ctx lexenv') body in
    TypedWhen (name, List.map (fun (n, t) -> ValueParameter (n, t)) bindings'', body')
  else
    err "The set of slots in the case statement doesn't match the set of slots in the union definition."

let rec augment_decl (module_name: module_name) (menv: menv) (decl: combined_definition): typed_decl =
  match decl with
  | CConstant (vis, name, ts, expr, doc) ->
     let ty = parse_typespec menv empty_region_map [] ts in
     let expr' = augment_expr module_name menv empty_lexenv (Some ty) expr in
     if ty = get_type expr' then
       TConstant (vis, name, ty, expr', doc)
     else
       err "The declared type does not match the actual type of the expression."
  | CTypeAlias (vis, name, typarams, universe, ts, doc) ->
     let rm = region_map_from_typarams typarams in
     let ty = parse_typespec menv rm typarams ts in
     TTypeAlias (vis, name, typarams, universe, ty, doc)
  | CRecord (vis, name, typarams, universe, slots, doc) ->
     let rm = region_map_from_typarams typarams in
     let slots' = augment_slots menv rm typarams slots in
     TRecord (vis, name, typarams, universe, slots', doc)
  | CUnion (vis, name, typarams, universe, cases, doc) ->
     let rm = region_map_from_typarams typarams in
     let cases' = augment_cases menv rm typarams cases in
     TUnion (vis, name, typarams, universe, cases', doc)
  | CFunction (vis, name, typarams, params, rt, body, doc, pragmas) ->
     let rm = region_map_from_typarams typarams in
     let params' = augment_params menv rm typarams params
     and rt' = parse_typespec menv rm typarams rt in
     (match pragmas with
      | [ForeignImportPragma s] ->
         if typarams = [] then
           TForeignFunction (vis, name, params', rt', s, doc)
         else
           err "Foreign functions can't have type parameters."
      | [] ->
         let ctx = module_name, menv, rm, typarams, (lexenv_from_params params') in
         let body' = augment_stmt ctx body in
         TFunction (vis, name, typarams, params', rt', body', doc)
      | _ ->
         err "Invalid pragmas")
  | CTypeclass (vis, name, typaram, methods, doc) ->
     let rm = region_map_from_typarams [typaram] in
     TTypeClass (vis, name, typaram, List.map (augment_method_decl menv rm typaram) methods, doc)
  | CInstance (vis, name, typarams, arg, methods, doc) ->
     (* TODO: the universe of the type parameter matches the universe of the type argument *)
     (* TODO: Check the methods in the instance match the methods in the class *)
     let rm = region_map_from_typarams typarams in
     let arg' = parse_typespec menv rm typarams arg in
     TInstance (vis, name, typarams, arg', List.map (augment_method_def module_name menv rm typarams) methods, doc)

and augment_slots menv rm typarams slots =
  List.map (fun (QualifiedSlot (n, ts)) -> TypedSlot (n, parse_typespec menv rm typarams ts)) slots

and augment_cases menv rm typarams cases =
  List.map (fun (QualifiedCase (n, ss)) -> TypedCase (n, augment_slots menv rm typarams ss)) cases

and augment_params menv rm typarams params =
  List.map (fun (QualifiedParameter (n, ts)) -> ValueParameter (n, parse_typespec menv rm typarams ts)) params

and lexenv_from_params (params: value_parameter list): lexenv =
  match params with
  | (ValueParameter (n, t))::rest ->
     push_var (lexenv_from_params rest) n t
  | [] ->
     empty_lexenv

and augment_method_decl menv rm typaram (CMethodDecl (name, params, rt, _)) =
  let params' = augment_params menv rm [typaram] params
  and rt' = parse_typespec menv rm [typaram] rt in
  TypedMethodDecl (name, params', rt')

and augment_method_def module_name menv rm typarams (CMethodDef (name, params, rt, _, body)) =
  let params' = augment_params menv rm typarams params
  and rt' = parse_typespec menv rm typarams rt in
  let ctx = module_name, menv, rm, typarams, (lexenv_from_params params') in
  let body' = augment_stmt ctx body in
  TypedMethodDef (name, params', rt', body')

let augment_module menv (CombinedModule { name; decls; _ }) =
  TypedModule (name, List.map (augment_decl name menv) decls)
