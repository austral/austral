open Identifier
open Type
open TypeSystem
open TypeBindings
open TypeMatch
open TypeVarSet
open TypeParser
open LexEnv
open ModuleSystem
open Ast
open Tast
open Semantic
open Error

let rec augment_expr (module_name: module_name) (menv: menv) (lexenv: lexenv) (asserted_ty: ty option) (expr: aexpr): texpr =
  let aug = augment_expr module_name menv lexenv asserted_ty in
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
     augment_call module_name menv asserted_ty name (augment_arglist module_name menv lexenv asserted_ty args)
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

and augment_call (module_name: module_name) (menv: menv) (asserted_ty: ty option) (name: qident) (args: typed_arglist): texpr =
  match get_callable menv module_name name with
  | Some callable ->
     augment_callable name callable asserted_ty args
  | None ->
     err "No callable with this name"

and augment_callable (name: qident) (callable: callable) (asserted_ty: ty option) (args: typed_arglist) =
  match callable with
  | FunctionCallable (typarams, params, rt) ->
     augment_function_call name typarams params rt asserted_ty args
  | RecordConstructor (typarams, universe, slots) ->
     augment_record_constructor name typarams universe slots asserted_ty args
  | UnionConstructor { type_name; type_params; universe; case } ->
     augment_union_constructor type_name type_params universe case asserted_ty args
  | MethodCallable { type_class_name; type_class_type_parameter; method_name; value_parameters; return_type } ->
     augment_method_call type_class_name type_class_type_parameter method_name value_parameters return_type asserted_ty args

and augment_function_call name typarams params rt asserted_ty args =
  (* For simplicity, and to reduce duplication of code, we convert the argument
     list to a positional list. *)
  let param_names = List.map (fun (ValueParameter (n, _)) -> n) params in
  let arguments = arglist_to_positional (args, param_names) in
  (* Check the list of params against the list of arguments *)
  let bindings = check_argument_list params arguments in
  (* Use the bindings to get the effective return type *)
  let rt' = replace_variables bindings rt in
  let rt'' = handle_return_type_polymorphism typarams rt' asserted_ty in
  (* Check: the set of bindings equals the set of type parameters *)
  check_bindings typarams bindings;
  TFuncall (name, arguments, rt'')

and augment_record_constructor (name: qident) (typarams: type_parameter list) (universe: universe) (slots: typed_slot list) (asserted_ty: ty option) (args: typed_arglist) =
  (* Check: the argument list must be named *)
  let args' = (match args with
               | TPositionalArglist _ ->
                  err "Arguments to a record constructor must be named"
               | TNamedArglist a ->
                  a) in
  (* Check: the set of slots matches the set of param names *)
  let slot_names: identifier list = List.map (fun (TypedSlot (name, _)) -> name) slots in
  let argument_names: identifier list = List.map (fun (n, _) -> n) args' in
  let sorter a b = compare (ident_string a) (ident_string b) in
  if (List.sort sorter slot_names) <> (List.sort sorter argument_names) then
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
    let rt'' = handle_return_type_polymorphism typarams rt' asserted_ty in
    (* Check: the set of bindings equals the set of type parameters *)
    check_bindings typarams bindings;
    (* Check the resulting type is in the correct universe *)
    if universe_compatible universe (type_universe rt'') then
      TRecordConstructor (rt'', List.map2 (fun a b -> (a, b)) slot_names arguments)
    else
      err "Universe mismatch"

and augment_union_constructor (type_name: qident) (typarams: type_parameter list) (universe: universe) (case: typed_case) (asserted_ty: ty option) (args: typed_arglist) =
  (* Check: the argument list must be named *)
  let args' = (match args with
               | TPositionalArglist _ ->
                  err "Arguments to a union constructor must be named"
               | TNamedArglist a ->
                  a) in
  (* Check: the set of slots matches the set of param names *)
  let (TypedCase (case_name, slots)) = case in
  let slot_names: identifier list = List.map (fun (TypedSlot (name, _)) -> name) slots in
  let argument_names: identifier list = List.map (fun (n, _) -> n) args' in
  let sorter a b = compare (ident_string a) (ident_string b) in
  if (List.sort sorter slot_names) <> (List.sort sorter argument_names) then
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
    let rt'' = handle_return_type_polymorphism typarams rt' asserted_ty in
    (* Check: the set of bindings equals the set of type parameters *)
    check_bindings typarams bindings;
    (* Check the resulting type is in the correct universe *)
    if universe_compatible universe (type_universe rt'') then
      TUnionConstructor (rt'', case_name, List.map2 (fun a b -> (a, b)) slot_names arguments)
    else
      err "Universe mismatch"

and augment_method_call _ _ _ _ _ _ =
  err "TODO"

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

and handle_return_type_polymorphism (typarams: type_parameter list) (rt: ty) (asserted_ty: ty option): ty =
  if is_return_type_polymorphic typarams rt then
    match asserted_ty with
    | (Some rt') -> rt'
    | None -> err "Callable is polymorphic in the return type but has no asserted type."
  else
    rt

(* Given a set of type parameters, check if the return type of a function is polymorphic. *)
and is_return_type_polymorphic (typarams: type_parameter list) (rt: ty): bool =
  (* Take the set of type variables in the return type. Remove those that are type parameters. If there are any left, the function is polymorphic in its return type. *)
  let rt_type_vars = type_variables rt in
  let vars = TypeVarSet.elements rt_type_vars in
  let vars_without_parameters =
    List.filter (fun (TypeVariable (n, _)) -> List.exists (fun (TypeParameter (n', _)) -> n = n') typarams) vars in
  List.length vars_without_parameters > 0

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
