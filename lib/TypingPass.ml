open Identifier
open Common
open Type
open TypeSystem
open TypeBindings
open TypeMatch
open TypeVarSet
open TypeParser
open Region
open Id
open LexEnv
open Env
open Ast
open Tast
open Linked
open Linearity
open Util
open Reporter
open Error

let get_instance (env: env) (source_module_name: module_name) (dispatch_ty: ty) (typeclass: decl_id): decl =
  let _ = source_module_name in
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
     coming from, because the method name `Print` points to the typeclass. As
     part of the type matching process, we match the parameter list from the
     typeclass against the argument list in the method call, and get a set of
     bindings that maps `T` to `Integer_32`.

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
    let pred = function
      | Instance { typeclass_id; argument; _ } ->
         if equal_decl_id typeclass_id typeclass then
           try
             let _ = match_type argument dispatch_ty in
             (* TODO: the rest of the process described above. *)
             true
           with
             Austral_error _ ->
             (* Does not match, just skip to the next instance, *)
             false
         else
           false
      | _ -> false
    in
    match (List.filter pred (visible_instances env)) with
    | [a] ->
       a
    | _::_ ->
       err "Overlapping typeclasses"
    | [] ->
       err "Typeclass resolution failed"

(* Since the extraction pass has already happened, we can simplify the call to
   `parse_type` by passing an empty list of local type signatures. *)
let parse_typespec (env: env) (rm: region_map) (typarams: type_parameter list) (ty: qtypespec): ty =
  parse_type env [] rm typarams ty

let rec augment_expr (module_name: module_name) (env: env) (rm: region_map) (typarams: type_parameter list) (lexenv: lexenv) (asserted_ty: ty option) (expr: aexpr): texpr =
  with_frame ("Augment expression: " ^ (expr_kind expr))
    (fun _ ->
      let aug = augment_expr module_name env rm typarams lexenv None in
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
         (match get_variable env lexenv name with
          | Some (ty, src) ->
             (match src with
              | VarConstant ->
                 TConstVar (name, ty)
              | VarParam ->
                 TParamVar ((original_name name), ty)
              | VarLocal ->
                 TLocalVar ((original_name name), ty))
          | None ->
             err ("I can't find the variable named " ^ (ident_string (original_name name))))
      | FunctionCall (name, args) ->
         augment_call module_name env asserted_ty name (augment_arglist module_name env rm typarams lexenv None args)
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
         let _ = match_type_with_value (get_type lhs') rhs' in
         TComparison (op, lhs', rhs')
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
         (* Path rules:

            1. Path that begins in a reference ends in a reference.
            2. All paths end in a type in the free universe.
          *)
         let e' = aug e in
         (* Is the initial expression of a path a read reference or write reference or no reference? *)
         let (is_read, region): (bool * ty option) =
           (match (get_type e') with
            | ReadRef (_, r) ->
               (true, Some r)
            | WriteRef (_, r) ->
               (false, Some r)
            | _ ->
               (false, None))
         in
         let elems' = augment_path env module_name rm typarams lexenv (get_type e') elems in
         let path_ty: ty = get_path_ty_from_elems elems' in
         let path_ty: ty =
           (* If the path starts in a reference it should end in one. *)
           (match region with
            | Some reg ->
               if is_read then
                 ReadRef (path_ty, reg)
               else
                 WriteRef (path_ty, reg)
            | None ->
               path_ty)
         in
         let universe = type_universe path_ty in
         if universe = FreeUniverse then
           TPath {
               head = e';
               elems = elems';
               ty = path_ty
             }
         else
           err ("Paths must end in the free universe: " ^ (show_ty path_ty))
      | Embed (ty, expr, args) ->
         TEmbed (
             parse_typespec env rm typarams ty,
             expr,
             List.map aug args
           )
      | Deref expr ->
         (* The type of the expression being dereferenced must be either a read-only
            reference or a write reference. *)
         let expr' = aug expr in
         let ty = get_type expr' in
         (match ty with
          | ReadRef _ ->
             TDeref expr'
          | WriteRef _ ->
             TDeref expr'
          | _ ->
             err "The dereference operator must be applied to an expression of a reference type.")
      | Typecast (expr, ty) ->
         (* The typecast operator has four uses:

            1. Clarifying the type of integer and floating point constants.

            2. Converting between different integer and floating point types
            (otherwise, you get a combinatorial explosion of typeclasses).

            3. Converting write references to read references.

            4. Clarifying the type of return type polymorphic functions.

          *)

         let rec is_int_or_float_type = function
           | Integer _ -> true
           | SingleFloat -> true
           | DoubleFloat -> true
           | _ -> false

         and augment_numeric_conversion (expr: texpr) (ty: ty): texpr =
           let source_type_name = type_conversion_name (get_type expr)
           and target_type_name = type_conversion_name ty in
           let conversion_function_call = "Austral__Core::convert_" ^ source_type_name ^ "_to_" ^ target_type_name ^ "($1)" in
           TEmbed (ty, conversion_function_call, [expr])

         and type_conversion_name (ty: ty): string =
           (match ty with
            | Integer (signedness, width) ->
               let s = (match signedness with
                        | Unsigned -> "nat"
                        | Signed -> "int")
               and w = (match width with
                        | Width8 -> "8"
                        | Width16 -> "16"
                        | Width32 -> "32"
                        | Width64 -> "64")
               in
               s ^ "int" ^ w
            | SingleFloat ->
               "float"
            | DoubleFloat ->
               "double"
            | _ ->
               err "Invalid type for numeric conversion.")
         in

         let target_type = parse_typespec env rm typarams ty in
         (* By passing target_type as the asserted type we're doing point 4. *)
         let expr' = augment_expr module_name env rm typarams lexenv (Some target_type) expr in
         (* For 1 and 2: if the expression has an int or float type, and the type is
            an integer or float constant, do the conversion: *)
         if (is_int_or_float_type (get_type expr')) && (is_int_or_float_type target_type) then
           augment_numeric_conversion expr' target_type
         else
           (* Otherwise, if the expression has a mutable reference type and the
              target type is a read reference, and they point to the same underlying
              type and underlying region, just convert it to a read ref. This does
              point 3. *)
           (match (get_type expr') with
            | WriteRef (underlying_ty, region) ->
               (match target_type with
                | ReadRef (underlying_ty', region') ->
                   if ((equal_ty underlying_ty underlying_ty') && (equal_ty region region')) then
                     TCast (expr', target_type)
                   else
                     err "Cannot convert because the references have different underlying types or regions."
                | _ ->
                   err "Bad conversion.")
            | _ ->
               err "Bad conversion")
      | SizeOf ty ->
         TSizeOf (parse_typespec env rm typarams ty))


and get_path_ty_from_elems (elems: typed_path_elem list): ty =
  assert ((List.length elems) > 0);
  let last = List.nth elems ((List.length elems) - 1) in
  path_elem_type last

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

and augment_arglist (module_name: module_name) (env: env) (rm: region_map) (typarams: type_parameter list) (lexenv: lexenv) (asserted_ty: ty option) (args: abstract_arglist): typed_arglist =
  let aug = augment_expr module_name env rm typarams lexenv asserted_ty in
  match args with
  | Positional args' ->
     TPositionalArglist (List.map aug args')
  | Named pairs ->
     TNamedArglist (List.map (fun (n, v) -> (n, aug v)) pairs)

and augment_path (env: env) (module_name: module_name) (rm: region_map) (typarams: type_parameter list) (lexenv: lexenv) (head_ty: ty) (elems: path_elem list): typed_path_elem list =
  match elems with
  | [elem] ->
     [augment_path_elem env module_name rm typarams lexenv head_ty elem]
  | elem::rest ->
     let elem' = augment_path_elem env module_name rm typarams lexenv head_ty elem in
     let rest' = augment_path env module_name rm typarams lexenv (path_elem_type elem') rest in
     elem' :: rest'
  | [] ->
     err "Path is empty"

and augment_path_elem (env: env) (module_name: module_name) (rm: region_map) (typarams: type_parameter list) (lexenv: lexenv) (head_ty: ty) (elem: path_elem): typed_path_elem =
  match elem with
  | SlotAccessor slot_name ->
     (match head_ty with
      | NamedType (name, args, _) ->
         augment_slot_accessor_elem env module_name slot_name name args
      | _ ->
         err "Not a record type")
  | PointerSlotAccessor slot_name ->
     (match head_ty with
      | Pointer pointed_to ->
         (* TODO: Addresses should not be indexable. *)
         augment_pointer_slot_accessor_elem env module_name slot_name pointed_to
      | ReadRef (ty, _) ->
         (match ty with
          | NamedType (name, args, _) ->
             augment_reference_slot_accessor_elem env module_name slot_name name args
          | _ ->
             err "Not a record type")
      | WriteRef (ty, _) ->
         (match ty with
          | NamedType (name, args, _) ->
             augment_reference_slot_accessor_elem env module_name slot_name name args
          | _ ->
             err "Not a record type")
      | _ ->
         err "Not a record type")
  | ArrayIndex ie ->
     let ie' = augment_expr module_name env rm typarams lexenv None ie in
     (match head_ty with
      | Array (elem_ty, _) ->
         TArrayIndex (ie', elem_ty)
      | _ ->
         err "Array index operator doesn't work for this type.")

and augment_slot_accessor_elem (env: env) (module_name: module_name) (slot_name: identifier) (type_name: qident) (type_args: ty list) =
  (* Check: e' is a public record type *)
  let (source_module, vis, typarams, slots) = get_record_definition env type_name in
  if (vis = TypeVisPublic) || (module_name = source_module) then
    (* Check: the given slot name must exist in this record type. *)
    let (TypedSlot (_, slot_ty)) = get_slot_with_name slots slot_name in
    let bindings = match_typarams typarams type_args in
    let slot_ty' = replace_variables bindings slot_ty in
    TSlotAccessor (slot_name, slot_ty')
  else
    err "Trying to read a slot from a non-public record"

and augment_pointer_slot_accessor_elem (env: env) (module_name: module_name) (slot_name: identifier) (pointed_to: ty): typed_path_elem =
  match pointed_to with
  | NamedType (type_name, type_args, _) ->
     (* Check arg is a public record *)
     let (source_module, vis, typarams, slots) = get_record_definition env type_name in
     if (vis = TypeVisPublic) || (module_name = source_module) then
       (* Check: the given slot name must exist in this record type. *)
       let (TypedSlot (_, slot_ty)) = get_slot_with_name slots slot_name in
       let bindings = match_typarams typarams type_args in
       let slot_ty' = replace_variables bindings slot_ty in
       TPointerSlotAccessor (slot_name, slot_ty')
     else
       err "Trying to read a slot from a pointer to a non-public record"
  | _ ->
     err "Pointer does not point to a record type."

and augment_reference_slot_accessor_elem (env: env) (module_name: module_name) (slot_name: identifier) (type_name: qident) (type_args: ty list) =
  (* Check: e' is a public record type *)
  let (source_module, vis, typarams, slots) = get_record_definition env type_name in
  if (vis = TypeVisPublic) || (module_name = source_module) then
    (* Check: the given slot name must exist in this record type. *)
    let (TypedSlot (_, slot_ty)) = get_slot_with_name slots slot_name in
    let bindings = match_typarams typarams type_args in
    let slot_ty' = replace_variables bindings slot_ty in
    TPointerSlotAccessor (slot_name, slot_ty')
  else
    err "Trying to read a slot from a reference to a non-public record"

and get_record_definition (env: env) (name: qident) =
  match get_decl_by_name env (qident_to_sident name) with
  | (Some (Record { mod_id; vis; typarams; slots; _ })) ->
     let mod_name: module_name = module_name_from_id env mod_id in
     (mod_name, vis, typarams, slots)
  | _ ->
     err ("No record with this name: " ^ (ident_string (original_name name)))

and get_slot_with_name slots slot_name =
  match List.find_opt (fun (TypedSlot (n, _)) -> n = slot_name) slots with
  | Some s -> s
  | None -> err "No slot with this name"

and augment_call (module_name: module_name) (env: env) (asserted_ty: ty option) (name: qident) (args: typed_arglist): texpr =
  match get_callable env module_name (qident_to_sident name) with
  | Some callable ->
     augment_callable module_name env name callable asserted_ty args
  | None ->
     err ("No callable with this name: " ^ (qident_debug_name name))

and augment_callable (module_name: module_name) (env: env) (name: qident) (callable: callable) (asserted_ty: ty option) (args: typed_arglist) =
  with_frame "Augment callable"
    (fun _ ->
      match callable with
      | FunctionCallable (id, typarams, params, rt) ->
         augment_function_call id name typarams params rt asserted_ty args
      | TypeAliasCallable (_, typarams, universe, ty) ->
         augment_typealias_callable name typarams universe asserted_ty ty args
      | RecordConstructor (_, typarams, universe, slots) ->
         augment_record_constructor name typarams universe slots asserted_ty args
      | UnionConstructor { union_id; type_params; universe; case } ->
         let type_name: qident = (match get_decl_by_id env union_id with
                                  | Some (Union { name; mod_id; _ }) ->
                                     (* TODO: constructing a fake qident *)
                                     make_qident (module_name_from_id env mod_id, name, name)
                                  | _ ->
                                     err "Internal")
         in
         augment_union_constructor type_name type_params universe case asserted_ty args
      | MethodCallable { typeclass_id; value_parameters; return_type; _ } ->
         let param = (match get_decl_by_id env typeclass_id with
                      | Some (TypeClass { param; _ }) ->
                         param
                      | _ ->
                         err "Internal")
         in
         augment_method_call env module_name typeclass_id param name value_parameters return_type asserted_ty args)

and augment_function_call (id: decl_id) name typarams params rt asserted_ty args =
  with_frame "Augment function call"
    (fun _ ->
      pqi ("Name", name);
      (* For simplicity, and to reduce duplication of code, we convert the argument
         list to a positional list. *)
      let param_names = List.map (fun (ValueParameter (n, _)) -> n) params in
      let arguments = arglist_to_positional (args, param_names) in
      (* Check the list of params against the list of arguments *)
      let bindings = check_argument_list params arguments in
      (* Use the bindings to get the effective return type *)
      let rt' = replace_variables bindings rt in
      let (bindings', rt'') = handle_return_type_polymorphism (local_name name) params rt' asserted_ty bindings in
      (* Check: the set of bindings equals the set of type parameters *)
      let bindings'' = merge_bindings bindings bindings' in
      check_bindings typarams bindings'';
      let arguments' = cast_arguments bindings'' params arguments in
      let substs = make_substs bindings'' typarams in
      ps ("Bindings", show_bindings bindings'');
      pt ("Return type", rt'');
      TFuncall (id, name, arguments', rt'', substs))

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
               List.map (fun (TypeParameter (n, u, from)) -> TyVar (TypeVariable (n, u, from))) typarams,
               universe
             )
  in
  let rt' = replace_variables bindings rt in
  let (bindings', rt'') = handle_return_type_polymorphism (local_name name) params rt' asserted_ty bindings in
  (* Check: the set of bindings equals the set of type parameters *)
  check_bindings typarams (merge_bindings bindings bindings');
  TTypeAliasConstructor (rt'', arg)

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
                        List.map (fun (TypeParameter (n, u, from)) -> TyVar (TypeVariable (n, u, from))) typarams,
                        universe)
    in
    let rt' = replace_variables bindings rt in
    let (bindings', rt'') = handle_return_type_polymorphism (local_name name) params rt' asserted_ty bindings in
    (* Check: the set of bindings equals the set of type parameters *)
    check_bindings typarams (merge_bindings bindings bindings');
    (* Check the resulting type is in the correct universe *)
    if universe_compatible universe (type_universe rt'') then
      TRecordConstructor (rt'', List.map2 (fun a b -> (a, b)) slot_names arguments)
    else
      err "Universe mismatch"

and augment_union_constructor (type_name: qident) (typarams: type_parameter list) (universe: universe) (case: typed_case) (asserted_ty: ty option) (args: typed_arglist) =
  with_frame "Augment union constructor"
    (fun _ ->
      pqi ("Type name", type_name);
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
      pi ("Case name", case_name);
      let slot_names: identifier list = List.map (fun (TypedSlot (name, _)) -> name) slots in
      let argument_names: identifier list = List.map (fun (n, _) -> n) args' in
      if not (ident_set_eq slot_names argument_names) then
        err "Slot names don't match argument names"
      else
        (* Convert args to positional *)
        let arguments = arglist_to_positional (args, slot_names) in
        (* Check the list of params against the list of arguments *)
        let params = List.map (fun (TypedSlot (n, t)) -> ValueParameter (n, t)) slots in
        ps ("Params", String.concat "\n" (List.map (fun (ValueParameter (n, t)) -> (ident_string n) ^ ": " ^ (type_string t)) params));
        ps ("Arguments", String.concat "\n" (List.map show_texpr arguments));
        let bindings = check_argument_list params arguments in
        (* Use the bindings to get the effective return type *)
        let rt = NamedType (type_name,
                            List.map (fun (TypeParameter (n, u, from)) -> TyVar (TypeVariable (n, u, from))) typarams,
                            universe)
        in
        pt ("Type", rt);
        ps ("Bindings", show_bindings bindings);
        let rt' = replace_variables bindings rt in
        pt ("Type'", rt');
        let (bindings', rt'') = handle_return_type_polymorphism case_name params rt' asserted_ty bindings in
        (* Check: the set of bindings equals the set of type parameters *)
        check_bindings typarams (merge_bindings bindings bindings');
        (* Check the resulting type is in the correct universe *)
        if universe_compatible universe (type_universe rt'') then
          let _ = pt ("Type''", rt'')
          in
          TUnionConstructor (rt'', case_name, List.map2 (fun a b -> (a, b)) slot_names arguments)
        else
          err "Universe mismatch")

and augment_method_call (env: env) (source_module_name: module_name) (typeclass_id: decl_id) (typaram: type_parameter) (callable_name: qident) (params: value_parameter list) (rt: ty) (asserted_ty: ty option) (args: typed_arglist): texpr =
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
  let (bindings', rt'') = handle_return_type_polymorphism (local_name callable_name) params rt' asserted_ty bindings in
  let bindings'' = merge_bindings bindings bindings' in
  (* Check: the set of bindings equals the set of type parameters *)
  check_bindings [typaram] bindings'';
  let (TypeParameter (type_parameter_name, _, from)) = typaram in
  match get_binding bindings'' type_parameter_name from with
  | (Some dispatch_ty) ->
     let instance: decl = get_instance env source_module_name dispatch_ty typeclass_id in
     let params' = List.map (fun (ValueParameter (n, t)) -> ValueParameter (n, replace_variables bindings'' t)) params in
     let arguments' = cast_arguments bindings'' params' arguments in
     let typarams = (match instance with
                     | Instance { typarams; _ } -> typarams
                     | _ -> err "Internal")
     in
     let instance_id: decl_id = decl_id instance in
     let meth_id: ins_meth_id =
       (match get_instance_method_from_instance_id_and_method_name env instance_id (original_name callable_name) with
        | Some (InsMethRec { id; _ }) -> id
        | None -> err "Internal")
     in
     let substs = make_substs bindings'' typarams in
     TMethodCall (meth_id, callable_name, typarams, arguments', rt'', substs)
  | None ->
     err "Internal: couldn't extract dispatch type."

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
  match_type_with_value ty arg

and cast_arguments (bindings: type_bindings) (params: value_parameter list) (arguments: texpr list): texpr list =
  let f (ValueParameter (_, expected)) value =
    let expected' = replace_variables bindings expected in
    match expected' with
    | Integer _ ->
       (* Cast integer types *)
       TCast (value, expected')
    | _ ->
       value
  in
  List.map2 f params arguments

and make_substs (bindings: type_bindings) (typarams: type_parameter list): (identifier * ty) list =
  let f (TypeParameter (n, u, from)) =
    if u = RegionUniverse then
      None
    else
      match get_binding bindings n from with
      | Some ty ->
         Some (n, ty)
      | None ->
         None
  in
  List.filter_map f typarams

and handle_return_type_polymorphism (name: identifier) (params: value_parameter list) (rt: ty) (asserted_ty: ty option) (bindings: type_bindings): (type_bindings * ty) =
  if is_return_type_polymorphic params rt then
    match asserted_ty with
    | (Some asserted_ty') ->
       let bindings' = match_type rt asserted_ty' in
       let bindings'' = merge_bindings bindings bindings' in
       (bindings'', replace_variables bindings'' rt)
    | None ->
       err ("Callable '"
            ^ (ident_string name)
            ^ "' is polymorphic in the return type but has no asserted type.")
  else
    (empty_bindings, replace_variables bindings rt)

(* Given a function's parameter list and return type, check if the return type
   of a function is polymorphic. *)
and is_return_type_polymorphic (params: value_parameter list) (rt: ty): bool =
  (* Find the set of type variables in the return type. *)
  let rt_type_vars: TypeVarSet.t = type_variables rt in
  (* Find the set of type variables in each of the parameters. *)
  let param_type_vars: TypeVarSet.t list = List.map (fun (ValueParameter (_, ty)) -> type_variables ty) params in
  (* Union the type variables of the parameters into one set of all type
     variables in the parameter list. *)
  let param_type_vars: TypeVarSet.t = List.fold_left TypeVarSet.union TypeVarSet.empty param_type_vars in
  (* Find the set of type variables that are in the return type set but not in
     the parameter set. *)
  let vars_not_params: TypeVarSet.t =
    let param_ty_vars: type_var list = TypeVarSet.elements param_type_vars
    in
    let var_name_in_typarams (n: identifier): bool =
      List.exists (fun (TypeVariable (n', _, _)) -> equal_identifier n n') param_ty_vars
    in
    TypeVarSet.of_list (List.filter (fun (TypeVariable (n, _, _)) -> not (var_name_in_typarams n)) (TypeVarSet.elements rt_type_vars))
  in
  (TypeVarSet.cardinal vars_not_params) > 0

(* Check that there are as many bindings as there are type parameters, and that
   every type parameter is satisfied. *)
and check_bindings (typarams: type_parameter list) (bindings: type_bindings): unit =
  if (List.length typarams) = (binding_count bindings) then
    let check (TypeParameter (n, u, from)): unit =
      (match get_binding bindings n from with
       | Some ty ->
          if universe_compatible u (type_universe ty) then
            ()
          else
            err ("Mismatched universes: expected "
                 ^ (show_universe u)
                 ^ " and got "
                 ^ (show_universe (type_universe ty)))
       | None ->
          err ("No binding for this parameter: " ^ (ident_string n)))
    in
    let _ = List.map check typarams in
    ()
  else
    (* I think this should not be an error *)
    (*err ("Not the same number of bindings and parameters. Bindings: "
         ^ (show_bindings bindings)
         ^ ". Parameters: "
         ^ (String.concat ", " (List.map (fun (TypeParameter (n, u)) -> (ident_string n) ^ " : " ^ (universe_string u)) typarams)))*)
    ()

let is_boolean = function
  | Boolean -> true
  | _ -> false

let is_compatible_with_size_type = function
  | TIntConstant _ ->
     true
  | e ->
     (get_type e) = size_type

type stmt_ctx = module_name * env * region_map * type_parameter list * lexenv * ty

let update_lexenv (mn, menv, rm, typarams, _, rt) lexenv =
  (mn, menv, rm, typarams, lexenv, rt)

let update_rm (mn, menv, _, typarams, lexenv, rt) rm =
  (mn, menv, rm, typarams, lexenv, rt)

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

let rec augment_stmt (ctx: stmt_ctx) (stmt: astmt): tstmt =
  with_frame ("Augment statement: " ^ (stmt_kind stmt))
    (fun _ ->
      let (module_name, env, rm, typarams, lexenv, rt) = ctx in
      match stmt with
      | ASkip span ->
         TSkip span
      | ALet (span, name, ty, value, body) ->
         pi ("Name", name);
         adorn_error_with_span span
           (fun _ ->
             let expected_ty = parse_typespec env rm typarams ty in
             pt ("Expected type", expected_ty);
             let value' = augment_expr module_name env rm typarams lexenv (Some expected_ty) value in
             let bindings = match_type_with_value expected_ty value' in
             ps ("Bindings", show_bindings bindings);
             let ty = replace_variables bindings expected_ty in
             pt ("Type", ty);
             pt ("Value type", get_type value');
             let lexenv' = push_var lexenv name ty VarLocal in
             let body' = augment_stmt (update_lexenv ctx lexenv') body in
             TLet (span, name, ty, value', body'))
      | ADestructure (span, bindings, value, body) ->
         adorn_error_with_span span
           (fun _ ->
             let value' = augment_expr module_name env rm typarams lexenv None value in
             (* Check: the value must be a public record type *)
             let rec_ty = get_type value' in
             (match rec_ty with
              | (NamedType (name, _, u)) ->
                 let (source_module, vis, record_typarams, slots) = get_record_definition env name in
                 let orig_type = NamedType (
                                     make_qident (source_module, original_name name, original_name name),
                                     List.map (fun (TypeParameter (n, u, from)) -> TyVar (TypeVariable (n, u, from))) record_typarams,
                                     u
                                   )
                 in
                 let typebindings = match_type orig_type rec_ty in
                 if (vis = TypeVisPublic) || (module_name = source_module) then
                   (* Find the set of slot names and the set of binding names, and compare them *)
                   let binding_names = List.map (fun (n, _) -> n) bindings
                   and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
                   if ident_set_eq binding_names slot_names then
                     let bindings' = group_bindings_slots bindings slots in
                     let bindings'' = List.map (fun (n, ty, actual) -> (n, parse_typespec env rm typarams ty, replace_variables typebindings actual)) bindings' in
                     let newvars = List.map (fun (n, ty, actual) ->
                                       let _ = match_type ty actual in
                                       (n, ty, VarLocal))
                                     bindings'' in
                     let lexenv' = push_vars lexenv newvars in
                     let body' = augment_stmt (update_lexenv ctx lexenv') body in
                     TDestructure (
                         span,
                         List.map (fun (n, _, t) -> (n, t)) bindings'',
                         value',
                         body'
                       )
                   else
                     err "Destructuring a record: not the same set of bindings and slots"
                 else
                   err "Not a public record"
              | _ ->
                 err "Not a record type"))
      | AAssign (span, LValue (var, elems), value) ->
         adorn_error_with_span span
           (fun _ ->
             (match get_var lexenv var with
              | Some (var_ty, _) ->
                 let elems = augment_lvalue_path env module_name rm typarams lexenv var_ty elems in
                 let value = augment_expr module_name env rm typarams lexenv None value in
                 let path = TPath {
                                head = value;
                                elems = elems;
                                ty = get_path_ty_from_elems elems
                              }
                 in
                 let universe = type_universe (get_type path) in
                 if universe = FreeUniverse then
                   TAssign (span, TypedLValue (var, elems), value)
                 else
                   err "Paths must end in the free universe"
              | None ->
                 err "No var with this name."))
      | AIf (span, c, t, f) ->
         adorn_error_with_span span
           (fun _ ->
             let c' = augment_expr module_name env rm typarams lexenv None c in
             if is_boolean (get_type c') then
               TIf (span, c', augment_stmt ctx t, augment_stmt ctx f)
             else
               err "The type of the condition in an if statement must be a boolean.")
      | AWhen (span, c, t) ->
         adorn_error_with_span span
           (fun _ ->
             let c = augment_expr module_name env rm typarams lexenv None c in
             if is_boolean (get_type c) then
               TIf (span, c, augment_stmt ctx t, TSkip span)
             else
               err "The type of the condition in an if statement must be a boolean.")
      | ACase (span, expr, whens) ->
         (* Type checking a case statement:

            1. Ensure the value is of a union type.
            2. Ensure the union type is public or it is defined in this module.
            3. Ensure the set of case names in the case statement equals the set of cases in the union definition.
            4. Iterate over the cases, and ensure the bindings are correct.
          *)
         adorn_error_with_span span
           (fun _ ->
             let expr' = augment_expr module_name env rm typarams lexenv None expr in
             let ty = get_type expr' in
             pt ("Case type", ty);
             let (union_ty, cases) = get_union_type_definition module_name env (get_type expr') in
             let typebindings = match_type union_ty ty in
             let case_names = List.map (fun (TypedCase (n, _)) -> n) (List.map union_case_to_typed_case cases) in
             let when_names = List.map (fun (AbstractWhen (n, _, _)) -> n) whens in
             if ident_set_eq case_names when_names then
               (* Group the cases and whens *)
               let whens' = group_cases_whens cases whens in
               let whens'' = List.map (fun (c, w) -> augment_when ctx typebindings w c) whens' in
               TCase (span, expr', whens'')
             else
               err "Non-exhaustive case statement.")
      | AWhile (span, c, body) ->
         adorn_error_with_span span
           (fun _ ->
             let c' = augment_expr module_name env rm typarams lexenv None c in
             if is_boolean (get_type c') then
               TWhile (span, c', augment_stmt ctx body)
             else
               err "The type of the condition in a while loop must be a boolean")
      | AFor { span; name; initial; final; body; } ->
         adorn_error_with_span span
           (fun _ ->
             let i' = augment_expr module_name env rm typarams lexenv None initial
             and f' = augment_expr module_name env rm typarams lexenv None final in
             if is_compatible_with_size_type i' then
               if is_compatible_with_size_type f' then
                 let lexenv' = push_var lexenv name size_type VarLocal in
                 let b' = augment_stmt (update_lexenv ctx lexenv') body in
                 TFor (span, name, i', f', b')
               else
                 err "The type of the final value in a for loop must be an integer type."
             else
               err "The type of the initial value in a for loop must be an integer type.")
      | ABorrow { span; original; rename; region; body; mode; } ->
         adorn_error_with_span span
           (fun _ ->
             (match get_var lexenv original with
              | (Some (orig_ty, _)) ->
                 let u = type_universe orig_ty in
                 if ((u = LinearUniverse) || (u = TypeUniverse)) then
                   let region_obj = fresh_region region in
                   let refty =
                     (match mode with
                      | ReadBorrow ->
                         ReadRef (orig_ty, RegionTy region_obj)
                      | WriteBorrow ->
                         WriteRef (orig_ty, RegionTy region_obj))
                   in
                   let lexenv' = push_var lexenv rename refty VarLocal in
                   let rm' = add_region rm region region_obj in
                   let ctx' = update_lexenv ctx lexenv' in
                   let ctx''= update_rm ctx' rm' in
                   TBorrow {
                       span=span;
                       original=original;
                       rename=rename;
                       region=region;
                       orig_type=orig_ty;
                       ref_type=refty;
                       body=augment_stmt ctx'' body;
                       mode=mode
                     }
                 else
                   err "Cannot borrow a non-linear type."
              | None ->
                 err "No variable with this name."))
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
               err "Discarding a linear value"
             else
               TDiscarding (span, e'))
      | AReturn (span, e) ->
         adorn_error_with_span span
           (fun _ ->
             let e' = augment_expr module_name env rm typarams lexenv None e in
             pt ("Type", get_type e');
             let _ = match_type_with_value rt e' in
             TReturn (span, e')))

and augment_lvalue_path (env: env) (module_name: module_name) (rm: region_map) (typarams: type_parameter list) (lexenv: lexenv) (head_ty: ty) (elems: path_elem list): typed_path_elem list =
  match elems with
  | [elem] ->
     [augment_lvalue_path_elem env module_name rm typarams lexenv head_ty elem]
  | elem::rest ->
     let elem' = augment_lvalue_path_elem env module_name rm typarams lexenv head_ty elem in
     let rest' = augment_lvalue_path env module_name rm typarams lexenv (path_elem_type elem') rest in
     elem' :: rest'
  | [] ->
     err "Path is empty"

and augment_lvalue_path_elem (env: env) (module_name: module_name) (rm: region_map) (typarams: type_parameter list) (lexenv: lexenv) (head_ty: ty) (elem: path_elem): typed_path_elem =
  match elem with
  | SlotAccessor slot_name ->
     (match head_ty with
      | NamedType (name, args, _) ->
         augment_slot_accessor_elem env module_name slot_name name args
      | _ ->
         err "Not a record type")
  | PointerSlotAccessor slot_name ->
     (match head_ty with
      | Pointer pointed_to ->
         (* TODO: addresses should not be indexable *)
         augment_pointer_slot_accessor_elem env module_name slot_name pointed_to
      | WriteRef (ty, _) ->
         (match ty with
          | NamedType (name, args, _) ->
             augment_reference_slot_accessor_elem env module_name slot_name name args
          | _ ->
             err "Not a record type")
      | _ ->
         err "Not a record type")
  | ArrayIndex ie ->
     let ie' = augment_expr module_name env rm typarams lexenv None ie in
     let _ = ie' in
     (match head_ty with
      | WriteRef (ref_ty, _) ->
         (match ref_ty with
          | _ ->
             err ("Can't index this type: " ^ (type_string ref_ty)))
      | _ ->
         err "Array index operator doesn't work for this type.")


and get_union_type_definition (importing_module: module_name) (env: env) (ty: ty): (ty * decl list) =
  let name: qident = (match ty with
                      | NamedType (n, _, _) ->
                         n
                      | _ ->
                         err "Not a named type") in
  match get_decl_by_name env (qident_to_sident name) with
  | Some (Union { id; vis; name; mod_id; typarams; universe; _ }) ->
     let module_name = module_name_from_id env mod_id in
     if (vis = TypeVisPublic) || (importing_module = module_name) then
       let n = make_qident (module_name, name, name)
       and args = List.map (fun (TypeParameter (n, u, from)) -> TyVar (TypeVariable (n, u, from))) typarams
       in
       let ty = NamedType (n, args, universe)
       and cases = get_union_cases env id in
       (ty, cases)
     else
       err "Union must be public or from the same module to be used in a case statement."
  | _ ->
     err "Not a union type"

and group_cases_whens (cases: decl list) (whens: abstract_when list): (typed_case * abstract_when) list =
  List.map (fun (TypedCase (n, s)) -> (TypedCase (n, s), List.find (fun (AbstractWhen (n', _, _)) -> n = n') whens))
    (List.map union_case_to_typed_case cases)

and group_bindings_slots (bindings: (identifier * qtypespec) list) (slots: typed_slot list): (identifier * qtypespec * ty) list =
  List.map (fun (n, t) -> (n, t, let (TypedSlot (_, ty')) = List.find (fun (TypedSlot (n', _)) -> n = n') slots in ty')) bindings

and augment_when (ctx: stmt_ctx) (typebindings: type_bindings) (w: abstract_when) (c: typed_case): typed_when =
  with_frame "Augment when"
    (fun _ ->
      let (_, menv, rm, typarams, lexenv, _) = ctx in
      let (AbstractWhen (name, bindings, body)) = w
      and (TypedCase (_, slots)) = c in
      pi ("Case name", name);
      (* Check the set of binding names is the same as the set of slots *)
      let binding_names = List.map (fun (n, _) -> n) bindings
      and slot_names = List.map (fun (TypedSlot (n, _)) -> n) slots in
      if ident_set_eq binding_names slot_names then
        (* Check the type of each binding matches the type of the slot *)
        let bindings' = group_bindings_slots bindings slots in
        let bindings'' = List.map (fun (n, ty, actual) -> (n, parse_typespec menv rm typarams ty, replace_variables typebindings actual)) bindings' in
        let newvars = List.map (fun (n, ty, actual) ->
                          if equal_ty ty actual then
                            let _ = pi ("Binding name", n)
                            in
                            pt ("Binding type",  ty);
                            (n, ty, VarLocal)
                          else
                            err ("Slot type mismatch: expected \n\n" ^ (type_string ty) ^ "\n\nbut got:\n\n" ^ (type_string actual)))
                        bindings'' in
        let lexenv' = push_vars lexenv newvars in
        let body' = augment_stmt (update_lexenv ctx lexenv') body in
        TypedWhen (name, List.map (fun (n, t, _) -> ValueParameter (n, t)) bindings'', body')
      else
        err "The set of slots in the case statement doesn't match the set of slots in the union definition.")

let rec validate_constant_expression (expr: texpr): unit =
  if is_constant expr then
    ()
  else
    err ("The value of this constant is not a valid constant expression.")

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
  | TArithmetic (_, lhs, rhs) ->
     (is_constant lhs) && (is_constant rhs)
  | TFuncall _ ->
     false
  | TMethodCall _ ->
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
  | TTypeAliasConstructor (_, e) ->
     is_constant e
  | TPath { head; elems; _ } ->
     (is_constant head) && (List.for_all is_path_elem_constant elems)
  | TEmbed _ ->
     true
  | TDeref _ ->
     false
  | TSizeOf _ ->
     true

and is_path_elem_constant = function
  | TSlotAccessor _ ->
     true
  | TPointerSlotAccessor _ ->
     true
  | TArrayIndex (e, _) ->
     is_constant e

let rec augment_decl (module_name: module_name) (kind: module_kind) (env: env) (decl: linked_definition): typed_decl =
  with_frame "Augment declaration"
    (fun _ ->
      match decl with
      | LConstant (decl_id, vis, name, ty, expr, doc) ->
         ps ("Kind", "Constant");
         let expr' = augment_expr module_name env empty_region_map [] empty_lexenv (Some ty) expr in
         let _ = match_type_with_value ty expr' in
         let _ = validate_constant_expression expr' in
         TConstant (decl_id, vis, name, ty, expr', doc)
      | LTypeAlias (decl_id, vis, name, typarams, universe, ty, doc) ->
         ps ("Kind", "Type Alias");
         TTypeAlias (decl_id, vis, name, typarams, universe, ty, doc)
      | LRecord (decl_id, vis, name, typarams, universe, slots, doc) ->
         ps ("Kind", "Record");
         TRecord (decl_id, vis, name, typarams, universe, slots, doc)
      | LUnion (decl_id, vis, name, typarams, universe, cases, doc) ->
         ps ("Kind", "Union");
         TUnion (decl_id, vis, name, typarams, universe, cases, doc)
      | LFunction (decl_id, vis, name, typarams, params, rt, body, doc, pragmas) ->
         ps ("Kind", "Function");
         pi ("Name", name);
         let rm = region_map_from_typarams typarams in
         (match pragmas with
          | [ForeignImportPragma s] ->
             if typarams = [] then
               if kind = UnsafeModule then
                 TForeignFunction (decl_id, vis, name, params, rt, s, doc)
               else
                 err "Can't declare a foreign function in a safe module."
             else
               err "Foreign functions can't have type parameters."
          | [] ->
             let ctx = (module_name, env, rm, typarams, (lexenv_from_params params), rt) in
             let body' = augment_stmt ctx body in
             TFunction (decl_id, vis, name, typarams, params, rt, body', doc)
          | _ ->
             err "Invalid pragmas")
      | LTypeclass (decl_id, vis, name, typaram, methods, doc) ->
         ps ("Kind", "Typeclass");
         let rm = region_map_from_typarams [typaram] in
         TTypeClass (decl_id, vis, name, typaram, List.map (augment_method_decl env rm typaram) methods, doc)
      | LInstance (decl_id, vis, name, typarams, arg, methods, doc) ->
         ps ("Kind", "Instance");
         (* TODO: the universe of the type parameter matches the universe of the type argument *)
         (* TODO: Check the methods in the instance match the methods in the class *)
         let rm = region_map_from_typarams typarams in
         TInstance (decl_id, vis, name, typarams, arg, List.map (augment_method_def module_name env rm typarams) methods, doc))

and lexenv_from_params (params: value_parameter list): lexenv =
  match params with
  | (ValueParameter (n, t))::rest ->
     push_var (lexenv_from_params rest) n t VarParam
  | [] ->
     empty_lexenv

and augment_method_decl _ _ _ (LMethodDecl (decl_id, name, params, rt, _)) =
  TypedMethodDecl (decl_id, name, params, rt)

and augment_method_def module_name menv rm typarams (LMethodDef (ins_meth_id, name, params, rt, _, body)) =
  let ctx = (module_name, menv, rm, typarams, (lexenv_from_params params), rt) in
  let body' = augment_stmt ctx body in
  TypedMethodDef (ins_meth_id, name, params, rt, body')

let augment_module menv (LinkedModule { name; decls; kind; _ }) =
  with_frame "Typing pass"
    (fun _ ->
      let decls' = List.map (augment_decl name kind menv) decls in
      let _ = List.map check_decl_linearity decls' in
      TypedModule (name, decls'))
