open Id
open Identifier
open IdentifierSet
open Type
open TypeBindings
open TypeParameter
open TypeParameters
open Tast
open TastUtil
open Env
open EnvTypes
open EnvExtras
open Reporter
open Error

let type_mismatch _ a b =
  austral_raise TypeError
    [
      Text "Expected a value of type";
      Code (type_string a);
      Text ", but got a value of type";
      Code (type_string b);
    ]

type ctx = env * module_name

let ctx_env (env, _) = env

let ctx_mn (_, mn) = mn

let rec match_type (ctx: ctx) (a: ty) (b: ty): type_bindings =
  pt ("A", a);
  pt ("B", b);
  match a with
  | Unit ->
     (match b with
      | Unit ->
         empty_bindings
      | _ ->
         type_mismatch "Expected Unit, but got another type." a b)
  | Boolean ->
     (match b with
      | Boolean ->
         empty_bindings
      | _ ->
         type_mismatch "Expected Boolean, but got another type." a b)
  | Integer (s, w) ->
     (match b with
      | Integer (s', w') ->
         if (s = s') && (w = w') then
           empty_bindings
         else
           type_mismatch "Integer types don't match" a b
      | _ ->
         type_mismatch "Expected an integer, but got another type." a b)
  | SingleFloat ->
     (match b with
      | SingleFloat ->
         empty_bindings
      | _ ->
         type_mismatch "Expected SingleFloat, but got another type." a b)
  | DoubleFloat ->
     (match b with
      | DoubleFloat ->
         empty_bindings
      | _ ->
         type_mismatch "Expected DoubleFloat, but got another type." a b)
  | NamedType (n, args, _) ->
     (match b with
      | NamedType (n', args', _) ->
         (* We ignore the universe, since the type system is nominal. *)
         if n = n' then
           match_type_list ctx args args'
         else
           type_mismatch "Type mismatch" a b
      | _ ->
         type_mismatch "Expected a named type, but got something else." a b)
  | StaticArray t ->
     (match b with
      | StaticArray t' ->
         match_type ctx t t'
      | _ ->
         type_mismatch "Expected an array, but got another type." a b)
  | RegionTy r ->
     (match b with
      | RegionTy r' ->
         if r = r' then
           empty_bindings
         else
           type_mismatch "Region type mismatch" a b
      | _ ->
         type_mismatch "Expected a region, but got another type." a b)
  | ReadRef (t, r) ->
     (match b with
      | ReadRef (t', r') ->
         let bindings = match_type ctx t t' in
         let bindings' = match_type ctx r r' in
         merge_bindings bindings bindings'
      | _ ->
         type_mismatch "Expected a read reference, but got another type." a b)
  | WriteRef (t, r) ->
     (match b with
      | WriteRef (t', r') ->
         let bindings = match_type ctx t t' in
         let bindings' = match_type ctx r r' in
         merge_bindings bindings bindings'
      | _ ->
         type_mismatch "Expected a write reference, but got another type." a b)
  | TyVar tyvar ->
     match_type_var ctx tyvar b
  | Address t ->
     (match b with
      | Address t' ->
         match_type ctx t t'
      | _ ->
         type_mismatch "Expected an Address, but got another type." a b)
  | Pointer t ->
     (match b with
      | Pointer t' ->
         match_type ctx t t'
      | _ ->
         type_mismatch "Expected a Pointer, but got another type." a b)
  | FnPtr (args, rt) ->
     (match b with
      | FnPtr (args', rt') ->
         match_type_list ctx (rt :: args) (rt' :: args')
      | _ ->
         type_mismatch "Expected a function pointer, but got another type." a b)
  | MonoTy _ ->
     err "Not applicable"

and match_type_var (ctx: ctx) (tv: type_var) (ty: ty): type_bindings =
  let (TypeVariable (name, universe, from, constraints)) = tv in
  (* Check if the argument type is a variable. *)
  match ty with
  | (TyVar (TypeVariable (i', u', from', constraints'))) ->
     (* When the argument type is a type variable, check if the variables have
        the same name and provenance. *)
     if (equal_identifier name i') && (universe = u') && (equal_typaram_source from from') then
       (* If so, do nothing: we don't want circular bindings *)
       empty_bindings
     else
       (* Check that the tyvar implements the type variable's constraints, if any. *)
       let _ = check_tyvar_implements_constraints constraints constraints' in
       (* Otherwise, add a new binding.

          The idea here is: suppose we have a function `f` that accepts an
          argument of a generic type `T`. Now we're type-checking a function `g`
          that accepts an argument `x` of generic type `U`. If we encounter a
          function call `f(x)`, then the parameter type is `T` and the argument
          type is `U`, so we create a binding `T -> U`.  *)
       add_binding empty_bindings (tyvar_to_typaram tv) ty
  | _ ->
     (* Check that the type implements the type variable's constraints, if any. *)
     check_type_implements_constraints ctx ty constraints;
     (* If the constraints are satisfied, add a straightforward binding. *)
     add_binding empty_bindings (tyvar_to_typaram tv) ty

and check_tyvar_implements_constraints (param: sident list) (arg: sident list): bool =
  (* Check that the param is a subset of the param. *)
  let param: SIdentSet.t = SIdentSet.of_list param
  and arg: SIdentSet.t = SIdentSet.of_list arg
  in
  SIdentSet.subset param arg

and check_type_implements_constraints (ctx: ctx) (ty: ty) (constraints: sident list): unit =
  with_frame "Check type implements constraints"
    (fun _ ->
      pt ("Type", ty);
      ps ("Constraints", "[" ^ (String.concat ", " (List.map show_sident constraints)) ^ "]");
      (* If there are no constraints, do nothing. *)
      if constraints = [] then
        ()
      else
        (* If there are constraints, make them into a set. *)
        let constraints: SIdentSet.t = SIdentSet.of_list constraints in
        (* Try to find an instance for this type for each of the constraints. *)
        List.iter (try_constraint ctx ty) (List.of_seq (SIdentSet.to_seq constraints)))

and try_constraint (ctx: ctx) (ty: ty) (typeclass_name: sident): unit =
  (* Find the typeclass. *)
  match get_decl_by_name (ctx_env ctx) typeclass_name with
  | Some (TypeClass { id; _ }) ->
     (* Try to find an instance for the given typeclass name that implements this type. *)
     (match get_instance (ctx_env ctx) (ctx_mn ctx) ty id with
      | Some _ ->
         ()
      | None ->
         err "Type constraint not satisfied.")
  | Some _ ->
     internal_err "Type parameter constraint refers to a declaration which is not a typeclass."
  | None ->
     internal_err "No typeclass with the given name. This should have been validated in the extraction pass."

and get_instance (env: env) (source_module_name: module_name) (dispatch_ty: ty) (typeclass: decl_id): (decl * type_bindings) option =
  with_frame "Typeclass Resolution"
    (fun _ ->
      ps ("In module", (mod_name_string source_module_name));
      ps ("Typeclass", (get_decl_name_or_die env typeclass));
      pt ("Dispatch type", dispatch_ty);
      let mod_id: mod_id =
        let (ModRec { id; _ }) = Option.get (get_module_by_name env source_module_name) in
        id
      in
      let pred (decl: decl): (decl * type_bindings) option =
        match decl with
        | Instance { typeclass_id; argument; _ } ->
           if equal_decl_id typeclass_id typeclass then
             let _ = pt ("Trying instance with argument", argument) in
             try
               let bindings = match_type (env, source_module_name) argument dispatch_ty in
               ps ("Resolution", "Success");
               Some (decl, bindings)
             with
               Austral_error _ ->
               (* Does not match, just skip to the next instance, *)
               ps ("Resolution", "Failure");
               None
           else
             None
        | _ ->
           None
      in
      let filtered: (decl * type_bindings) list =
        with_frame "Filtering instances"
          (fun _ -> List.filter_map pred (visible_instances env mod_id))
      in
      match filtered with
      | [a] ->
         Some a
      | _::_ ->
         internal_err "Multiple instances satisfy this call. This is an internal error in the compiler: the compiler should validate that there are no overlapping instances."
      | [] ->
         None)

and match_type_list ctx tys tys' =
  let bs = List.map2 (match_type ctx) tys tys' in
  List.fold_left merge_bindings empty_bindings bs

let match_typarams (ctx: ctx) (typarams: typarams) (args: ty list): type_bindings =
  let typarams' = List.map (fun tp -> TyVar (typaram_to_tyvar tp)) (typarams_as_list typarams) in
  match_type_list ctx typarams' args

let match_type_with_value (ctx: ctx) (ty: ty) (expr: texpr): type_bindings =
  (* Like type_match, but gentler with integer constants. *)
  match ty with
  | Integer _ ->
     (match expr with
      | TIntConstant _ ->
         (* TODO: check sign against signedness *)
         (* TODO: check value against width *)
         empty_bindings
      | _ ->
         match_type ctx ty (get_type expr))
  | _ ->
     match_type ctx ty (get_type expr)
