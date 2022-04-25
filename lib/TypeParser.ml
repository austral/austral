open Identifier
open Names
open Type
open TypeSystem
open Region
open Ast
open Env
open Error

let decl_type_signature (decl: decl): type_signature option =
  match decl with
  | Constant _ ->
     None
  | TypeAlias { name; typarams; universe; _ } ->
     Some (TypeSignature (name, typarams, universe))
  | Record { name; typarams; universe; _ } ->
     Some (TypeSignature (name, typarams, universe))
  | Union { name; typarams; universe; _ } ->
     Some (TypeSignature (name, typarams, universe))
  | UnionCase _ ->
     None
  | Function _ ->
     None
  | TypeClass _ ->
     None
  | TypeClassMethod _ ->
     None
  | Instance _ ->
     None

let memory_module_name = make_mod_name "Austral.Memory"

let is_address_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name)
  && (equal_identifier o (make_ident address_name))

let is_pointer_type (name: qident): bool =
  let s = source_module_name name
  and o = original_name name
  in
  (equal_module_name s memory_module_name)
  && (equal_identifier o (make_ident pointer_name))

let parse_built_in_type (name: qident) (args: ty list): ty option =
  if is_address_type name then
    match args with
    | [ty] ->
       Some (Address ty)
    | _ ->
       err "Invalid Address type specifier."
  else
    if is_pointer_type name then
      match args with
      | [ty] ->
         Some (Pointer ty)
      | _ ->
         err "Invalid Pointer type specifier."
    else
      let name_str: string = ident_string (original_name name) in
      match name_str with
      | "Unit" ->
         Some Unit
      | "Boolean" ->
         Some Boolean
      | "Natural_8" ->
         Some (Integer (Unsigned, Width8))
      | "Natural_16" ->
         Some (Integer (Unsigned, Width16))
      | "Natural_32" ->
         Some (Integer (Unsigned, Width32))
      | "Natural_64" ->
         Some (Integer (Unsigned, Width64))
      | "Integer_8" ->
         Some (Integer (Signed, Width8))
      | "Integer_16" ->
         Some (Integer (Signed, Width16))
      | "Integer_32" ->
         Some (Integer (Signed, Width32))
      | "Integer_64" ->
         Some (Integer (Signed, Width64))
      | "Index" ->
         Some (Integer (Unsigned, WidthIndex))
      | "Single_Float" ->
         Some SingleFloat
      | "Double_Float" ->
         Some DoubleFloat
      | "Static" ->
         Some (RegionTy static_region)
      | "Fixed_Array" ->
         (match args with
          | [ty] ->
             Some (StaticArray (ty, static_region))
          | _ ->
             err "Invalid Fixed_Array type specifier.")
      | "Reference" ->
         (match args with
          | [ty; ty'] ->
             let u = type_universe ty' in
             if (u = RegionUniverse) then
               Some (ReadRef (ty, ty'))
             else
               err "Reference error: Not a region"
          | _ ->
             err "Invalid Reference type specifier.")
      | "WriteReference" ->
         (match args with
          | [ty; ty'] ->
             let u' = type_universe ty' in
             if (u' = RegionUniverse) then
               Some (WriteRef (ty, ty'))
             else
               err "WriteReference error: Not a region"
          | _ ->
             err "Invalid WriteReference type specifier.")
      | _ ->
         None

let rec effective_universe name typarams declared_universe args =
  (* Algorithm:

     1. If the declared universe is Free then none of the type parameters can be Linear or Type.

         1.1 Unless it's `Pointer[T]` or `Address[T]`. But we don't have to take
         care of that here, since those are built in types, and the
         `type_universe` function determines their universe.

     2. If the declared universe is Linear, then no type parameter can change this. Therefore, the effective
        universe is Linear.

     3. If the declared universe is Region, that's an internal error: this function should not be called with a Region type.

     4. Finally, if the universe is Type:
         4.1. First, assert that there is at least one type parameter. This should have been checked by the
              compiler when declarations are processed.

         4.2. If any argument's universe is Linear, the effective universe is Linear.

         4.3. Otherwise, if any argument's universe is Type, the effective universe is Type. The ordering
              here is because Linear is stronger (i.e. lower in the poset of universes) than Type.

         4.4. Otherwise, the effective universe is Free.
   *)
  match declared_universe with
  | FreeUniverse ->
     if all_arguments_are_free args then
       FreeUniverse
     else
       err ("Free type called with non-free argument: " ^ (qident_debug_name name))
  | LinearUniverse ->
     LinearUniverse
  | RegionUniverse ->
     err "effective_universe called with a region type"
  | TypeUniverse ->
     assert ((List.length typarams) > 0);
     if any_arg_is_linear args then
       LinearUniverse
     else
       if any_arg_is_type args then
         TypeUniverse
       else
         FreeUniverse

(** Given a list of types, check that all of them are either in the Free or
    Region universes. *)
and all_arguments_are_free (args: ty list): bool =
  let is_compatible_with_free = function
    | FreeUniverse ->
       true
    | RegionUniverse ->
       true
    | _ ->
       false
  in
  List.for_all is_compatible_with_free (List.map type_universe args)

(** Given a list of types, return whether any of them are in the Linear
    universe. *)
and any_arg_is_linear (args: ty list) =
  let is_linear = function
    | LinearUniverse -> true
    | _ -> false
  in
  List.exists is_linear (List.map type_universe args)

(** Given a list of types, return whether any of them are in the Type
    universe. *)
and any_arg_is_type (args: ty list) =
  let is_type = function
    | TypeUniverse -> true
    | _ -> false
  in
  List.exists is_type (List.map type_universe args)

(* Type signature retrieval *)

let get_type_signature (env: env) (sigs: type_signature list) (name: qident) =
  let get_local_type_signature (sigs: type_signature list) (name: qident): type_signature option =
    List.find_opt (fun (TypeSignature (n, _, _)) -> n = (local_name name)) sigs

  and get_foreign_type_signature (env: env) (name: qident): type_signature option =
    match get_decl_by_name env (qident_to_sident name) with
    | (Some decl) ->
       decl_type_signature decl
    | None ->
       None
  in
  match get_local_type_signature sigs name with
  | (Some ts) ->
     Some ts
  | None ->
     get_foreign_type_signature env name

(* Parsing *)

let rec parse_type (env: env) (sigs: type_signature list) (rm: region_map) (typarams: type_parameter list) (QTypeSpecifier (name, args)) =
  let args' = List.map (parse_type env sigs rm typarams) args in
  match parse_built_in_type name args' with
  | Some ty ->
     ty
  | None ->
     (match is_region rm name with
      | Some ty ->
         ty
      | None ->
         (match is_param typarams name with
          | Some ty ->
             ty
          | None ->
             parse_user_defined_type env sigs name args'))

(* Is the given name a type parameter in the list of type paramters? If so,
   return it as a type variable. *)
and is_param (typarams: type_parameter list) (name: qident): ty option =
  let name' = original_name name
  in
  match List.find_opt (fun (TypeParameter (n, _, _)) -> equal_identifier n name') typarams with
  | Some (TypeParameter (_, u, from)) ->
     Some (TyVar (TypeVariable (name', u, from)))
  | None ->
     None

(* Is the given name a region parameter? If so, return it as a RegionTy instance
   by finding the region in the region map. *)
and is_region (rm: region_map) (name: qident): ty option =
  match get_region rm (original_name name) with
  | (Some r) -> Some (RegionTy r)
  | None -> None

and parse_user_defined_type (env: env) (sigs: type_signature list) (name: qident) (args: ty list): ty =
  match get_type_signature env sigs name with
  | Some ts ->
     parse_user_defined_type' ts name args
  | None ->
     err ("No user defined type with name: " ^ (qident_debug_name name))

and parse_user_defined_type' (ts: type_signature) (name: qident) (args: ty list): ty =
  let (TypeSignature (_, ts_params, declared_universe)) = ts in
  (* Check: the number of type parameters in the signature matches the number of
     type arguments *)
  check_param_arity_matches ts_params args;
  (* Check: the universe of each type argument matches the universe of each type
     parameter in the type signature. *)
  check_universes_match ts_params args;
  (* Construct the named type *)
  let universe = effective_universe name ts_params declared_universe args in
  NamedType (name, args, universe)

and check_param_arity_matches (params: type_parameter list) (args: ty list): unit =
  assert ((List.length params) = (List.length args))

and check_universes_match (params: type_parameter list) (args: ty list): unit =
  let _ = List.map2 check_universes_match' params args in ()

and check_universes_match' (TypeParameter (_, param_u, _)) (arg: ty): unit =
  let arg_u = type_universe arg in
  if universe_compatible param_u arg_u then
    ()
  else
    err ("Type parser: Universe mismatch: parameter universe is "
         ^ (universe_string param_u)
         ^ ", argument universe is "
         ^ (universe_string arg_u))

and universe_compatible param arg =
  (* The check here is:

     1. If the parameter universe is Free, the argument universe must be Free.
     2. If the parameter universe is Linear, the argument universe must be Linear.
     3. If the parameter universe is Type, the argument universe must be any one of Free, Linear, or
        type.
     4. If the parameter universe is Region, the argument universe must be Region.
   *)
  match param with
  | FreeUniverse ->
     arg = FreeUniverse
  | LinearUniverse ->
     arg = LinearUniverse
  | TypeUniverse ->
     (arg = FreeUniverse) || (arg = LinearUniverse) || (arg = TypeUniverse)
  | RegionUniverse ->
     arg = RegionUniverse
