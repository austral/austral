open Identifier
open Type
open TypeSystem
open Region
open Ast
open ModuleSystem
open Semantic
open Error

let parse_built_in_type name args =
  match name with
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
  | "Single_Float" ->
     Some SingleFloat
  | "Double_Float" ->
     Some DoubleFloat
  | "Fixed_String" ->
     Some string_type
  | "Static" ->
     Some (RegionTy static_region)
  | "Fixed_Array" ->
     (match args with
      | [ty] ->
         Some (Array (ty, static_region))
      | _ ->
         err "Invalid Fixed_Array type specifier.")
  | _ ->
     None

let rec effective_universe typarams declared_universe args =
  (* Algorithm:

     1. If the declared universe is Free then none of the type parameters can be Linear or Type.

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
       err "Free type called with non-free argument"
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

and any_arg_is_linear (args: ty list) =
  let is_linear = function
    | LinearUniverse -> true
    | _ -> false
  in
  List.exists is_linear (List.map type_universe args)

and any_arg_is_type (args: ty list) =
  let is_type = function
    | TypeUniverse -> true
    | _ -> false
  in
  List.exists is_type (List.map type_universe args)

(* Type signature retrieval *)

let get_type_signature menv sigs name =
  let get_local_type_signature (sigs: type_signature list) (name: qident): type_signature option =
    List.find_opt (fun (TypeSignature (n, _, _)) -> n = (local_name name)) sigs

  and get_foreign_type_signature (menv: menv) (name: qident): type_signature option =
    match get_decl menv name with
    | (Some decl) ->
       decl_type_signature decl
    | None ->
     None
  in
  match get_local_type_signature sigs name with
  | (Some ts) ->
     Some ts
  | None ->
     get_foreign_type_signature menv name

(* Parsing *)

let rec parse_type (menv: menv) (sigs: type_signature list) (rm: region_map) (typarams: type_parameter list) (QTypeSpecifier (name, args)) =
  let args' = List.map (parse_type menv sigs rm typarams) args in
  match parse_built_in_type (ident_string (original_name name)) args' with
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
             parse_user_defined_type menv sigs name args'))

(* Is the given name a type parameter in the list of type paramters? If so,
   return it as a type variable. *)
and is_param (typarams: type_parameter list) (name: qident): ty option =
  let name' = original_name name
  in
  match List.find_opt (fun (TypeParameter (n, _)) -> n = name') typarams with
  | Some (TypeParameter (_, u)) ->
     Some (TyVar (TypeVariable (name', u)))
  | None ->
     None

(* Is the given name a region parameter? If so, return it as a RegionTy instance
   by finding the region in the region map. *)
and is_region (rm: region_map) (name: qident): ty option =
  match get_region rm (original_name name) with
  | (Some r) -> Some (RegionTy r)
  | None -> None

and parse_user_defined_type (menv: menv) (sigs: type_signature list) (name: qident) (args: ty list): ty =
  match get_type_signature menv sigs name with
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
  let universe = effective_universe ts_params declared_universe args in
  NamedType (name, args, universe)

and check_param_arity_matches (params: type_parameter list) (args: ty list): unit =
  assert ((List.length params) = (List.length args))

and check_universes_match (params: type_parameter list) (args: ty list): unit =
  let _ = List.map2 check_universes_match' params args in ()

and check_universes_match' (TypeParameter (_, param_u)) (arg: ty): unit =
  let arg_u = type_universe arg in
  if universe_compatible param_u arg_u then
    ()
  else
    err "Universe mismatch"

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
