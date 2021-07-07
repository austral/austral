open Identifier
open Type

type type_vis =
  | TypeVisPublic
  | TypeVisOpaque
  | TypeVisPrivate

type vis =
  | VisPublic
  | VisPrivate

(* Semantic modules represent everything we need to know
   about a module to perform semantic analysis on it. That
   is, we know the structure of types, type parameters,
   function signatures, etc. but no code. *)
type semantic_module =
  SemanticModule of {
      name: module_name;
      decls: sem_decl list;
      imported_classes: semantic_typeclass list;
      imported_instances: semantic_instance list
    }

and sem_decl =
  | SConstantDefinition of vis * identifier * ty
  | STypeAliasDefinition of type_vis * identifier * type_parameter list * universe * ty
  | SRecordDefinition of module_name * type_vis * identifier * type_parameter list * universe * typed_slot list
  | SUnionDefinition of module_name * type_vis * identifier * type_parameter list * universe * typed_case list
  | SFunctionDeclaration of vis * identifier * type_parameter list * value_parameter list * ty
  | STypeClassDecl of semantic_typeclass
  | STypeClassInstanceDecl of semantic_instance

and semantic_typeclass =
  STypeClass of vis * identifier * type_parameter * semantic_method_decl list

and semantic_instance =
  STypeClassInstance of vis * identifier * type_parameter list * ty * semantic_method_decl list

and semantic_method_decl =
  SMethodDecl of identifier * value_parameter list * ty

and callable =
  | FunctionCallable of type_parameter list * value_parameter list * ty
  | TypeAliasCallable of type_parameter list * universe * ty
  | RecordConstructor of type_parameter list * universe * typed_slot list
  | UnionConstructor of {
      type_name: qident;
      type_params: type_parameter list;
      universe: universe;
      case: typed_case;
    }
  | MethodCallable of {
      type_class_name: qident;
      type_class_type_parameter: type_parameter;
      method_name: identifier;
      value_parameters: value_parameter list;
      return_type: ty
    }

let visible_instances (SemanticModule { decls; imported_instances; _ }) =
  let get_instance = function
    | STypeClassInstanceDecl decl -> Some decl
    | _ -> None
  in
  let local_instances = List.filter_map get_instance decls in
  List.append imported_instances local_instances

let get_declaration (SemanticModule { decls; _ }, name) =
  let is_decl = function
    | SConstantDefinition (_, n, _) -> name = n
    | STypeAliasDefinition (_, n, _, _, _) -> name = n
    | SRecordDefinition (_, _, n, _, _, _) -> name = n
    | SUnionDefinition (_, _, n, _, _, _) -> name = n
    | SFunctionDeclaration (_, n, _, _, _) -> name = n
    | STypeClassDecl (STypeClass (_, n, _, _)) -> name = n
    | STypeClassInstanceDecl _ -> false
  in
  List.find_opt is_decl decls

let get_callable_typeclass source_module importing_module callable_name (STypeClass (vis, type_class_name, param, methods)) =
  (* A typeclass is callable if the callable name is the name of one of the
     methods and the typeclass is public. *)
  (* TODO: Same as above *)
  match (List.find_opt (fun (SMethodDecl (n, _, _)) -> n = callable_name) methods) with
  | (Some (SMethodDecl (_, params, rt))) ->
     let type_class_name' = make_qident (source_module, type_class_name, type_class_name) in
     if ((vis = VisPublic) || (source_module == importing_module)) then
       Some (MethodCallable {
                 type_class_name = type_class_name';
                 type_class_type_parameter = param;
                 method_name = callable_name;
                 value_parameters = params;
                 return_type = rt
         })
     else
       None
  | None -> None

(* Given a callable, the name of the module that contains it, the name of the
   importing module, and the name of a callable, see if they match. *)
let get_callable_decl source_module importing_module callable_name decl  =
  match decl with
  | SFunctionDeclaration (vis, name, typarams, params, rt) ->
     (* A function is callable if its name is the same as the callable name,
        and if it's either public or in the same module. *)
     if (name = callable_name) && ((vis = VisPublic) || (source_module == importing_module)) then
       Some (FunctionCallable (typarams, params, rt))
     else
       None
  | (STypeAliasDefinition (vis, name, typarams, universe, ty)) ->
     if (name = callable_name) && ((vis = TypeVisPublic) || (source_module == importing_module)) then
       Some (TypeAliasCallable (typarams, universe, ty))
     else
       None
  | SRecordDefinition (_, vis, name, typarams, universe, slots) ->
     (* A record is callable if its name is the same as the callable name, and
        it's either public (not opaque) or in the same module. *)
     if (name = callable_name) && ((vis = TypeVisPublic) || (source_module == importing_module)) then
       Some (RecordConstructor (typarams, universe, slots))
     else
       None
  | SUnionDefinition (_, vis, _, typarams, universe, cases) ->
     (* A union is callable if the callable name is the name of a case, and
        the union is public or is in the same module. *)
     (* TODO: Here we're assuming the local (import nickname) name is the same as the
        original name. This may lead to confusing error messages. To do this properly,
        we should look at the import map of the importing module and find how the name
        of the union type is being qualified. *)
     (match (List.find_opt (fun (TypedCase (n, _)) -> n == callable_name) cases) with
      | (Some case) ->
         let name' = make_qident (source_module, callable_name, callable_name) in
         if ((vis = TypeVisPublic) || (source_module == importing_module)) then
           Some (UnionConstructor {
                     type_name = name';
                     type_params = typarams;
                     universe = universe;
                     case = case
             })
         else
           None
      | None -> None)
  | (STypeClassDecl type_class) ->
     get_callable_typeclass source_module importing_module callable_name type_class
  (* Not callables *)
  | (SConstantDefinition _) -> None
  | (STypeClassInstanceDecl _) -> None

(* TODO: Refactor into is_callable_with_name, is_importable, as_callable *)

let get_module_callable (SemanticModule { name=source_module_name; decls; imported_classes; _ }, importing_module_name, callable_name) =
  match List.find_map (get_callable_decl source_module_name importing_module_name callable_name) decls with
  | (Some decl) -> Some decl
  | None ->
     List.find_map (get_callable_typeclass source_module_name importing_module_name callable_name) imported_classes

let decl_type_signature = function
  | SConstantDefinition _ ->
     None
  | STypeAliasDefinition (_, n, p, u, _) ->
     Some (TypeSignature (n, p, u))
  | SRecordDefinition (_, _, n, p, u, _) ->
     Some (TypeSignature (n, p, u))
  | SUnionDefinition (_, _, n, p, u, _) ->
     Some (TypeSignature (n, p, u))
  | SFunctionDeclaration _ ->
     None
  | STypeClassDecl _ ->
     None
  | STypeClassInstanceDecl _ ->
     None

let rec is_importable = function
  | SConstantDefinition (vis, _ ,_) ->
     is_public vis
  | STypeAliasDefinition (vis, _, _, _, _) ->
     is_public_or_opaque vis
  | SRecordDefinition (_, vis, _, _, _, _) ->
     is_public_or_opaque vis
  | SUnionDefinition (_, vis, _, _, _, _) ->
     is_public_or_opaque vis
  | SFunctionDeclaration (vis, _, _, _, _) ->
     is_public vis
  | STypeClassDecl (STypeClass (vis, _, _, _)) ->
     is_public vis
  | STypeClassInstanceDecl (STypeClassInstance (vis, _, _, _, _)) ->
     is_public vis

and is_public = function
  | VisPublic -> true
  | VisPrivate -> false

and is_public_or_opaque = function
  | TypeVisPublic -> true
  | TypeVisOpaque -> true
  | TypeVisPrivate -> false
