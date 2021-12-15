open Identifier
open Common
open Type
open Tast

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
  | SFunctionDeclaration of vis * identifier * type_parameter list * value_parameter list * ty * tstmt option
  | STypeClassDecl of semantic_typeclass
  | STypeClassInstanceDecl of semantic_instance

and semantic_typeclass =
  STypeClass of vis * identifier * type_parameter * semantic_method_decl list

and semantic_instance =
  STypeClassInstance of vis * identifier * type_parameter list * ty * semantic_method_decl list

and semantic_method_decl =
  SMethodDecl of identifier * value_parameter list * ty * tstmt option

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

(* Return all instances defined in the module. *)
val defined_instances : semantic_module -> semantic_instance list

(* Return all typeclass instances in a module. These are
   not only the instances that are defined in the module
   itself, but the instances imported by the module. *)
val visible_instances : semantic_module -> semantic_instance list

(* Find a declaration in a module by name *)
val get_declaration : semantic_module * identifier -> sem_decl option

(* Find a callable in a semantic module. The second
   argument is the name of the module doing the
   importing. *)
val get_module_callable : semantic_module * module_name * identifier -> callable option

val decl_type_signature : sem_decl -> type_signature option

val is_importable : sem_decl -> bool

val has_union_constructor_with_name : semantic_module -> identifier -> bool

val has_method_with_name : semantic_module -> identifier -> bool
