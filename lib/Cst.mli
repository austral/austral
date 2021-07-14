open Identifier
open Common
open Type

type concrete_module_interface =
  ConcreteModuleInterface of module_name * concrete_import_list list * concrete_decl list

and concrete_module_body =
  ConcreteModuleBody of module_name * concrete_import_list list * concrete_def list

and concrete_import_list =
  ConcreteImportList of module_name * concrete_import list

and concrete_import =
  ConcreteImport of identifier * identifier option

and concrete_decl =
  | ConcreteConstantDecl of identifier * typespec * docstring
  | ConcreteOpaqueTypeDecl of identifier * type_parameter list * universe * docstring
  | ConcreteTypeAliasDecl of concrete_type_alias
  | ConcreteRecordDecl of concrete_record
  | ConcreteUnionDecl of concrete_union
  | ConcreteFunctionDecl of identifier * type_parameter list * concrete_param list * typespec * docstring
  | ConcreteTypeClassDecl of concrete_typeclass
  | ConcreteInstanceDecl of identifier * type_parameter list * typespec * docstring

and concrete_def =
  | ConcreteConstantDef of identifier * typespec * cexpr * docstring
  | ConcreteTypeAliasDef of concrete_type_alias
  | ConcreteRecordDef of concrete_record
  | ConcreteUnionDef of concrete_union
  | ConcreteFunctionDef of identifier * type_parameter list * concrete_param list * typespec * cstmt * docstring * pragma list
  | ConcreteTypeClassDef of concrete_typeclass
  | ConcreteInstanceDef of concrete_instance

and concrete_type_alias =
  ConcreteTypeAlias of identifier * type_parameter list * universe * typespec * docstring

and concrete_record =
  ConcreteRecord of identifier * type_parameter list * universe * concrete_slot list * docstring

and concrete_union =
  ConcreteUnion of identifier * type_parameter list * universe * concrete_case list * docstring

and concrete_slot =
  ConcreteSlot of identifier * typespec

and concrete_case =
  ConcreteCase of identifier * concrete_slot list

and concrete_typeclass =
  ConcreteTypeClass of identifier * type_parameter * concrete_method_decl list * docstring

and concrete_instance =
  ConcreteInstance of identifier * type_parameter list * typespec * concrete_method_def list * docstring

and concrete_method_decl =
  ConcreteMethodDecl of identifier * concrete_param list * typespec * docstring

and concrete_method_def =
  ConcreteMethodDef of identifier * concrete_param list * typespec * cstmt * docstring

and typespec =
  TypeSpecifier of identifier * typespec list

and cexpr =
  | CNilConstant
  | CBoolConstant of bool
  | CIntConstant of string
  | CFloatConstant of string
  | CStringConstant of string
  | CVariable of identifier
  | CArith of arithmetic_operator * cexpr * cexpr
  | CFuncall of identifier * concrete_arglist
  | CComparison of comparison_operator * cexpr * cexpr
  | CConjunction of cexpr * cexpr
  | CDisjunction of cexpr * cexpr
  | CNegation of cexpr
  | CIfExpression of cexpr * cexpr * cexpr
  | CPath of cexpr * concrete_path_elem list

and cstmt =
  | CSkip
  | CLet of identifier * typespec * cexpr
  | CAssign of identifier * cexpr
  | CIf of cexpr * cstmt * cstmt
  | CCase of cexpr * concrete_when list
  | CWhile of cexpr * cstmt
  | CFor of identifier * cexpr * cexpr * cstmt
  | CBorrow of {
      original: identifier;
      rename: identifier;
      region: identifier;
      body: cstmt
    }
  | CBlock of cstmt list
  | CDiscarding of cexpr
  | CReturn of cexpr

and condition_branch =
  ConditionBranch of cexpr * cstmt

and concrete_when =
  ConcreteWhen of identifier * concrete_param list * cstmt

and concrete_arglist =
  | ConcretePositionalArgs of cexpr list
  | ConcreteNamedArgs of (identifier * cexpr) list

and concrete_param =
  ConcreteParam of identifier * typespec

and concrete_path_elem =
  CSlotAccessor of identifier

val decl_name : concrete_decl -> identifier option

val def_name : concrete_def -> identifier option

val get_concrete_decl : concrete_module_interface -> identifier -> concrete_decl option

val get_concrete_def : concrete_module_body -> identifier -> concrete_def option

val has_instance_decl : concrete_module_interface -> identifier -> type_parameter list -> typespec -> bool

val get_instance_def : concrete_module_body -> identifier -> type_parameter list -> typespec -> concrete_instance option

val make_pragma : identifier -> concrete_arglist -> pragma

val append_import_to_interface : concrete_module_interface -> concrete_import_list -> concrete_module_interface

val append_import_to_body : concrete_module_body -> concrete_import_list -> concrete_module_body
