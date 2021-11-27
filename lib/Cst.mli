open Identifier
open Common
open Type
open Span

type concrete_module_interface =
  ConcreteModuleInterface of module_name * concrete_import_list list * concrete_decl list

and concrete_module_body =
  ConcreteModuleBody of module_name * module_kind * concrete_import_list list * concrete_def list

and concrete_import_list =
  ConcreteImportList of module_name * concrete_import list

and concrete_import =
  ConcreteImport of identifier * identifier option

and concrete_decl =
  | ConcreteConstantDecl of identifier * typespec * docstring
  | ConcreteOpaqueTypeDecl of identifier * concrete_type_param list * universe * docstring
  | ConcreteTypeAliasDecl of concrete_type_alias
  | ConcreteRecordDecl of concrete_record
  | ConcreteUnionDecl of concrete_union
  | ConcreteFunctionDecl of identifier * concrete_type_param list * concrete_param list * typespec * docstring
  | ConcreteTypeClassDecl of concrete_typeclass
  | ConcreteInstanceDecl of identifier * concrete_type_param list * typespec * docstring

and concrete_def =
  | ConcreteConstantDef of identifier * typespec * cexpr * docstring
  | ConcreteTypeAliasDef of concrete_type_alias
  | ConcreteRecordDef of concrete_record
  | ConcreteUnionDef of concrete_union
  | ConcreteFunctionDef of identifier * concrete_type_param list * concrete_param list * typespec * cstmt * docstring * pragma list
  | ConcreteTypeClassDef of concrete_typeclass
  | ConcreteInstanceDef of concrete_instance

and concrete_type_alias =
  ConcreteTypeAlias of identifier * concrete_type_param list * universe * typespec * docstring

and concrete_record =
  ConcreteRecord of identifier * concrete_type_param list * universe * concrete_slot list * docstring

and concrete_union =
  ConcreteUnion of identifier * concrete_type_param list * universe * concrete_case list * docstring

and concrete_slot =
  ConcreteSlot of identifier * typespec

and concrete_case =
  ConcreteCase of identifier * concrete_slot list

and concrete_typeclass =
  ConcreteTypeClass of identifier * concrete_type_param * concrete_method_decl list * docstring

and concrete_instance =
  ConcreteInstance of identifier * concrete_type_param list * typespec * concrete_method_def list * docstring

and concrete_method_decl =
  ConcreteMethodDecl of identifier * concrete_param list * typespec * docstring

and concrete_method_def =
  ConcreteMethodDef of identifier * concrete_param list * typespec * cstmt * docstring

and typespec =
  TypeSpecifier of identifier * typespec list

and cexpr =
  | CNilConstant of span
  | CBoolConstant of span * bool
  | CIntConstant of span * string
  | CFloatConstant of span * string
  | CStringConstant of span * string
  | CVariable of span * identifier
  | CArith of span * arithmetic_operator * cexpr * cexpr
  | CFuncall of span * identifier * concrete_arglist
  | CComparison of span * comparison_operator * cexpr * cexpr
  | CConjunction of span * cexpr * cexpr
  | CDisjunction of span * cexpr * cexpr
  | CNegation of span * cexpr
  | CIfExpression of span * cexpr * cexpr * cexpr
  | CPath of span * cexpr * concrete_path_elem list
  | CEmbed of span * typespec * string * cexpr list
  | CDeref of span * cexpr

and cstmt =
  | CSkip of span
  | CLet of span * identifier * typespec * cexpr
  | CDestructure of span * (identifier * typespec) list * cexpr
  | CAssign of span * concrete_lvalue * cexpr
  | CIf of span * cexpr * cstmt * cstmt
  | CCase of span * cexpr * concrete_when list
  | CWhile of span * cexpr * cstmt
  | CFor of span * identifier * cexpr * cexpr * cstmt
  | CBorrow of {
      span: span;
      original: identifier;
      rename: identifier;
      region: identifier;
      body: cstmt;
      mode: borrowing_mode
    }
  | CBlock of span * cstmt list
  | CDiscarding of span * cexpr
  | CReturn of span * cexpr

and condition_branch =
  ConditionBranch of cexpr * cstmt

and concrete_when =
  ConcreteWhen of identifier * concrete_param list * cstmt

and concrete_arglist =
  | ConcretePositionalArgs of cexpr list
  | ConcreteNamedArgs of (identifier * cexpr) list

and concrete_param =
  ConcreteParam of identifier * typespec

and concrete_type_param =
  ConcreteTypeParam of identifier * universe

and concrete_path_elem =
  | CSlotAccessor of identifier
  | CPointerSlotAccessor of identifier
  | CArrayIndex of cexpr

and concrete_lvalue =
  ConcreteLValue of identifier * concrete_path_elem list

val make_module_body : module_name -> concrete_import_list list -> pragma list -> concrete_def list -> concrete_module_body

val decl_name : concrete_decl -> identifier option

val def_name : concrete_def -> identifier option

val get_concrete_decl : concrete_module_interface -> identifier -> concrete_decl option

val get_concrete_def : concrete_module_body -> identifier -> concrete_def option

val has_instance_decl : concrete_module_interface -> identifier -> concrete_type_param list -> typespec -> bool

val get_instance_def : concrete_module_body -> identifier -> concrete_type_param list -> typespec -> concrete_instance option

val make_pragma : identifier -> concrete_arglist -> pragma
