open Identifier
open Common
open Type
open Error

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
  | CDestructure of (identifier * typespec) list * cexpr
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

let decl_name = function
  | ConcreteConstantDecl (n, _, _) -> Some n
  | ConcreteOpaqueTypeDecl (n, _, _, _) -> Some n
  | ConcreteTypeAliasDecl (ConcreteTypeAlias (n, _, _, _, _)) -> Some n
  | ConcreteRecordDecl (ConcreteRecord (n, _, _, _, _)) -> Some n
  | ConcreteUnionDecl (ConcreteUnion (n, _, _, _, _)) -> Some n
  | ConcreteFunctionDecl (n, _, _, _, _) -> Some n
  | ConcreteTypeClassDecl (ConcreteTypeClass (n, _, _, _)) -> Some n
  | ConcreteInstanceDecl _ -> None

let def_name = function
  | ConcreteConstantDef (n, _, _, _) -> Some n
  | ConcreteTypeAliasDef (ConcreteTypeAlias (n, _, _, _, _)) -> Some n
  | ConcreteRecordDef (ConcreteRecord (n, _, _, _, _)) -> Some n
  | ConcreteUnionDef (ConcreteUnion (n, _, _, _, _)) -> Some n
  | ConcreteFunctionDef (n, _, _, _, _, _, _) -> Some n
  | ConcreteTypeClassDef (ConcreteTypeClass (n, _, _, _)) -> Some n
  | ConcreteInstanceDef _ -> None

let get_concrete_decl (ConcreteModuleInterface (_, _, decls)) name =
  let pred decl =
    match decl_name decl with
    | (Some name') ->
       name = name'
    | None ->
       false
  in
  List.find_opt pred decls

let get_concrete_def (ConcreteModuleBody (_, _, defs)) name =
  let pred def =
    match def_name def with
    | (Some name') ->
       name = name'
    | None ->
       false
  in
  List.find_opt pred defs

let has_instance_decl (ConcreteModuleInterface (_, _, decls)) (name: identifier) (typarams: type_parameter list) (ty: typespec): bool =
  let pred = function
    | ConcreteInstanceDecl (name', typarams', ty', _) ->
       (name = name') && (typarams = typarams') && (ty = ty')
    | _ ->
       false
  in
  List.exists pred decls

let get_instance_def (ConcreteModuleBody (_, _, defs)) (name: identifier) (typarams: type_parameter list) (ty: typespec): concrete_instance option =
  let filter = function
    | ConcreteInstanceDef ci -> Some ci
    | _ -> None
  and pred (ConcreteInstance (name', typarams', ty', _, _)) =
    (name = name') && (typarams = typarams') && (ty = ty')
  in
  List.find_opt pred (List.filter_map filter defs)

let make_pragma name args =
  let s = ident_string name in
  if s = "Foreign_Import" then
    match args with
    | ConcreteNamedArgs [(a, CStringConstant f)] ->
       (* FIXME: For some reason if we move `make_ident "External_Name"` to `a`
          in the pattern above, a weird syntax error happens. *)
       if a = make_ident "External_Name" then
         ForeignImportPragma f
       else
         err "Invalid foreign import pragma"
    | _ ->
       err "Invalid foreign import pragma"
  else
    err ("Unknown pragma: " ^ s)
