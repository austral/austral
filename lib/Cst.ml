open Identifier
open Common
open Type
open Error
open Span

type concrete_module_interface =
  ConcreteModuleInterface of module_name * docstring * concrete_import_list list * concrete_decl list

and concrete_module_body =
  ConcreteModuleBody of module_name * module_kind * docstring * concrete_import_list list * concrete_def list

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
  | TypeSpecifier of identifier * typespec list
  | ConcreteReadRef of typespec * typespec
  | ConcreteWriteRef of typespec * typespec

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
  | CTypecast of span * cexpr * typespec
  | CSizeOf of span * typespec

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

let make_module_body (name: module_name) (imports: concrete_import_list list) (pragmas: pragma list) (defs: concrete_def list) (docstring: docstring) =
  let is_unsafe_module p =
    match p with
    | UnsafeModulePragma -> true
    | _ -> false
  in
  let kind_from_pragmas pragmas =
    if (List.exists is_unsafe_module pragmas) then
      UnsafeModule
    else
      SafeModule
  in
  let kind = kind_from_pragmas pragmas
  in
  ConcreteModuleBody (name, kind, docstring, imports, defs)

let concrete_decl_name = function
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

let get_concrete_decl (ConcreteModuleInterface (_, _, _, decls)) name =
  let pred decl =
    match concrete_decl_name decl with
    | (Some name') ->
       name = name'
    | None ->
       false
  in
  List.find_opt pred decls

let get_concrete_def (ConcreteModuleBody (_, _, _, _, defs)) name =
  let pred def =
    match def_name def with
    | (Some name') ->
       name = name'
    | None ->
       false
  in
  List.find_opt pred defs

let has_instance_decl (ConcreteModuleInterface (_, _, _, decls)) (name: identifier) (typarams: concrete_type_param list) (ty: typespec): bool =
  let pred = function
    | ConcreteInstanceDecl (name', typarams', ty', _) ->
       (name = name') && (typarams = typarams') && (ty = ty')
    | _ ->
       false
  in
  List.exists pred decls

let get_instance_def (ConcreteModuleBody (_, _, _, _, defs)) (name: identifier) (typarams: concrete_type_param list) (ty: typespec): concrete_instance option =
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
    | ConcreteNamedArgs [(a, CStringConstant (_, f))] ->
       (* FIXME: For some reason if we move `make_ident "External_Name"` to `a`
          in the pattern above, a weird syntax error happens. *)
       if a = make_ident "External_Name" then
         ForeignImportPragma f
       else
         err "Invalid foreign import pragma"
    | _ ->
       err "Invalid foreign import pragma"
  else
    if s = "Unsafe_Module" then
      match args with
      | ConcretePositionalArgs [] ->
         UnsafeModulePragma
      | _ ->
         err "Unsafe_Module pragma takes no arguments."
    else
      err ("Unknown pragma: " ^ s)
