open Identifier
open Common
open Cst
open Error

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
  | ConcreteRecordDecl (ConcreteRecord (n, _, _, _, _)) -> Some n
  | ConcreteUnionDecl (ConcreteUnion (n, _, _, _, _)) -> Some n
  | ConcreteFunctionDecl (n, _, _, _, _) -> Some n
  | ConcreteTypeClassDecl (ConcreteTypeClass (n, _, _, _)) -> Some n
  | ConcreteInstanceDecl _ -> None

let def_name = function
  | ConcreteConstantDef (n, _, _, _) -> Some n
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
