open Identifier
open IdentifierMap
open Semantic

type menv = ModuleEnv of semantic_module ModuleNameMap.t

let empty_menv =
  ModuleEnv ModuleNameMap.empty

let get_module (ModuleEnv menv) module_name =
  ModuleNameMap.find_opt module_name menv

let get_decl menv name =
  match get_module menv (source_module_name name) with
  | (Some sm) ->
     get_declaration (sm, (original_name name))
  | None ->
     None

let get_callable menv importing_module_name name =
  match get_module menv (source_module_name name) with
  | (Some sm) ->
     get_module_callable (sm, importing_module_name, original_name name)
  | None ->
     None
