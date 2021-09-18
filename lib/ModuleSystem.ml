open Identifier
open IdentifierMap
open Semantic
open LexEnv
open Type
open Error

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

let put_module m sem =
  let (SemanticModule { name; _ }) = sem
  and (ModuleEnv menv) = m in
  match get_module m name with
  | None -> ModuleEnv (ModuleNameMap.add name sem menv)
  | (Some _) -> err "Module with this name already exists"

let menv_get_var (menv: menv) (lexenv: lexenv) (name: qident): ty option =
  match get_var lexenv (original_name name) with
  | Some ty ->
     Some ty
  | None ->
     (match get_decl menv name with
      | Some decl ->
         (match decl with
          | SConstantDefinition (_, _, ty) ->
             Some ty
          | _ ->
             None)
      | None ->
         None)
