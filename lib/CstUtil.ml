(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Identifier
open Common
open Cst
open Error
open ErrorText

module Errors = struct
  let pragma_argument_error ~pragma_name ~argument =
    let text = Text ("Invalid " ^ pragma_name ^ " pragma.") :: match argument with
    | Some argument -> [Break; Text "This pragma takes exactly one argument, "; Code argument]
    | None -> [Break; Text "This pragma takes no arguments."]
    in
    austral_raise DeclarationError text

  let unknown_pragma name =
    austral_raise DeclarationError [
      Text "Unknown pragma ";
      Code name
    ]
end

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
  | ConcreteConstantDecl (_, n, _, _) -> Some n
  | ConcreteOpaqueTypeDecl (_, n, _, _, _) -> Some n
  | ConcreteRecordDecl (ConcreteRecord (_, n, _, _, _, _)) -> Some n
  | ConcreteUnionDecl (ConcreteUnion (_, n, _, _, _, _)) -> Some n
  | ConcreteFunctionDecl (_, n, _, _, _, _) -> Some n
  | ConcreteTypeClassDecl (ConcreteTypeClass (_, n, _, _, _)) -> Some n
  | ConcreteInstanceDecl _ -> None

let def_name = function
  | ConcreteConstantDef (_, n, _, _, _) -> Some n
  | ConcreteRecordDef (ConcreteRecord (_, n, _, _, _, _)) -> Some n
  | ConcreteUnionDef (ConcreteUnion (_, n, _, _, _, _)) -> Some n
  | ConcreteFunctionDef (_, n, _, _, _, _, _, _) -> Some n
  | ConcreteTypeClassDef (ConcreteTypeClass (_, n, _, _, _)) -> Some n
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
    | ConcreteInstanceDecl (_, name', typarams', ty', _) ->
       (name = name') && (typarams = typarams') && (ty = ty')
    | _ ->
       false
  in
  List.exists pred decls

let get_instance_def (ConcreteModuleBody (_, _, _, _, defs)) (name: identifier) (typarams: concrete_type_param list) (ty: typespec): concrete_instance option =
  let filter = function
    | ConcreteInstanceDef ci -> Some ci
    | _ -> None
  and pred (ConcreteInstance (_, name', typarams', ty', _, _)) =
    (name = name') && (typarams = typarams') && (ty = ty')
  in
  List.find_opt pred (List.filter_map filter defs)

let make_pragma name args =
  let s = ident_string name in
  if s = "Foreign_Import" then
    let raise_err () =
      Errors.pragma_argument_error
        ~pragma_name:"Foreign_Import"
        ~argument:(Some "External_Name")
    in
    match args with
    | ConcreteNamedArgs [(a, CStringConstant (_, f))] ->
       if equal_identifier a (make_ident "External_Name") then
         ForeignImportPragma f
       else
         raise_err ()
    | _ ->
       raise_err ()
  else
    if s = "Foreign_Export" then
      let raise_err () =
        Errors.pragma_argument_error
          ~pragma_name:"Foreign_Export"
          ~argument:(Some "External_Name")
      in
      match args with
      | ConcreteNamedArgs [(a, CStringConstant (_, f))] ->
         if equal_identifier a (make_ident "External_Name") then
           ForeignExportPragma f
         else
           raise_err ()
      | _ ->
         raise_err ()
    else
      if s = "Unsafe_Module" then
        let raise_err () =
          Errors.pragma_argument_error
            ~pragma_name:"Unsafe_Module"
            ~argument:None
        in
        match args with
        | ConcretePositionalArgs [] ->
           UnsafeModulePragma
        | _ ->
           raise_err ()
      else
        Errors.unknown_pragma s

let mod_int_name (inter: concrete_module_interface): module_name =
  let (ConcreteModuleInterface (name, _, _, _)) = inter in
  name

let mod_body_name (body: concrete_module_body): module_name =
  let (ConcreteModuleBody (name, _, _, _, _)) = body in
  name

let rec typespec_string (ts: typespec): string =
  match ts with
  | TypeSpecifier (name, args) ->
     (match args with
      | [] -> (ident_string name)
      | args -> (ident_string name) ^ "[" ^ (String.concat ", " (List.map typespec_string args)) ^ "]")
  | ConcreteReadRef (ty, reg) ->
     "&[" ^ (typespec_string ty) ^ ", " ^ (typespec_string reg) ^ "]"
  | ConcreteWriteRef (ty, reg) ->
     "&![" ^ (typespec_string ty) ^ ", " ^ (typespec_string reg) ^ "]"
  | ConcreteSpan (ty, reg) ->
     "Span[" ^ (typespec_string ty) ^ ", " ^ (typespec_string reg) ^ "]"
  | ConcreteSpanWrite (ty, reg) ->
     "Span![" ^ (typespec_string ty) ^ ", " ^ (typespec_string reg) ^ "]"
