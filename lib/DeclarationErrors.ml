(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** The errors in the structure of declarations, and their wording. *)
open Identifier
open Type
open TypeParameter
open ErrorText

(** The kinds of declaration that the wording of an error names. *)
type declaration_kind =
  | ConstantDeclaration
  | TypeDeclaration
  | RecordDeclaration
  | UnionDeclaration
  | FunctionDeclaration
  | MethodDeclaration
  | InstanceDeclaration

let declaration_kind_word (kind: declaration_kind): string =
  match kind with
  | ConstantDeclaration -> "constant"
  | TypeDeclaration -> "type"
  | RecordDeclaration -> "record"
  | UnionDeclaration -> "union"
  | FunctionDeclaration -> "function"
  | MethodDeclaration -> "method"
  | InstanceDeclaration -> "instance"

(** How the declaration of a function in an interface differs from its definition in the body. *)
type function_difference =
  | DifferentTypeParameters
  | DifferentValueParameters
  | DifferentReturnTypes
  | OtherDifference

(** The pragmas the compiler knows. *)
type known_pragma =
  | ForeignImport
  | ForeignExport
  | UnsafeModule

type t =
  | ForeignInSafeModule
  | ForeignTypeParameters
  | FunInvalidPragmas
  | DeclarationKindMismatch of { name: identifier; expected: declaration_kind }
  | FunctionMismatch of { name: identifier; difference: function_difference }
  | MissingBodyDefinition of { name: identifier; declaration: declaration_kind option }
  | MissingInstanceBodyDefinition of { name: identifier; ty: Cst.typespec }
  | ModuleNameMismatch of { interface: module_name; body: module_name }
  | MultiargTypeclass of identifier
  | DeclarationTypeMismatch of identifier
  | DeclarationUniverseMismatch of identifier
  | PragmaArgumentError of known_pragma
  | UnknownPragma of identifier
  | DeclarationAlreadyExists of { name: identifier; module_name: module_name }
  | ModuleAlreadyExists of module_name
  | UnknownModule of module_name
  | ExportGeneric of identifier
  | ForeignImportExport of identifier
  | InstanceForNonTypeclass of identifier
  | TypeclassNoMethod of { typeclass: identifier; method_name: identifier }
  | TypeclassParamIsRegion of identifier
  | TypesAreNotRegions of { name: identifier; declaration: declaration_kind }
  | UnknownTypeclass of identifier
  | TyparamNotInSignature of type_parameter
  | DoubleImport of identifier
  | ImportMemoryInSafeModule
  | ImportPrivate of identifier
  | ImportUnresolved of { name: identifier; module_name: module_name }
  | MissingReturn of { name: identifier; declaration: declaration_kind }
  | InstanceDuplicateTyvars
  | InstanceLeftoverTypeParameters
  | InstanceArgumentLoneTyvar
  | InstanceArgumentNotATyvar of { ty: ty; universe: universe }
  | InstanceArgumentRegion
  | InstanceShadowsTypeclassParam of identifier

let render (error: t): err_text =
  match error with
  | ForeignInSafeModule ->
     [
       Text "Can't declare a foreign function in a safe module."
     ]
  | ForeignTypeParameters ->
     [
       Text "Foreign functions can't have type parameters."
     ]
  | FunInvalidPragmas ->
     [
       Text "Only import and export pragmas are allowed inside functions."
     ]
  | DeclarationKindMismatch { name; expected } ->
     [
       Text "The interface requires ";
       Code (ident_string name);
       Text " to be a ";
       Text (declaration_kind_word expected)
     ]
  | FunctionMismatch { name; difference } ->
     let message = match difference with
       | DifferentTypeParameters -> "have different type parameters."
       | DifferentValueParameters -> "have different parameters."
       | DifferentReturnTypes -> "have different return types"
       | OtherDifference -> "are different."
     in
     [
       Text "The interface declaration for ";
       Code (ident_string name);
       Text " and its implementation ";
       Text message
     ]
  | MissingBodyDefinition { name; declaration } ->
     (match declaration with
      | Some declaration ->
         [
           Text "The ";
           Text (declaration_kind_word declaration);
           Text " ";
           Code (ident_string name);
           Text " has no corresponding body implementation."
         ]
      | None ->
         [
           Code (ident_string name);
           Text " has no corresponding body implementation."
         ])
  | MissingInstanceBodyDefinition { name; ty } ->
     [
       Text "The instance ";
       Code (ident_string name);
       Text " with type ";
       Code (Cst.typespec_string ty);
       Text " has no corresponding body implementation."
     ]
  | ModuleNameMismatch { interface; body } ->
     [
       Text "Module interface and body have different names: ";
       Code (mod_name_string interface);
       Text " and ";
       Code (mod_name_string body)
     ]
  | MultiargTypeclass name ->
     [
       Text "The typeclass ";
       Code (ident_string name);
       Text " has multiple type parameters, which is unsupported."
     ]
  | DeclarationTypeMismatch name ->
     [
       Text "The interface declaration for ";
       Code (ident_string name);
       Text " and its implementation have different types."
     ]
  | DeclarationUniverseMismatch name ->
     [
       Text "The interface declaration for ";
       Code (ident_string name);
       Text " and its implementation have different universes."
     ]
  | PragmaArgumentError pragma ->
     let (pragma_name, argument) = match pragma with
       | ForeignImport -> ("Foreign_Import", Some "External_Name")
       | ForeignExport -> ("Foreign_Export", Some "External_Name")
       | UnsafeModule -> ("Unsafe_Module", None)
     in
     Text ("Invalid " ^ pragma_name ^ " pragma.") :: (match argument with
      | Some argument -> [Break; Text "This pragma takes exactly one argument, "; Code argument]
      | None -> [Break; Text "This pragma takes no arguments."])
  | UnknownPragma name ->
     [
       Text "Unknown pragma ";
       Code (ident_string name)
     ]
  | DeclarationAlreadyExists { name; module_name } ->
     [
       Text "A declaration with the name ";
       Code (ident_string name);
       Text " already exists in the module ";
       Code (mod_name_string module_name);
       Text "."
     ]
  | ModuleAlreadyExists name ->
     [
       Text "A module with the name ";
       Code (mod_name_string name);
       Text " already exists."
     ]
  | UnknownModule name ->
     [
       Text "Unknown module ";
       Code (mod_name_string name);
       Text "."
     ]
  | ExportGeneric name ->
     [
       Text "The function ";
       Code (ident_string name);
       Text " cannot be exported because it is generic."
     ]
  | ForeignImportExport name ->
     [
       Text "The function ";
       Code (ident_string name);
       Text " cannot have both the ";
       Code "Foreign_Import";
       Text " and ";
       Code "Foreign_Export";
       Text " pragmas simultaneously."
     ]
  | InstanceForNonTypeclass name ->
     [
       Text "Cannot create an instance for ";
       Code (ident_string name);
       Text " because it is not a typeclass."
     ]
  | TypeclassNoMethod { typeclass; method_name } ->
     [
       Text "The typeclass ";
       Code (ident_string typeclass);
       Text " has no method named ";
       Code (ident_string method_name)
     ]
  | TypeclassParamIsRegion name ->
     [
       Text "The parameter for the typeclass ";
       Code (ident_string name);
       Text " cannot be declared to be in the ";
       Code "Region";
       Text " universe, because regions are distinct from types.";
       Break;
       Text "Consider using one of ";
       Code "Type";
       Text ", ";
       Code "Linear";
       Text ", or ";
       Code "Free";
       Text " instead.";
     ]
  | TypesAreNotRegions { name; declaration } ->
     [
       Text "The ";
       Text (declaration_kind_word declaration);
       Text " ";
       Code (ident_string name);
       Text " was declared to belong to the ";
       Code "Region";
       Text " universe, but regions are distinct from types.";
       Break;
       Text "Consider using one of ";
       Code "Type";
       Text ", ";
       Code "Linear";
       Text ", or ";
       Code "Free";
       Text " instead.";
     ]
  | UnknownTypeclass name ->
     [
       Text "Cannot find typeclass with the name ";
       Code (ident_string name)
     ]
  | TyparamNotInSignature tp ->
     [
       Text "The type parameter ";
       Code (ident_string (typaram_name tp));
       Text " does not appear anywhere in the signature of the function."
     ]
  | DoubleImport name ->
     [
       Text "Colliding imports: a declaration with name ";
       Code (ident_string name);
       Text " has already been imported.";
       Break;
       Text "Consider giving it a different nickname using ";
       Code "as";
       Text "."
     ]
  | ImportMemoryInSafeModule ->
     [
       Text "Cannot import from the ";
       Code "Austral.Memory";
       Text " module in a safe module.";
       Break;
       Text "Consider using a safe wrapper or adding ";
       Code "pragma Unsafe_Module;"
     ]
  | ImportPrivate name ->
     [
       Code (ident_string name);
       Text " is private and cannot be imported."
     ]
  | ImportUnresolved { name; module_name } ->
     [
       Text "No declaration with the name ";
       Code (ident_string name);
       Text " in the module ";
       Code (mod_name_string module_name)
     ]
  | MissingReturn { name; declaration } ->
     [
       Text "The ";
       Text (declaration_kind_word declaration);
       Text " ";
       Code (ident_string name);
       Text " does not end in a return statement."
     ]
  | InstanceDuplicateTyvars ->
     [
       Text "Generic instances can only use each type parameter once in the argument."
     ]
  | InstanceLeftoverTypeParameters ->
     [
       Text "The number of type parameters in the instance declaration must be the same as the number of type variables applied to the argument."
     ]
  | InstanceArgumentLoneTyvar ->
     [
       Text "Typeclass arguments cannot be lone type parameters."
     ]
  | InstanceArgumentNotATyvar { ty; universe } ->
     [
       Text "The type ";
       Type ty;
       Text " is not a type parameter, but generic instances must use either all type parameters or none of them in the argument.";
       Break;
       Text "Consider adding another generic parameter with universe ";
       Code (universe_string universe)
     ]
  | InstanceArgumentRegion ->
     [
       Text "Typeclass arguments cannot be regions."
     ]
  | InstanceShadowsTypeclassParam name ->
     [
       Text "The type parameters in a generic instance declaration cannot have the same name as the type parameter in the corresponding typeclass. The colliding name is ";
       Code (ident_string name);
       Text ".";
       Break;
       Text "This is, regrettably, a load-bearing hack for https://github.com/austral/austral/issues/244. Fixing this properly would require rewriting large parts of the frontend."
     ]
