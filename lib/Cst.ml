(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
(** This module implements Austral's concrete syntax tree, the first
    intermediate representation in the compiler. *)
open Identifier
open Common
open Type
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
  | ConcreteConstantDecl of span * identifier * typespec * docstring
  | ConcreteOpaqueTypeDecl of span * identifier * concrete_type_param list * universe * docstring
  | ConcreteRecordDecl of concrete_record
  | ConcreteUnionDecl of concrete_union
  | ConcreteFunctionDecl of span * identifier * concrete_type_param list * concrete_param list * typespec * docstring
  | ConcreteTypeClassDecl of concrete_typeclass
  | ConcreteInstanceDecl of span * identifier * concrete_type_param list * typespec * docstring

and concrete_def =
  | ConcreteConstantDef of span * identifier * typespec * cexpr * docstring
  | ConcreteRecordDef of concrete_record
  | ConcreteUnionDef of concrete_union
  | ConcreteFunctionDef of span * identifier * concrete_type_param list * concrete_param list * typespec * cstmt * docstring * pragma list
  | ConcreteTypeClassDef of concrete_typeclass
  | ConcreteInstanceDef of concrete_instance

and concrete_record =
  ConcreteRecord of span * identifier * concrete_type_param list * universe * concrete_slot list * docstring

and concrete_union =
  ConcreteUnion of span * identifier * concrete_type_param list * universe * concrete_case list * docstring

and concrete_slot =
  ConcreteSlot of identifier * typespec

and concrete_case =
  ConcreteCase of identifier * concrete_slot list

and concrete_typeclass =
  ConcreteTypeClass of span * identifier * concrete_type_param * concrete_method_decl list * docstring

and concrete_instance =
  ConcreteInstance of span * identifier * concrete_type_param list * typespec * concrete_method_def list * docstring

and concrete_method_decl =
  ConcreteMethodDecl of identifier * concrete_type_param list * concrete_param list * typespec * docstring

and concrete_method_def =
  ConcreteMethodDef of identifier * concrete_type_param list * concrete_param list * typespec * cstmt * docstring

and typespec =
  | TypeSpecifier of identifier * typespec list
  | ConcreteReadRef of typespec * typespec
  | ConcreteWriteRef of typespec * typespec
  | ConcreteSpan of typespec * typespec
  | ConcreteSpanWrite of typespec * typespec

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
  | CPath of span * identifier * concrete_path_elem list
  | CRefPath of span * identifier* concrete_path_elem list
  | CEmbed of span * typespec * string * cexpr list
  | CDeref of span * cexpr
  | CTypecast of span * cexpr * typespec
  | CSizeOf of span * typespec
  | CBorrowExpr of span * borrowing_mode * identifier
  | CReborrow of span * identifier

and cstmt =
  | CSkip of span
  | CLet of span * mutability * identifier * typespec * cexpr
  | CDestructure of span * mutability * concrete_binding list * cexpr
  | CAssign of span * cexpr * cexpr
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
      mode: borrow_stmt_kind
    }
  | CBlock of span * cstmt list
  | CDiscarding of span * cexpr
  | CReturn of span * cexpr

and concrete_binding =
  ConcreteBinding of  {
      name: identifier;
      ty: typespec;
      rename: identifier;
    }

and condition_branch =
  ConditionBranch of cexpr * cstmt

and concrete_when =
  ConcreteWhen of identifier * concrete_binding list * cstmt

and concrete_arglist =
  | ConcretePositionalArgs of cexpr list
  | ConcreteNamedArgs of (identifier * cexpr) list

and concrete_param =
  ConcreteParam of identifier * typespec

and concrete_type_param =
  ConcreteTypeParam of identifier * universe * identifier list

and concrete_path_elem =
  | CSlotAccessor of identifier
  | CPointerSlotAccessor of identifier
  | CArrayIndex of cexpr
