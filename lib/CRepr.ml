(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Common
open Escape

(** This module defines types for representing C code. *)

type c_function_linkage =
  | LinkageInternal
  | LinkageExternal

(** The types that C or the prelude defines. *)
type c_builtin_ty =
  | CUnitType
  | CBoolType
  | CNat8Type
  | CNat16Type
  | CNat32Type
  | CNat64Type
  | CInt8Type
  | CInt16Type
  | CInt32Type
  | CInt64Type
  | CSizeType
  | CIndexType
  | CFloatType
  | CDoubleType
  | CRegionType
  | CSpanType
  | CFnPtrType

type c_ty =
  | CBuiltinType of c_builtin_ty
  (* A type the generated C defines: a monomorph or the tag enum of a union. *)
  | CNamedType of CIdent.t
  | CPointer of c_ty
  | CStructType of c_slot list
  | CUnionType of c_slot list

and c_slot = CSlot of CIdent.t * c_ty
and c_param = CValueParam of CIdent.t * c_ty

type c_expr =
  | CBool of bool
  | CInt of string
  | CFloat of string
  | CString of escaped_string
  | CVar of CIdent.t
  | CFuncall of CIdent.t * c_expr list
  | CFptrCall of c_expr * c_ty * c_ty list * c_expr list
  | CCast of c_expr * c_ty
  | CComparison of comparison_operator * c_expr * c_expr
  | CConjunction of c_expr * c_expr
  | CDisjunction of c_expr * c_expr
  | CNegation of c_expr
  | CIfExpression of c_expr * c_expr * c_expr
  | CStructInitializer of (CIdent.t * c_expr) list
  | CStructAccessor of c_expr * CIdent.t
  | CPointerStructAccessor of c_expr * CIdent.t
  | CAddressOf of c_expr
  | CEmbed of c_ty * string * c_expr list
  | CDeref of c_expr
  | CSizeOf of c_ty

type c_stmt =
  | CLet of CIdent.t * c_ty * c_expr option
  | CAssign of c_expr * c_expr
  | CDiscarding of c_expr
  | CIf of c_expr * c_stmt * c_stmt
  | CSwitch of c_expr * c_switch_case list
  | CWhile of c_expr * c_stmt
  | CFor of CIdent.t * c_expr * c_stmt
  | CReturn of c_expr
  | CBlock of c_stmt list
  | CExplicitBlock of c_stmt list
  | CLocalFunctionDeclaration of CIdent.t * c_param list * c_ty * c_function_linkage

(** Represents a case of a C switch statement *)
and c_switch_case =
  CSwitchCase of c_expr * c_stmt

type desc = Desc of string

type c_decl =
  | CMacro of desc * CIdent.t * c_expr
  | CStructForwardDeclaration of desc * CIdent.t
  | CNamedStructDefinition of desc * CIdent.t * c_slot list
  | CEnumDefinition of desc * CIdent.t * CIdent.t list
  | CFunctionDeclaration of desc * CIdent.t * c_param list * c_ty * c_function_linkage
  | CFunctionDefinition of desc * CIdent.t * c_param list * c_ty * c_stmt

type c_unit = CUnit of string * c_decl list

(** A whole C program, in the order it is written out. The prelude and the
    entrypoint are C source text. *)
type c_program = CProgram of {
      prelude: string;
      units: c_unit list;
      entrypoint: string option;
    }
