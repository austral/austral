open Common
open Escape

(** This module defines types for representing C code. *)

type c_function_linkage =
  | LinkageInternal
  | LinkageExternal

type c_ty =
  | CNamedType of string
  | CPointer of c_ty
  | CStructType of c_struct
  | CUnionType of c_slot list
[@@deriving show]

and c_slot = CSlot of string * c_ty
and c_struct = CStruct of string option * c_slot list

type c_expr =
  | CBool of bool
  | CInt of string
  | CFloat of string
  | CString of escaped_string
  | CVar of string
  | CFuncall of string * c_expr list
  | CCast of c_expr * c_ty
  | CArithmetic of arithmetic_operator * c_expr * c_expr
  | CComparison of comparison_operator * c_expr * c_expr
  | CConjunction of c_expr * c_expr
  | CDisjunction of c_expr * c_expr
  | CNegation of c_expr
  | CIfExpression of c_expr * c_expr * c_expr
  | CStructInitializer of (string * c_expr) list
  | CStructAccessor of c_expr * string
  | CPointerStructAccessor of c_expr * string
  | CIndex of c_expr * c_expr
  | CAddressOf of c_expr
  | CEmbed of c_ty * string * c_expr list
  | CDeref of c_expr
  | CSizeOf of c_ty

type c_stmt =
  | CLet of string * c_ty * c_expr
  | CAssign of c_expr * c_expr
  | CDiscarding of c_expr
  | CIf of c_expr * c_stmt * c_stmt
  | CSwitch of c_expr * c_switch_case list
  | CWhile of c_expr * c_stmt
  | CFor of string * c_expr * c_expr * c_stmt
  | CReturn of c_expr
  | CBlock of c_stmt list
  | CExplicitBlock of c_stmt list

(** Represents a case of a C switch statement *)
and c_switch_case =
  CSwitchCase of c_expr * c_stmt

type c_decl =
  | CConstantDefinition of string * c_ty * c_expr
  | CStructForwardDeclaration of string
  | CTypeDefinition of string * c_ty
  | CStructDefinition of c_struct
  | CEnumDefinition of string * string list
  | CFunctionDeclaration of string * c_param list * c_ty * c_function_linkage
  | CFunctionDefinition of string * c_param list * c_ty * c_stmt

and c_param = CValueParam of string * c_ty
