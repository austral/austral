open Common

type cpp_function_linkage =
  | LinkageInternal
  | LinkageExternal

type cpp_ty =
  | CNamedType of string * cpp_ty list
  | CPointer of cpp_ty
  | CStructType of cpp_struct
  | CUnionType of cpp_slot list
and cpp_slot = CSlot of string * cpp_ty
and cpp_struct = CStruct of string option * cpp_slot list

type cpp_expr =
  | CBool of bool
  | CInt of string
  | CFloat of string
  | CString of string
  | CVar of string
  | CFuncall of string * cpp_expr list
  | CArithmetic of arithmetic_operator * cpp_expr * cpp_expr
  | CComparison of comparison_operator * cpp_expr * cpp_expr
  | CConjunction of cpp_expr * cpp_expr
  | CDisjunction of cpp_expr * cpp_expr
  | CNegation of cpp_expr
  | CIfExpression of cpp_expr * cpp_expr * cpp_expr
  | CStructInitializer of (string * cpp_expr) list
  | CStructAccessor of cpp_expr * string

type cpp_stmt =
  | CLet of string * cpp_ty * cpp_expr
  | CAssign of string * cpp_expr
  | CDiscarding of cpp_expr
  | CIf of cpp_expr * cpp_stmt * cpp_stmt
  | CSwitch of cpp_expr * cpp_switch_case list
  | CWhile of cpp_expr * cpp_stmt
  | CFor of string * cpp_expr * cpp_expr * cpp_stmt
  | CReturn of cpp_expr
  | CBlock of cpp_stmt list
  | CExplicitBlock of cpp_stmt list

(* Represents a case of a C++ switch statement *)
and cpp_switch_case =
  CSwitchCase of cpp_expr * cpp_stmt

type cpp_decl =
  | CNamespace of string * cpp_decl list
  | CUsingDeclaration of { namespace: string; symbol: string }
  | CConstantDefinition of string * cpp_ty * cpp_expr
  | CTypeDeclaration of string * cpp_ty_param list
  | CStructForwardDeclaration of string * cpp_ty_param list
  | CTypeDefinition of string * cpp_ty_param list * cpp_ty
  | CStructDefinition of cpp_ty_param list * cpp_struct
  | CEnumDefinition of string * string list
  | CFunctionDeclaration of string * cpp_ty_param list * cpp_value_param list * cpp_ty * cpp_function_linkage
  | CFunctionDefinition of string * cpp_ty_param list * cpp_value_param list * cpp_ty * cpp_stmt
  | CDeclBlock of cpp_decl list

and cpp_ty_param = CTypeParam of string
and cpp_value_param = CValueParam of string * cpp_ty
