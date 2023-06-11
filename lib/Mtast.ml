(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)

open Id
open Identifier
open Common
open Escape
open MonoType
open Error

type mono_binding =
  MonoBinding of {
      name: identifier;
      ty: mono_ty;
      rename: identifier;
    }

type mono_module = MonoModule of module_name * mdecl list

and mdecl =
  | MConstant of decl_id * identifier * mono_ty * mexpr
  | MRecord of decl_id * identifier * mono_slot list
  | MRecordMonomorph of mono_id * mono_slot list
  | MUnion of decl_id * identifier * mono_case list
  | MUnionMonomorph of mono_id * mono_case list
  | MFunction of decl_id * identifier * mvalue_parameter list * mono_ty * mstmt
  | MFunctionMonomorph of mono_id * mvalue_parameter list * mono_ty * mstmt
  | MForeignFunction of decl_id * identifier * mvalue_parameter list * mono_ty * string
  | MConcreteInstance of decl_id * qident * mono_ty  * concrete_method list
  | MMethodMonomorph of mono_id * mvalue_parameter list * mono_ty * mstmt

and concrete_method =
  MConcreteMethod of ins_meth_id * identifier * mvalue_parameter list * mono_ty * mstmt

and mstmt =
  | MSkip
  | MLet of identifier * mono_ty * mstmt
  | MDestructure of mono_binding list * mexpr * mstmt
  | MAssign of mexpr * mexpr
  | MAssignVar of qident * mexpr
  | MInitialAssign of qident * mexpr
  | MIf of mexpr * mstmt * mstmt
  | MCase of mexpr * mtyped_when list * Tast.case_ref
  | MWhile of mexpr * mstmt
  | MFor of identifier * mexpr * mexpr * mstmt
  | MBorrow of {
      original: identifier;
      rename: identifier;
      region: identifier;
      orig_type: mono_ty;
      ref_type: mono_ty;
      body: mstmt;
      mode: borrow_stmt_kind
    }
  | MBlock of mstmt * mstmt
  | MDiscarding of mexpr
  | MReturn of mexpr

and mexpr =
  | MNilConstant
  | MBoolConstant of bool
  | MIntConstant of string
  | MFloatConstant of string
  | MStringConstant of escaped_string
  | MConstVar of qident * mono_ty
  | MParamVar of identifier * mono_ty
  | MLocalVar of identifier * mono_ty
  | MGenericFunVar of mono_id * mono_ty
  | MConcreteFunVar of decl_id * mono_ty
  | MConcreteFuncall of decl_id * qident * mexpr list * mono_ty
  (** Represents a call to a concrete function. *)
  | MGenericFuncall of mono_id * mexpr list * mono_ty
  (** Represents a call to a generic function. *)
  | MConcreteMethodCall of ins_meth_id * qident * mexpr list * mono_ty
  (** Represents a call to an instance method of a concrete instance. *)
  | MGenericMethodCall of ins_meth_id * mono_id * mexpr list * mono_ty
  (** Represents a call to an instance method of a generic instance. *)
  | MFptrCall of identifier * mexpr list * mono_ty
  | MCast of mexpr * mono_ty
  | MComparison of comparison_operator * mexpr * mexpr
  | MConjunction of mexpr * mexpr
  | MDisjunction of mexpr * mexpr
  | MNegation of mexpr
  | MIfExpression of mexpr * mexpr * mexpr
  | MRecordConstructor of mono_ty * (identifier * mexpr) list
  | MUnionConstructor of mono_ty * identifier * (identifier * mexpr) list
  | MEmbed of mono_ty * string * mexpr list
  | MDeref of mexpr
  | MTypecast of mexpr * mono_ty
  | MSizeOf of mono_ty
  | MSlotAccessor of mexpr * identifier * mono_ty
  | MPointerSlotAccessor of mexpr * identifier * mono_ty
  | MArrayIndex of mexpr * mexpr * mono_ty

and mtyped_when =
  MTypedWhen of identifier * mono_binding list * mstmt

and mvalue_parameter =
  MValueParameter of identifier * mono_ty

type mono_method =
  MonoMethod of {
      method_id: ins_meth_id;
      params: mvalue_parameter list;
      rt: mono_ty;
      body: mstmt;
    }

let rec get_type (e: mexpr): mono_ty =
  match e with
  | MNilConstant ->
     MonoUnit
  | MBoolConstant _ ->
     MonoBoolean
  | MIntConstant _ ->
     MonoInteger (Signed, Width32)
  | MFloatConstant _ ->
     MonoDoubleFloat
  | MStringConstant _ ->
     err "Type of the string constant"
  | MConstVar (_, ty) ->
     ty
  | MParamVar (_, ty) ->
     ty
  | MLocalVar (_, ty) ->
     ty
  | MGenericFunVar (_, ty) ->
     ty
  | MConcreteFunVar (_, ty) ->
     ty
  | MConcreteFuncall (_, _, _, ty) ->
     ty
  | MGenericFuncall (_, _, ty) ->
     ty
  | MConcreteMethodCall (_, _, _, ty) ->
     ty
  | MGenericMethodCall (_, _, _, ty) ->
     ty
  | MFptrCall (_, _, rt) ->
     rt
  | MCast (_, ty) ->
     ty
  | MComparison _ ->
     MonoBoolean
  | MConjunction _ ->
     MonoBoolean
  | MDisjunction _ ->
     MonoBoolean
  | MNegation _ ->
     MonoBoolean
  | MIfExpression (_, t, _) ->
     get_type t
  | MRecordConstructor (ty, _) ->
     ty
  | MUnionConstructor (ty, _, _) ->
     ty
  | MEmbed (ty, _, _) ->
     ty
  | MDeref e ->
     (match get_type e with
      | MonoReadRef (t, _) ->
         t
      | MonoWriteRef (t, _) ->
         t
      | _ ->
         internal_err ("a dereference expression was constructed whose argument is not a reference type."))
  | MTypecast (_, ty) ->
     ty
  | MSizeOf _ ->
     MonoInteger (Unsigned, Width64)
  | MSlotAccessor (_, _, ty) -> ty
  | MPointerSlotAccessor (_, _, ty) -> ty
  | MArrayIndex (_, _, ty) -> ty
