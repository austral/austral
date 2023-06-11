(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Id
open Env
open Type
open MonoTypeBindings
open Error
open CodeGen
open CRepr
open Identifier

module Errors = struct
  let disallowed_type ty =
    austral_raise TypeError [
      Text "The type ";
      Type ty;
      Text " is not allowed in the signature of an exported function."
    ]
end

(** Get the monomorph for an exported function. *)
let get_export_monomorph (env: env) (id: decl_id): mono_id =
  match get_decl_by_id env id with
  | Some (Function { id; name; _ }) ->
     (match get_function_monomorph env id empty_mono_bindings with
      | Some id ->
         id
      | _ ->
         internal_err ("No monomorph of exported function `" ^ (ident_string name) ^ "`"))
  | _ ->
     internal_err "Can't find the exported fuction in the environment."

let get_export_data (env: env) (id: decl_id): (value_parameter list * ty) =
  match get_decl_by_id env id with
  | Some (Function { value_params; rt; _ }) ->
     (value_params, rt)
  | _ ->
     internal_err "Can't find the exported fuction in the environment."

let rec transform_ty (ty: ty): c_ty =
  match ty with
  | Unit ->
     Errors.disallowed_type ty
  | Boolean ->
     CNamedType "au_bool_t"
  | Integer (s, w) ->
     let sgn: string =
       match s with
       | Unsigned -> "au_nat"
       | Signed -> "au_int"
     in
     let name: string =
       match w with
       | Width8 -> sgn ^ "8_t"
       | Width16 -> sgn ^ "16_t"
       | Width32 -> sgn ^ "32_t"
       | Width64 -> sgn ^ "64_t"
       | WidthByteSize -> "size_t"
       | WidthIndex -> "au_index_t"
     in
     CNamedType name
  | SingleFloat ->
     CNamedType "float"
  | DoubleFloat ->
     CNamedType "double"
  | NamedType _ ->
     Errors.disallowed_type ty
  | StaticArray (Integer (Unsigned, Width8)) ->
     c_string_type
  | StaticArray _ ->
     Errors.disallowed_type ty
  | RegionTy _ ->
     Errors.disallowed_type ty
  | ReadRef _ ->
     Errors.disallowed_type ty
  | WriteRef _ ->
     Errors.disallowed_type ty
  | Span _ ->
     Errors.disallowed_type ty
  | SpanMut _ ->
     Errors.disallowed_type ty
  | TyVar _ ->
     Errors.disallowed_type ty
  | Address t ->
     CPointer (transform_ty t)
  | Pointer _ ->
     Errors.disallowed_type ty
  | FnPtr _ ->
     fn_type
  | MonoTy _ ->
     Errors.disallowed_type ty

let transform_param (ValueParameter (name, ty)): c_param =
  CValueParam (gen_ident name, transform_ty ty)

let make_wrapper (env: env) (decl_id: decl_id) (export_name: string): c_decl =
  let mono_id: mono_id = get_export_monomorph env decl_id
  and (params, rt): value_parameter list * ty = get_export_data env decl_id in
  CFunctionDefinition (
      Desc "Wrapper function",
      export_name,
      List.map transform_param params,
      transform_ty rt,
      CBlock [
          CReturn (CFuncall (gen_mono_id mono_id, List.map (fun (ValueParameter (n, _)) -> CVar (gen_ident n)) params))
      ]
    )

let all_wrappers (env: env): c_decl list =
  List.map (fun (id, name) -> make_wrapper env id name) (get_export_functions env)
