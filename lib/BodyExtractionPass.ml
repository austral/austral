(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Env
open Tast

let rec extract_bodies (env: env) (m: typed_module): env =
  let (TypedModule (_, decls)) = m in
  Util.iter_with_context extract_decl env decls

and extract_decl (env: env) (decl: typed_decl): env =
  match decl with
  | TFunction (decl_id, _, _, _, _, _, body, _) ->
      store_function_body env decl_id body
  | TInstance (_, _, _, _, _, methods, _) ->
     Util.iter_with_context extract_method env methods
  | _ ->
     env

and extract_method (env: env) (meth: typed_method_def): env =
  let (TypedMethodDef (ins_meth_id, _, _, _, body)) = meth in
  store_method_body env ins_meth_id body
