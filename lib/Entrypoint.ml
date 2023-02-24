open Id
open Identifier
open Env
open Type
open TypeParameters
open Error
open ErrorText
open BuiltIn
open Names
open CodeGen
open MonoTypeBindings

module Errors = struct
  let multiple_parameters () =
    austral_raise EntrypointError [
      Text "The entrypoint function should take a single parameter with type ";
      Code "RootCapability";
      Text ", or none at all, but it has several."
    ]

  let no_such_function ~name ~module_name =
    austral_raise EntrypointError [
      Text "No function named ";
      Code (ident_string name);
      Text " in module ";
      Code (mod_name_string module_name)
    ]

  let not_function () =
    austral_raise EntrypointError [
      Text "The entrypoint must be a function."
    ]

  let not_monomorphic () =
    austral_raise EntrypointError [
      Text "The entrypoint function cannot be generic."
    ]

  let not_public () =
    austral_raise EntrypointError [
      Text "The entrypoint function is not public.";
      Break;
      Text "Consider declaring it in the module interface."
    ]

  let wrong_parameter_type ty =
    austral_raise EntrypointError [
      Text "The type of the entrypoint argument must be ";
      Code "RootCapability";
      Text " but it is ";
      Code (type_string ty)
    ]

  let wrong_return_type ty =
    austral_raise EntrypointError [
      Text "The type of the entrypoint function must be ";
      Code "ExitCode";
      Text " but it is ";
      Code (type_string ty)
    ]
end

(** The kind of entrypoint function we have. *)
type entrypoint_kind =
  | EmptyEntrypoint
  (** Zero-parameter entrypoint function. *)
  | RootCapEntrypoint
  (** Single-parameter entrypoint function with a root capability argument. *)

(** Check the entrypoint function is valid, return the decl_id of the function
    and the entrypoint kind. *)
let rec check_entrypoint_validity (env: env) (name: qident): decl_id * entrypoint_kind =
  match get_decl_by_name env (qident_to_sident name) with
  | Some decl ->
     (match decl with
      | Function { id; vis; typarams; value_params; rt; _ } ->
         if vis = VisPublic then
           if (typarams_size typarams) = 0 then
             match value_params with
             | [] ->
               (* Empty parameter list case *)
                if is_exit_code_type rt then
                  (id, EmptyEntrypoint)
                else
                  Errors.wrong_return_type rt
             | [ValueParameter (_, pt)] ->
                (* Single parameter case: the `root` parameter. *)
                if is_root_cap_type pt then
                  if is_exit_code_type rt then
                    (id, RootCapEntrypoint)
                  else
                    Errors.wrong_return_type rt
                else
                  Errors.wrong_parameter_type pt
             | _ ->
                Errors.multiple_parameters ()
           else
             Errors.not_monomorphic ()
         else
           Errors.not_public ()
      | _ ->
         Errors.not_function ())
  | None ->
      Errors.no_such_function
        ~name:(original_name name)
        ~module_name:(source_module_name name)

and is_root_cap_type = function
  | NamedType (name, [], LinearUniverse) ->
     let m = equal_module_name (source_module_name name) pervasive_module_name
     and n = equal_identifier (original_name name) (make_ident root_cap_name) in
     m && n
  | _ ->
     false

and is_exit_code_type = function
  | NamedType (name, [], FreeUniverse) ->
     let m = equal_module_name (source_module_name name) pervasive_module_name
     and n = equal_identifier (original_name name) (make_ident exit_code_name) in
     m && n
  | _ ->
     false

let entrypoint_code root_cap_mono_id exit_code_mono_id id =
  let f = gen_decl_id id in
  let exit_code: string = gen_mono_id exit_code_mono_id in
  ("int main(int argc, char** argv) {\n"
   ^ "    au_store_cli_args(argc, argv);\n"
   ^ "    " ^ exit_code ^ " result = " ^ f ^ "((" ^ (gen_mono_id root_cap_mono_id) ^ "){ .value = false });\n"
   ^ "    switch(result.tag) {\n"
   ^ "        case " ^ exit_code ^ "_tag_ExitSuccess:\n"
   ^ "            return 0;\n"
   ^ "        case " ^ exit_code ^ "_tag_ExitFailure:\n"
   ^ "            return 1;\n"
   ^ "    }\n"
   ^ "}")

let empty_entrypoint_code (entrypoint_id: decl_id) (exit_code_id: mono_id): string =
  let f = gen_decl_id entrypoint_id in
  let exit_code: string = gen_mono_id exit_code_id in
  ("int main(int argc, char** argv) {\n"
   ^ "    au_store_cli_args(argc, argv);\n"
   ^ "    " ^ exit_code ^ " result = " ^ f ^ "();\n"
   ^ "    switch(result.tag) {\n"
   ^ "        case " ^ exit_code ^ "_tag_ExitSuccess:\n"
   ^ "            return 0;\n"
   ^ "        case " ^ exit_code ^ "_tag_ExitFailure:\n"
   ^ "            return 1;\n"
   ^ "    }\n"
   ^ "}")

let get_root_capability_monomorph (env: env): mono_id =
  let mn: module_name = make_mod_name "Austral.Pervasive"
  and n: identifier = make_ident root_cap_name in
  let sn: sident = make_sident mn n in
  match get_decl_by_name env sn with
  | Some (Record { id; _ }) ->
     (match get_type_monomorph env id empty_mono_bindings with
      | Some id ->
         id
      | _ ->
         internal_err "No monomorph of RootCapability.")
  | _ ->
     internal_err "Can't find the RootCapability type in the environment."

let get_exit_code_monomorph (env: env): mono_id =
  let mn: module_name = make_mod_name "Austral.Pervasive"
  and n: identifier = make_ident exit_code_name in
  let sn: sident = make_sident mn n in
  match get_decl_by_name env sn with
  | Some (Union { id; _ }) ->
     (match get_type_monomorph env id empty_mono_bindings with
      | Some id ->
         id
      | _ ->
         internal_err "No monomorph of ExitCode.")
  | _ ->
     internal_err "Can't find the ExitCode type in the environment."

let entrypoint_code (env: env) (name: qident): string =
  let (entrypoint_id, kind): decl_id * entrypoint_kind = check_entrypoint_validity env name in
  match kind with
  | EmptyEntrypoint ->
     empty_entrypoint_code entrypoint_id (get_exit_code_monomorph env)
  | RootCapEntrypoint ->
     entrypoint_code (get_root_capability_monomorph env) (get_exit_code_monomorph env) entrypoint_id
