open Names
open Identifier
open Id
open Env
open BuiltIn
open BuiltInModules
open ParserInterface
open CombiningPass
open ExtractionPass
open TypingPass
open BodyExtractionPass
open CodeGen
open CRenderer
open Cst
open Tast
open Type
open TypeParameters
open Error
open Util
open Combined
open Linked
open Mtast
open Monomorphize
open ReturnCheck
open LinearityCheck
open Reporter
open Filename

let append_import_to_interface (ci: concrete_module_interface) (import: concrete_import_list): concrete_module_interface =
  let (ConcreteModuleInterface (mn, docstring, imports, decls)) = ci in
  if equal_module_name mn pervasive_module_name then
    ci
  else
    ConcreteModuleInterface (mn, docstring, import :: imports, decls)

let append_import_to_body (cb: concrete_module_body) (import: concrete_import_list): concrete_module_body =
  let (ConcreteModuleBody (mn, kind, docstring, imports, decls)) = cb in
  if equal_module_name mn pervasive_module_name then
    cb
  else
    ConcreteModuleBody (mn, kind, docstring, import :: imports, decls)

type compiler = Compiler of env * string

(** Extract the env from the compiler. *)
let cenv (Compiler (m, _)): env = m

let compiler_code (Compiler (_, c)): string = c

type module_source =
  | TwoFileModuleSource of {
      int_filename: string;
      int_code: string;
      body_filename: string;
      body_code: string
    }
  | BodyModuleSource of {
      body_filename: string;
      body_code: string
    }

let parse_and_combine (env: env) (source: module_source): (env * combined_module * file_id option * file_id) =
  match source with
  | TwoFileModuleSource { int_filename; int_code; body_filename; body_code } ->
     let (env, int_file_id) = add_file env { path = int_filename; contents = int_code } in
     let (env, body_file_id) = add_file env { path = body_filename; contents = body_code } in
     let ci: concrete_module_interface = parse_module_int int_code int_filename
     and cb: concrete_module_body = parse_module_body body_code body_filename
     in
     let ci: concrete_module_interface = append_import_to_interface ci pervasive_imports
     and cb: concrete_module_body = append_import_to_body cb pervasive_imports in
     let combined: combined_module = combine env ci cb in
     (env, combined, Some int_file_id, body_file_id)
  | BodyModuleSource { body_filename; body_code } ->
     let (env, body_file_id) = add_file env { path = body_filename; contents = body_code } in
     let cb: concrete_module_body = parse_module_body body_code body_filename in
     let cb: concrete_module_body = append_import_to_body cb pervasive_imports in
     let combined: combined_module = body_as_combined env cb in
     (env, combined, None, body_file_id)

let rec compile_mod (c: compiler) (source: module_source): compiler =
  with_frame "Compile module"
    (fun _ ->
      let env: env = cenv c in
      let (env, combined, int_file_id, body_file_id) = parse_and_combine env source in
      let _ = check_ends_in_return combined in
      let (env, linked): (env * linked_module) = extract env combined int_file_id body_file_id in
      let typed: typed_module = augment_module env linked in
      let _ = check_module_linearity typed in
      let env: env = extract_bodies env typed in
      let (env, mono): (env * mono_module) = monomorphize env typed in
      let unit = gen_module env mono in
      let c_code: string = render_unit unit in
      Compiler (env, (compiler_code c) ^ "\n" ^ c_code))

let rec compile_multiple c modules =
  match modules with
  | m::rest -> compile_multiple (compile_mod c m) rest
  | [] -> c

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
                  err "The return type of the entrypoint function must be `ExitCode`,"
             | [ValueParameter (_, pt)] ->
                (* Single parameter case: the `root` parameter. *)
                if is_root_cap_type pt then
                  if is_root_cap_type rt then
                    (id, RootCapEntrypoint)
                  else
                    err "Entrypoint function must return a value of type RootCapability."
                else
                  err "Entrypoint function must take a single argument of type RootCapability."
             | _ ->
                err "Entrypoint function must take a single parameter of type RootCapability, or zero parameters."
           else
             err "Entrypoint function cannot be generic."
         else
           err "Entrypoint function is not public."
      | _ ->
         err "Entrypoint is not a function.")
  | None ->
     err "Entrypoint does not exist."

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

let entrypoint_code root_cap_mono_id id =
  let f = gen_decl_id id in
  "int main() {\n    " ^ f ^ "((" ^ (gen_mono_id root_cap_mono_id) ^ "){ .value = false });\n    return 0;\n}\n"

let empty_entrypoint_code (entrypoint_id: decl_id) (exit_code_id: mono_id): string =
  let f = gen_decl_id entrypoint_id in
  let exit_code: string = gen_mono_id exit_code_id in
  ("int main() {\n"
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
     (match get_type_monomorph env id [] with
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
  | Some (Record { id; _ }) ->
     (match get_type_monomorph env id [] with
      | Some id ->
         id
      | _ ->
         internal_err "No monomorph of ExitCode.")
  | _ ->
     internal_err "Can't find the ExitCode type in the environment."

let compile_entrypoint c mn i =
  let qi = make_qident (mn, i, i) in
  let (entrypoint_id, kind): decl_id * entrypoint_kind = check_entrypoint_validity (cenv c) qi in
  let (Compiler (m, code)) = c in
  let entry_code: string =
    match kind with
    | EmptyEntrypoint ->
       empty_entrypoint_code entrypoint_id (get_exit_code_monomorph (cenv c))
    | RootCapEntrypoint ->
       entrypoint_code (get_root_capability_monomorph (cenv c)) entrypoint_id
  in
  Compiler (m, code ^ "\n" ^ entry_code)

let fake_mod_source (is: string) (bs: string): module_source =
  TwoFileModuleSource { int_filename = ""; int_code = is; body_filename = ""; body_code = bs }

let dump_and_die _: 'a =
  print_endline "Compiler call tree printed to calltree.html";
  Reporter.dump ();
  exit (-1)

let empty_compiler: compiler =
  with_frame "Compile built-in modules"
    (fun _ ->
      (* We have to compile the Austral.Pervasive module, followed by
         Austral.Memory, since the latter uses declarations from the former. *)
      let env: env = empty_env in
      (* Start with the C prelude interface. *)
      let c = Compiler (env, prelude_h) in
      let c =
        (* Handle errors during the compilation of the Austral,Pervasive
           module. Otherwise, a typo in the source code of this module will cause a
           fatal error due to an exception stack overflow (unsure why this
           happens). *)
        try
          compile_mod c (fake_mod_source pervasive_interface_source pervasive_body_source)
        with Austral_error error ->
          Printf.eprintf "%s" (render_error_to_plain error);
          dump_and_die ()
      in
      let c =
        try
          compile_mod c (fake_mod_source memory_interface_source memory_body_source)
        with Austral_error error ->
          Printf.eprintf "%s" (render_error_to_plain error);
          dump_and_die ()
      in
      c)

let compile_and_run (modules: (string * string) list) (entrypoint: string): (int * string) =
  let compiler = compile_multiple empty_compiler (List.map (fun (i, b) -> fake_mod_source i b) modules) in
  let (entrypoint_mod, entrypoint_name) =
    let ss = String.split_on_char ':' entrypoint in
    match ss with
    | [mn; i] ->
       (make_mod_name mn, make_ident i)
    | _ ->
       err "Bad entrypoint format."
  in
  let compiler = compile_entrypoint compiler entrypoint_mod entrypoint_name in
  let code = compiler_code compiler in
  let code_path = temp_file "code" ".c"
  and bin_path = temp_file "program" ".exe" in
  write_string_to_file code_path code;
  let _ = compile_c_code code_path bin_path in
  let (CommandOutput { code; stdout; _ }) = run_command bin_path in
  (code, stdout)
