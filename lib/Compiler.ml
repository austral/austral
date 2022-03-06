open Identifier
open Env
open BuiltIn
open Pervasive
open CppPrelude
open ParserInterface
open CombiningPass
open ExtractionPass
open TypingPass
open CodeGen
open CppRenderer
open Cst
open Type
open Error
open Util
open Combined
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

type module_source = ModuleSource of {
      int_filename: string;
      int_code: string;
      body_filename: string;
      body_code: string
    }

let rec compile_mod c (ModuleSource { int_filename; int_code; body_filename; body_code }) =
  let env: env = cenv c in
  let (env, int_file_id) = add_file env { path = int_filename; contents = int_code } in
  let (env, body_file_id) = add_file env { path = body_filename; contents = body_code } in
  let ci: concrete_module_interface = parse_module_int int_code int_filename
  and cb: concrete_module_body = parse_module_body body_code body_filename
  in
  let ci: concrete_module_interface = append_import_to_interface ci pervasive_imports
  and cb: concrete_module_body = append_import_to_body cb pervasive_imports in
  let combined: combined_module = combine env ci cb in
  let env: env = extract env combined int_file_id body_file_id in
  let typed = augment_module env combined in
  let cpp = gen_module typed in
  let code = render_module cpp in
  Compiler (env, (compiler_code c) ^ "\n" ^ code)

let rec compile_multiple c modules =
  match modules with
  | m::rest -> compile_multiple (compile_mod c m) rest
  | [] -> c

let rec check_entrypoint_validity (env: env) (qi: qident): unit =
  match get_decl_by_name env (qident_to_sident qi) with
  | Some decl ->
     (match decl with
      | Function { vis; typarams; value_params; rt; _ } ->
         if vis = VisPublic then
           if typarams = [] then
             match value_params with
             | [ValueParameter (_, pt)] ->
                if is_root_cap_type pt then
                  if is_root_cap_type rt then
                    ()
                  else
                    err "Entrypoint function must return a value of type Root_Capability."
                else
                  err "Entrypoint function must take a single argument of type Root_Capability."
             | _ ->
                err "Entrypoint function must take a single argument of type Root_Capability."
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
     and n = equal_identifier (original_name name) root_cap_type_name in
     m && n
  | _ ->
     false

let entrypoint_code mn i =
  let f = (gen_module_name mn) ^ "::" ^ (gen_ident i) in
  "int main() {\n    " ^ f ^ "((A_Austral__Pervasive::A_Root_Capability) { .value = false });\n    return 0;\n}\n"

let compile_entrypoint c mn i =
  let qi = make_qident (mn, i, i) in
  check_entrypoint_validity (cenv c) qi;
  let (Compiler (m, c)) = c in
  Compiler (m, c ^ "\n" ^ (entrypoint_code mn i))

let fake_mod_source (is: string) (bs: string): module_source =
  ModuleSource { int_filename = ""; int_code = is; body_filename = ""; body_code = bs }

let empty_compiler: compiler =
  (* Add the main prelude. Then compile the Austral.Pervasive module. *Then* add
     the source code of the FFI module. This is because the FFI module uses the
     Option type, which is defined in the Austral.Pervasive module and has to be
     compiled first. *)
  let env: env = add_memory_module empty_env in
  let c = Compiler (env, prelude) in
  let c =
    (* Handle errors during the compilation of the Austral,Pervasive
       module. Otherwise, a typo in the source code of this module will cause a
       fatal error due to an exception stack overflow (unsure why this
       happens). *)
    try
      compile_mod c (fake_mod_source pervasive_interface_source pervasive_body_source)
    with Austral_error error ->
      Printf.eprintf "%s" (render_error error None);
      exit (-1)
  in
  let (Compiler (menv, code)) = c in
  Compiler (menv, code ^ austral_memory_code)

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
  let code_path = temp_file "code" ".cpp"
  and bin_path = temp_file "program" ".exe" in
  write_string_to_file code_path code;
  let _ = compile_cpp_code code_path bin_path in
  let (CommandOutput { code; stdout; _ }) = run_command bin_path in
  (code, stdout)
