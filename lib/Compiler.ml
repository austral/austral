open Identifier
open ModuleSystem
open BuiltIn
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
open Filename

let append_import_to_interface ci import =
  let (ConcreteModuleInterface (mn, imports, decls)) = ci in
  if equal_module_name mn pervasive_module_name then
    ci
  else
    ConcreteModuleInterface (mn, import :: imports, decls)

let append_import_to_body cb import =
  let (ConcreteModuleBody (mn, kind, imports, decls)) = cb in
  if equal_module_name mn pervasive_module_name then
    cb
  else
    ConcreteModuleBody (mn, kind, import :: imports, decls)

type compiler = Compiler of menv * string

let cmenv (Compiler (m, _)) = m

let compiler_code (Compiler (_, c)) = c

type module_source = ModuleSource of {
      int_filename: string;
      int_code: string;
      body_filename: string;
      body_code: string
    }

let rec compile_mod c (ModuleSource { int_filename; int_code; body_filename; body_code }) =
  let ci = parse_module_int int_code int_filename
  and cb = parse_module_body body_code body_filename
  and menv = cmenv c in
  let ci' = append_import_to_interface ci pervasive_imports
  and cb' = append_import_to_body cb pervasive_imports in
  let combined = combine menv ci' cb' in
  let semantic = extract menv combined in
  let menv' = put_module (cmenv c) semantic in
  let typed = augment_module menv' combined in
  let cpp = gen_module typed in
  let code = render_module cpp in
  Compiler (menv', (compiler_code c) ^ "\n" ^ code)

let rec compile_multiple c modules =
  match modules with
  | m::rest -> compile_multiple (compile_mod c m) rest
  | [] -> c

let rec check_entrypoint_validity menv qi =
  match get_decl menv qi with
  | Some decl ->
     (match decl with
      | SFunctionDeclaration (vis, _, typarams, params, rt) ->
         if vis = VisPublic then
           if typarams = [] then
             match params with
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
  "int main() {\n    " ^ f ^ "(false);\n    return 0;\n}\n"

let compile_entrypoint c mn i =
  let qi = make_qident (mn, i, i) in
  check_entrypoint_validity (cmenv c) qi;
  let (Compiler (m, c)) = c in
  Compiler (m, c ^ "\n" ^ (entrypoint_code mn i))

let empty_compiler =
  let menv = put_module empty_menv memory_module in
  let menv = put_module menv pervasive_module in
  let c = Compiler (menv, prelude) in
  c

let fake_mod_source (is: string) (bs: string): module_source =
  ModuleSource { int_filename = ""; int_code = is; body_filename = ""; body_code = bs }

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
