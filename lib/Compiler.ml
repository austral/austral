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
open CRepr
open Cst
open CstUtil
open Tast
open Error
open Combined
open Linked
open Mtast
open Monomorphize
open ReturnCheck
open LinearityCheck
open Reporter
open Entrypoint
open ExportInstantiation

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

let parse_and_combine (env: env) (source: module_source): (env * module_name * combined_module * file_id option * file_id) =
  match source with
  | TwoFileModuleSource { int_filename; int_code; body_filename; body_code } ->
     let (env, int_file_id) = add_file env { path = int_filename; contents = int_code } in
     let (env, body_file_id) = add_file env { path = body_filename; contents = body_code } in
     let ci: concrete_module_interface = parse_module_int int_code int_filename in
     let name: module_name = mod_int_name ci in
     adorn_error_with_module_name name
       (fun _ ->
         let cb: concrete_module_body = parse_module_body body_code body_filename in
         let ci: concrete_module_interface = append_import_to_interface ci pervasive_imports
         and cb: concrete_module_body = append_import_to_body cb pervasive_imports in
         let combined: combined_module = combine env ci cb in
         (env, name, combined, Some int_file_id, body_file_id))
  | BodyModuleSource { body_filename; body_code } ->
     let (env, body_file_id) = add_file env { path = body_filename; contents = body_code } in
     let cb: concrete_module_body = parse_module_body body_code body_filename in
     let name: module_name = mod_body_name cb in
     adorn_error_with_module_name name
       (fun _ ->
         let cb: concrete_module_body = append_import_to_body cb pervasive_imports in
         let combined: combined_module = body_as_combined env cb in
         (env, name, combined, None, body_file_id))

let rec compile_mod (c: compiler) (source: module_source): compiler =
  with_frame "Compile module"
    (fun _ ->
      let env: env = cenv c in
      let (env, name, combined, int_file_id, body_file_id) = parse_and_combine env source in
      adorn_error_with_module_name name
       (fun _ ->
         let _ = check_ends_in_return combined in
         let (env, linked): (env * linked_module) = extract env combined int_file_id body_file_id in
         let typed: typed_module = augment_module env linked in
         let _ = check_module_linearity typed in
         let env: env = extract_bodies env typed in
         let (env, mono): (env * mono_module) = monomorphize env typed in
         let unit: c_unit = gen_module env mono in
         let unit_code: string = render_unit unit in
         let code: string = (compiler_code c) ^ "\n" ^ unit_code in
         Compiler (env, code)))

let rec compile_multiple c modules =
  match modules with
  | m::rest -> compile_multiple (compile_mod c m) rest
  | [] -> c

let compile_entrypoint c mn i =
  let qi = make_qident (mn, i, i) in
  let (Compiler (m, code)) = c in
  let entry_code: string = entrypoint_code m qi in
  Compiler (m, code ^ "\n" ^ entry_code)

let fake_mod_source (is: string) (bs: string): module_source =
  TwoFileModuleSource { int_filename = ""; int_code = is; body_filename = ""; body_code = bs }

let dump_and_die _: 'a =
  print_endline "Compiler call tree printed to calltree.html";
  Reporter.dump ();
  exit (-1)

let post_compile (compiler: compiler): compiler =
  let env: env = cenv compiler in
  let env: env = monomorphize_wrappers env in
  let wrappers: c_unit = CUnit ("Wrappers", all_wrappers env) in
  let wrapper_code: string = render_unit wrappers in
  let code: string = (compiler_code compiler) ^ "\n" ^ wrapper_code in
  Compiler (env, code)

let empty_compiler: compiler =
  with_frame "Compile built-in modules"
    (fun _ ->
      (* We have to compile the Austral.Pervasive module, followed by
         Austral.Memory, since the latter uses declarations from the former. *)
      let env: env = empty_env in
      (* Start with the C prelude. *)
      let c = Compiler (env, prelude_c) in
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
