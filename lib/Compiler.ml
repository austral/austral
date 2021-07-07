open ModuleSystem
open BuiltIn
open CppPrelude
open ParserInterface
open CombiningPass
open ExtractionPass
open TypingPass
open CodeGen
open CppRenderer

type compiler = Compiler of menv * string

let cmenv (Compiler (m, _)) = m

let compiler_code (Compiler (_, c)) = c

let empty_compiler =
  let menv = put_module empty_menv memory_module in
  Compiler (menv, prelude)

let rec compile_mod c is bs =
  let ci = parse_module_int is
  and cb = parse_module_body bs
  and menv = cmenv c in
  let combined = combine menv ci cb in
  let semantic = extract menv combined in
  let menv' = put_module (cmenv c) semantic in
  let typed = augment_module menv' combined in
  let cpp = gen_module typed in
  let code = render_module cpp in
  Compiler (menv', (compiler_code c) ^ "\n" ^ code)

let rec compile_multiple c modules =
  match modules with
  | (is, bs)::rest -> compile_multiple (compile_mod c is bs) rest
  | [] -> c
