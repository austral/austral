open ModuleSystem
open ParserInterface
open CombiningPass
open ExtractionPass

type compiler = Compiler of menv

let cmenv (Compiler m) = m

let empty_compiler = Compiler empty_menv

let rec compile_mod c is bs =
  let ci = parse_module_int is
  and cb = parse_module_body bs
  and menv = cmenv c in
  let combined = combine menv ci cb in
  let semantic = extract menv combined in
  let menv' = put_module (cmenv c) semantic in
  let _ = menv' in
  print_endline "Hi!";
  c

let rec compile_multiple c modules =
  match modules with
  | (is, bs)::rest -> compile_multiple (compile_mod c is bs) rest
  | [] -> c
