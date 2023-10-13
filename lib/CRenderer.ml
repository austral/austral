(*
   Part of the Austral project, under the Apache License v2.0 with LLVM Exceptions.
   See LICENSE file for details.

   SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
*)
open Common
open Escape
open CRepr
open Util

type indentation = Indentation of int

let zero_indent = Indentation 0

let indent (Indentation i) =
  Indentation (i+4)

type line = Line of indentation * string

let render_line (Line (Indentation i, s)) =
  (String.make i ' ') ^ s

let desc_text (Desc text): string =
  "/*\n  " ^ text ^ "\n*/"

(* https://en.cppreference.com/w/c/keyword *)
let is_c_keyword name = Str.string_match (Str.regexp "alignas\\|alignof\\|auto\\|bool\\|break\\|case\\|char\\|const\\|constexpr\\|continue\\|default\\|do\\|double\\|else\\|enum\\|extern\\|false\\|float\\|for\\|goto\\|if\\|inline\\|int\\|long\\|nullptr\\|register\\|restrict\\|return\\|short\\|signed\\|sizeof\\|static\\|static_assert\\|struct\\|switch\\|thread_local\\|true\\|typedef\\|typeof\\|typeof_unqual\\|union\\|unsigned\\|void\\|volatile\\|while\\|_Alignas\\|_Alignof\\|_Atomic\\|_BitInt\\|_Bool\\|_Complex\\|_Decimal128\\|_Decimal32\\|_Decimal64\\|_Generic\\|_Imaginary\\|_Noreturn\\|_Static_assert\\|_Thread_local\\|_Pragma\\|asm\\|fortran") name 0

let avoid_ckw (v: string): string =
  if is_c_keyword v then ("au_ckw_" ^ v) else v

let rec render_unit (CUnit (name, decls)): string =
  let rd d =
    String.concat "\n" (List.map render_line (render_decl zero_indent d))
  in
  "/* --- BEGIN translation unit for module '" ^ name ^ "' --- */\n"
  ^ (String.concat "\n\n" (List.map rd decls))
  ^ "\n/* --- END translation unit for module '" ^ name ^ "' --- */\n"

and render_decl i d =
  match d with
  | CMacro (desc, name, value) ->
     [
       Line (i, desc_text desc);
       Line (i, "#define " ^ name ^ " " ^ (e value))
     ]
  | CConstantDefinition (desc, name, ty, value) ->
     [
       Line (i, desc_text desc);
       Line (i, (render_type ty) ^ " " ^ (avoid_ckw name) ^ " = " ^ (e value) ^ ";")
     ]
  | CStructForwardDeclaration (desc, name) ->
     [
       Line (i, desc_text desc);
       Line (i, "struct " ^ (avoid_ckw name) ^ ";");
       Line (i, "typedef struct " ^ (avoid_ckw name) ^ " " ^ (avoid_ckw name) ^ ";")
     ]
  | CTypeDefinition (desc, name, def) ->
     [
       Line (i, desc_text desc);
       Line (i, "typedef " ^ (render_type def) ^ " " ^ (avoid_ckw name) ^ ";")
     ]
  | CStructDefinition (desc, record) ->
     [
       Line (i, desc_text desc);
       Line (i, (render_struct record) ^ ";")
     ]
  | CNamedStructDefinition (desc, name, slots) ->
     [
       Line (i, desc_text desc);
       Line (i, "typedef struct " ^ (avoid_ckw name) ^ " {" ^ (String.concat "" (List.map (fun (CSlot (n, t)) -> (render_type t) ^ " " ^ n ^ ";") slots)) ^ "} " ^ (avoid_ckw name) ^ ";")
     ]
  | CEnumDefinition (desc, name, cases) ->
     List.concat [
         [Line (i, desc_text desc)];
         [Line (i, "typedef enum {")];
         [Line (indent i, comma_sep (List.map (fun case -> case) cases))];
         [Line (i, "} " ^ (avoid_ckw name) ^ ";")];
       ]
  | CFunctionDeclaration (desc, name, params, rt, linkage) ->
     let s = (render_linkage linkage)
             ^ (render_type rt)
             ^ " "
             ^ (avoid_ckw name)
             ^ "("
             ^ (comma_sep (List.map (fun (CValueParam (n, t)) -> (render_type t) ^ " " ^ (avoid_ckw n)) params))
             ^ ");"
     in
     [
       Line (i, desc_text desc);
       Line (i, s)
     ]
  | CFunctionDefinition (desc, name, params, rt, body) ->
     let s = (render_type rt)
             ^ " "
             ^ (avoid_ckw name)
             ^ "("
             ^ (comma_sep (List.map (fun (CValueParam (n, t)) -> (render_type t) ^ " " ^ (avoid_ckw n)) params))
             ^ ") {"
     in
     List.concat [
         [Line (i, desc_text desc)];
         [Line (i, s)];
         render_stmt (indent i) body;
         [Line (i, "}")];
       ]

and render_linkage = function
  | LinkageInternal -> ""
  | LinkageExternal -> "extern "

and render_stmt (i: indentation) (stmt: c_stmt): line list =
  match stmt with
  | CLet (name, ty, value) -> begin
      match value with
      | (Some value) ->
         let s = (render_type ty) ^ " " ^ (avoid_ckw name) ^ " = " ^ (e value) ^ ";" in
         [Line (i, s)]
      | None ->
         let s = (render_type ty) ^ " " ^ (avoid_ckw name) ^ ";" in
         [Line (i, s)]
     end
  | CAssign (lvalue, value) ->
     let s = (e lvalue) ^ " = " ^ (e value) ^ ";" in
     [Line (i, s)]
  | CDiscarding value ->
     [Line (i, (e value) ^ ";")]
  | CIf (c, t, f) ->
     List.concat [
         [Line (i, "if (" ^ (e c) ^ ") {")];
         render_stmt (indent i) t;
         [Line (i, "} else {")];
         render_stmt (indent i) f;
         [Line (i, "}")]
       ]
  | CSwitch (value, cases) ->
     List.concat [
         [Line (i, "switch (" ^ (e value) ^ ") {")];
         List.concat (List.map (render_switch_case (indent i)) cases);
         [Line (i, "}")]
       ]
  | CWhile (c, b) ->
     List.concat [
         [Line (i, "while (" ^ (e c) ^ ") {")];
         render_stmt (indent i) b;
         [Line (i, "}")]
       ]
  | CFor (v, final, b) ->
     let s = "; "
             ^ (avoid_ckw v)
             ^ " <= "
             ^ (e final)
             ^ "; "
             ^ (avoid_ckw v)
             ^ "++"
     in
     List.concat [
         [Line (i, "for (" ^ s ^ ") {")];
         render_stmt (indent i) b;
         [Line (i, "}")]
       ]
  | CReturn v ->
     [Line (i, "return " ^ (e v) ^ ";")]
  | CBlock ss ->
     List.concat (List.map (render_stmt i) ss)
  | CLocalFunctionDeclaration (name, params, rt, linkage) ->
     let s = (render_linkage linkage)
             ^ (render_type rt)
             ^ " "
             ^ name
             ^ "("
             ^ (comma_sep (List.map (fun (CValueParam (n, t)) -> (render_type t) ^ " " ^ n) params))
             ^ ");"
     in
     [
       Line (i, s)
     ]
  | CExplicitBlock ss ->
     let i' = indent i in
     List.concat [
         [Line (i, "{")];
         (List.concat (List.map (render_stmt i') ss));
         [Line (i, "}")];
       ]

and render_switch_case i (CSwitchCase (value, b)) =
  List.concat [
      [Line (i, "case " ^ (e value) ^ ":")];
      render_stmt (indent i) b;
      [Line (indent i, "break;")]
    ]

and render_type = function
  | CNamedType n ->
     n
  | CPointer t ->
     (render_type t) ^ "*"
  | CStructType s ->
     render_struct s
  | CUnionType slots ->
     "union "
     ^ "{ "
     ^ (String.concat "" (List.map (fun (CSlot (n, t)) -> (render_type t) ^ " " ^ n ^ ";") slots))
     ^ "}"

and render_struct (CStruct (name, slots)) =
  let name' = Option.value name ~default:"" in
  "struct "
  ^ name'
  ^ " {"
  ^ (String.concat "" (List.map (fun (CSlot (n, t)) -> (render_type t) ^ " " ^ n ^ ";") slots))
  ^ "}"

and render_expr = function
  | (CBool b) -> render_bool b
  | (CInt i) -> i
  | (CFloat f) -> f
  | (CString s) -> "\"" ^ (unescape_string s) ^ "\""
  | (CVar s) -> (avoid_ckw s)
  | (CFuncall (n, a)) ->
     (avoid_ckw n) ^ (paren (comma_sep (List.map e a)))
  | CFptrCall (expr, rt, argtys, args) ->
     let rt: string = render_type rt
     and argtys: string = String.concat ", " (List.map render_type argtys)
     and expr: string = render_expr expr
     and args: string = String.concat ", " (List.map render_expr args)
     in
     ("((" ^ rt ^ "(*)(" ^ argtys ^ "))(" ^ expr ^ "))(" ^ args ^ ");")
  | (CCast (e, t)) ->
     let e' = render_expr e
     and t' = render_type t in
     paren ((paren t') ^ " " ^ e')
  | (CArithmetic (op, l, r)) ->
     paren ((e l) ^ " " ^ render_arith op ^ " " ^ (e r))
  | (CComparison (op, l, r)) ->
     paren ((e l) ^ " " ^ (render_comp op) ^ " " ^ (e r))
  | (CConjunction (l, r)) ->
     paren ((e l) ^ " && " ^ (e r))
  | (CDisjunction (l, r)) ->
     paren ((e l) ^ " || " ^ (e r))
  | (CNegation exp) ->
     "!" ^ (paren (e exp))
  | (CIfExpression (c, t, f)) ->
     paren ((e c) ^ " ? " ^ (e t) ^ " : " ^ (e f))
  | (CStructInitializer args) ->
     "{ " ^ (comma_sep (List.map (fun (n, v) -> "." ^ n ^ " = " ^ (e v)) args)) ^ " }"
  | (CStructAccessor (v, n)) ->
     (paren (e v)) ^ "." ^ n
  | (CPointerStructAccessor (v, n)) ->
     (paren (e v)) ^ "->" ^ n
  | CIndex (arr, idx) ->
     (e arr) ^ "[" ^ (e idx) ^ "]"
  | CAddressOf exp ->
     "&(" ^ (e exp) ^ ")"
  | CPath (head, elems) ->
     let f (elem: c_path_elem): string =
       match elem with
       | CPathSlot name -> "." ^ name
       | CPathPointer name -> "->" ^ name
     in
     (render_expr head) ^ (String.concat "" (List.map f elems))
  | CEmbed (ty, expr, args) ->
     let strs = List.map render_expr args in
     (* the `expr` has a format like "derp $1 $2", so iterate over the list of
        arg swith an index, and replace the corresponding dollar sign marker if
        any *)
     let replace_index (text: string) (idx: int) (replacement: string): string =
       let marker = "$" ^ (string_of_int idx) in
       search_replace {
           text = text;
           search = marker;
           replacement = replacement
         }
     in
     let rec replace_all (text: string) (strs: string list) (idx: int): string =
       match strs with
       | first::rest ->
          let text' = replace_index text idx first in
          replace_all text' rest (idx + 1)
       | [] ->
          text
     in
     let expr' = replace_all expr strs 1 in
     "((" ^ (render_type ty) ^ ")(" ^ expr' ^ "))"
  | CDeref expr ->
     "*" ^ (e expr)
  | CSizeOf ty ->
     "sizeof(" ^ (render_type ty) ^ ")"

and e expr = render_expr expr

and render_bool = function
  | false -> "false"
  | true -> "true"

and render_arith = function
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"

and render_comp = function
  | Equal -> "=="
  | NotEqual -> "!="
  | LessThan -> "<"
  | LessThanOrEqual -> "<="
  | GreaterThan -> ">"
  | GreaterThanOrEqual -> ">="

and paren s =
  "(" ^ s ^ ")"

and comma_sep ss =
  String.concat ", " ss
