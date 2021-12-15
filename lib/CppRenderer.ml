open Common
open Escape
open Cpp
open Util

type indentation = Indentation of int

let zero_indent = Indentation 0

let indent (Indentation i) =
  Indentation (i+4)

type line = Line of indentation * string

let render_line (Line (Indentation i, s)) =
  (String.make i ' ') ^ s

let rec render_module d =
  String.concat "\n" (List.map render_line (render_decl zero_indent d))

and render_decl i d =
  match d with
  | CNamespace (name, decls) ->
     List.concat [
         [Line (i, "namespace " ^ name ^ " {")];
         List.concat (List.map (render_decl (indent i)) decls);
         [Line (i, "}")]
       ]
  | CUsingDeclaration { namespace; symbol } ->
     [Line (i, "using " ^ namespace ^ "::" ^ symbol)]
  | CConstantDefinition (name, ty, value) ->
     [Line (i, (render_type ty) ^ " " ^ name ^ " = " ^ (e value) ^ ";")]
  | CStructForwardDeclaration (name, typarams) ->
     List.concat [
         render_typarams i typarams;
         [Line (i, "struct " ^ name ^ ";")]
       ]
  | CTypeDefinition (name, typarams, def) ->
     List.concat [
         render_typarams i typarams;
         [Line (i, "typedef " ^ (render_type def) ^ " " ^ name ^ ";")]
       ]
  | CStructDefinition (typarams, record) ->
     List.concat [
         render_typarams i typarams;
         [Line (i, (render_struct record) ^ ";")]
       ]
  | CEnumDefinition (name, cases) ->
     List.concat [
         [Line (i, "enum " ^ name ^ " {")];
         [Line (indent i, comma_sep (List.map (fun case -> case) cases))];
         [Line (i, "};")];
       ]
  | CFunctionDeclaration (name, typarams, params, rt, linkage) ->
     let s = (render_linkage linkage)
             ^ (render_type rt)
             ^ " "
             ^ name
             ^ "("
             ^ (comma_sep (List.map (fun (CValueParam (n, t)) -> (render_type t) ^ " " ^ n) params))
             ^ ");"
     in
     List.concat [
         render_typarams i typarams;
         [Line (i, s)]
       ]
  | CFunctionDefinition (name, typarams, params, rt, body) ->
     let s = (render_type rt)
             ^ " "
             ^ name
             ^ "("
             ^ (comma_sep (List.map (fun (CValueParam (n, t)) -> (render_type t) ^ " " ^ n) params))
             ^ ") {"
     in
     List.concat [
         render_typarams i typarams;
         [Line (i, s)];
         render_stmt (indent i) body;
         [Line (i, "}")];
       ]

and render_typarams i params =
  let t (CTypeParam n) = "typename " ^ n in
  match params with
  | first::rest ->
     [Line (i, "template<" ^ (comma_sep (List.map t (first::rest))) ^ ">")]
  | [] -> []

and render_linkage = function
  | LinkageInternal -> ""
  | LinkageExternal -> "extern \"C\" "

and render_stmt (i: indentation) (stmt: cpp_stmt): line list =
  match stmt with
  | CLet (name, ty, value) ->
     let s = (render_type ty) ^ " " ^ name ^ " = " ^ (e value) ^ ";" in
     [Line (i, s)]
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
  | CFor (v, initial, f, b) ->
     let s = "size_t "
             ^ v
             ^ " = "
             ^ (e initial)
             ^ "; "
             ^ v
             ^ " < "
             ^ (e f)
             ^ "; "
             ^ v
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
  | CNamedType (n, args) ->
     n ^ render_type_args args
  | CPointer t ->
     (render_type t) ^ "*"
  | CStructType s ->
     render_struct s
  | CUnionType slots ->
     "union "
     ^ "{ "
     ^ (String.concat "" (List.map (fun (CSlot (n, t)) -> (render_type t) ^ " " ^ n ^ ";") slots))
     ^ "}"

and render_type_args = function
  | first::rest -> "<" ^ (comma_sep (List.map render_type (first::rest))) ^ ">"
  | [] -> ""

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
  | (CVar s) -> s
  | (CFuncall (n, a, tyargs)) ->
     let t' = if tyargs = [] then
                ""
              else
                "<" ^ (String.concat ", " (List.map render_type tyargs)) ^ ">"
     in
     n ^ t' ^ (paren (comma_sep (List.map e a)))
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
     "&" ^ (e exp)
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
(*
and semi_sep ss =
  String.concat "; " ss
 *)
