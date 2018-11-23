(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Austral.

    Austral is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Austral is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Austral.  If not, see <http://www.gnu.org/licenses/>.
*)

structure CRenderer :> C_RENDERER = struct
    (* Utilities *)

    local
        open Substring
    in
        fun sepBy sep strings = trimWhitespace (String.concatWith sep strings)
        and trimWhitespace s = string (dropl (fn c => c = #"\n") (full s))

        fun commaSep strings = sepBy ", " strings
    end

    fun pad n =
        if n > 0 then
            " " ^ (pad (n-1))
        else
            ""

    val indentation = 2
    fun indent d = d + indentation
    fun unindent d = d - indentation

    (* Rendering types *)

    structure C = CAst

    fun renderType (C.NamedType n) =
        n
      | renderType (C.Pointer t) =
        (renderType t) ^ "*"
      | renderType (C.Struct slots) =
        let fun renderSlot (ty, name) =
                (renderType ty) ^ " " ^ name
        in
            let val slots' = map renderSlot slots
            in
                "struct { "
                ^ (sepBy "; " slots')
                ^ "; }"
            end
        end
      | renderType (C.Union variants) =
        let fun renderVariant (ty, name) =
                (renderType ty) ^ " " ^ name
        in
            let val variants' = map renderVariant variants
            in
                "union { "
                ^ (sepBy "; " variants')
                ^ "; }"
            end
        end

    (* Rendering expressions *)

    fun binopStr C.Add = "+"
      | binopStr C.Sub = "-"
      | binopStr C.Mul = "*"
      | binopStr C.Div = "/"
      | binopStr C.EqualTo = "=="
      | binopStr C.NotEqualTo = "!="
      | binopStr C.GreaterThan = ">"
      | binopStr C.LessThan = "<"
      | binopStr C.GreaterThanEq = ">="
      | binopStr C.LessThanEq = "<="

    fun renderExp (C.BoolConstant true) =
        "true"
      | renderExp (C.BoolConstant false) =
        "false"
      | renderExp (C.FloatConstant f) =
        f
      | renderExp (C.IntConstant i) =
        i
      | renderExp (C.StringConstant s) =
        let fun tr #"\"" = "\\\""
              | tr c = str c
        in
            "\"" ^ (String.translate tr s) ^ "\""
        end
      | renderExp C.NullConstant =
        "NULL"
      | renderExp (C.Negation exp) =
        "!" ^ (renderExp exp)
      | renderExp (C.Variable s) =
        s
      | renderExp (C.Binop (oper, a, b)) =
        "(" ^ (renderExp a) ^ " " ^ (binopStr oper) ^ " " ^ (renderExp b) ^ ")"
      | renderExp (C.Cast (ty, a)) =
        "((" ^ (renderType ty) ^ ")(" ^ (renderExp a) ^ "))"
      | renderExp (C.Deref e) =
        "*" ^ (renderExp e)
      | renderExp (C.AddressOf e) =
        "&" ^ (renderExp e)
      | renderExp (C.ArrayIndex (a, i)) =
        (renderExp a) ^ "[" ^ (renderExp i) ^ "]"
      | renderExp (C.SizeOf t) =
        "sizeof(" ^ (renderType t) ^ ")"
      | renderExp (C.StructInitializer (ty, inits)) =
        "(("
        ^ (renderType ty)
        ^ ") { "
        ^ (String.concatWith ", " (map (fn (n, e) => "." ^ (n) ^ " = " ^ (renderExp e)) inits))
        ^ " })"
      | renderExp (C.StructAccess (r, slot)) =
        (renderExp r)
        ^ "."
        ^ (slot)
      | renderExp (C.Funcall (f, args)) =
        "(" ^ f ^ "(" ^ (commaSep (map renderExp args)) ^ "))"

    (* Rendering blocks *)

    fun renderBlock' d (C.Sequence l) =
        sepBy "\n" (map (renderBlock' d) l)

      | renderBlock' d (C.Declare (t, n)) =
        (pad d) ^ (renderType t) ^ " " ^ (n) ^ ";"

      | renderBlock' d (C.Assign (var, v)) =
        (pad d) ^ (renderExp var) ^ " = " ^ (renderExp v) ^ ";"

      | renderBlock' d (C.DeclareAssign (ty, var, value)) =
        (pad d) ^ (renderType ty) ^ " " ^ var ^ " = " ^ (renderExp value) ^ ";"

      | renderBlock' d (C.Cond (t, c, a)) =
        let val cond = renderExp t
            and tblock = renderBracketedBlock (indent d) c
            and fblock = renderBracketedBlock (indent d) a
        in
            (pad d) ^ "if (" ^ cond ^ ") " ^ tblock ^ " else " ^ fblock
        end

      | renderBlock' d (C.While (t, b)) =
        (pad d) ^ "while (" ^ (renderExp t) ^ ") {\n" ^ (renderBlock' (indent d) b) ^ "\n" ^ (pad d) ^ "}"

      | renderBlock' d (C.Switch (exp, cases)) =
        (pad d) ^ "switch (" ^ (renderExp exp) ^ ") {\n"
        ^ (sepBy "\nbreak;\n" (map (fn (id, block) =>
                                       (pad (indent d)) ^ "case " ^ (Int.toString id) ^ ":\n"
                                       ^ (renderBlock' (indent (indent d)) block))
                                   cases))
        ^ (pad d) ^ "}"

      | renderBlock' d (C.VoidFuncall (name, args)) =
        (pad d) ^ name ^ "(" ^ (commaSep (map renderExp args)) ^ "));"

    and renderBracketedBlock d block =
        "{\n" ^ (renderBlock' d block) ^ "\n" ^ (pad (unindent d)) ^ "}"

    fun renderBlock b =
        renderBlock' (indent 0) b

    (* Rendering top-level AST nodes *)

    fun renderTop (C.FunctionDef (name, params, rt, body, retval)) =
        let val params' = commaSep (map renderParam params)
            and rt' = renderType rt
            and name' = name
            and body' = (renderBlock body) ^ "\n  return " ^ (renderExp retval) ^ ";"
        in
            rt' ^ " " ^ name' ^ "(" ^ params' ^ ") {\n" ^ body' ^ "\n}"
        end

      | renderTop (C.ExternFunctionDecl (name, paramtys, arity, rt)) =
        let val paramlist = "(" ^ (commaSep (map renderType paramtys))
        in
            let val paramlist' =
                    case arity of
                        Function.FixedArity => paramlist ^ ")"
                      | Function.VariableArity => paramlist ^ " ...)"
            in
                "extern " ^ renderType rt ^ " " ^ name ^ paramlist' ^ ";"
            end
        end

      | renderTop (C.TypeDef (name, ty)) =
        "typedef "
        ^ (renderType ty)
        ^ " "
        ^ name
        ^ ";\n"

      | renderTop (C.ToplevelProgn nodes) =
        sepBy "\n\n" (map renderTop nodes)

    and renderParam (C.Param (n, t)) =
        (renderType t) ^ " " ^ n
end
