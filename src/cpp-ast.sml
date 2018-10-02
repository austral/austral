(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Boreal.

    Boreal is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Boreal is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Boreal.  If not, see <http://www.gnu.org/licenses/>.
*)

structure CppAst :> CPP_AST = struct
    (* Types *)

    datatype typaram = TypeParam of string

    datatype typespec = NamedType of string
                      | Pointer of typespec
                      | TypeCons of string * typespec list

    datatype exp_ast = ConstBool of bool
                     | ConstInt of int
                     | ConstString of string
                     | ConstNull
                     | Var of string
                     | Binop of binop * exp_ast * exp_ast
                     | Cast of typespec * exp_ast
                     | Deref of exp_ast
                     | AddressOf of exp_ast
                     | SizeOf of typespec
                     | CreateTuple of exp_ast list
                     | AccessTuple of exp_ast * int
                     | StructInitializer of string * (string * exp_ast) list
                     | StructAccess of exp_ast * string
                     | Funcall of string * typespec list * exp_ast list
                     | Raw of string
         and binop = Add
                   | Sub
                   | Mul
                   | Div
                   | EqualTo
                   | NotEqualTo
                   | GreaterThan
                   | LessThan
                   | GreaterThanEq
                   | LessThanEq

    datatype block_ast = Sequence of block_ast list
                       | Block of block_ast list
                       | Declare of typespec * string
                       | Assign of exp_ast * exp_ast
                       | Cond of exp_ast * block_ast * block_ast
                       | While of exp_ast * block_ast
                       | VoidFuncall of string * exp_ast list

    datatype top_ast = FunctionDef of string * param list * typespec * block_ast * exp_ast
                     | StructDef of string * slot list
         and param = Param of string * typespec
         and slot = Slot of string * typespec


    (* Rendering utilities *)

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

    (* Rendering *)

    fun binopStr Add = "+"
      | binopStr Sub = "-"
      | binopStr Mul = "*"
      | binopStr Div = "/"
      | binopStr EqualTo = "=="
      | binopStr NotEqualTo = "!="
      | binopStr GreaterThan = ">"
      | binopStr LessThan = "<"
      | binopStr GreaterThanEq = ">="
      | binopStr LessThanEq = "<="

    fun renderType (NamedType n) = n
      | renderType (Pointer t) = (renderType t) ^ "*"
      | renderType (TypeCons (n, args)) = n ^ "<" ^ (commaSep (map renderType args)) ^ ">"

    fun renderExp (ConstBool true) =
        "true"
      | renderExp (ConstBool false) =
        "false"
      | renderExp (ConstInt i) =
        (if i < 0 then "-" else "") ^ (Int.toString (abs i))
      | renderExp (ConstString s) =
        let fun tr #"\"" = "\\\""
              | tr c = str c
        in
            "\"" ^ (String.translate tr s) ^ "\""
        end
      | renderExp ConstNull =
        "NULL"
      | renderExp (Var s) =
        s
      | renderExp (Binop (oper, a, b)) =
        "(" ^ (renderExp a) ^ " " ^ (binopStr oper) ^ " " ^ (renderExp b) ^ ")"
      | renderExp (Cast (ty, a)) =
        "((" ^ (renderType ty) ^ ")(" ^ (renderExp a) ^ "))"
      | renderExp (Deref e) =
        "*" ^ (renderExp e)
      | renderExp (AddressOf e) =
        "&" ^ (renderExp e)
      | renderExp (SizeOf t) =
        "sizeof(" ^ (renderType t) ^ ")"
      | renderExp (CreateTuple exps) =
        "std::make_tuple(" ^ (commaSep (map renderExp exps)) ^ ")"
      | renderExp (AccessTuple (exp, i)) =
        "std::get<" ^ (Int.toString i) ^ ">(" ^ (renderExp exp) ^ ")"
      | renderExp (StructInitializer (name, inits)) =
        "(("
        ^ (name)
        ^ ") { "
        ^ (String.concatWith ", " (map (fn (n, e) => "." ^ (n) ^ " = " ^ (renderExp e)) inits))
        ^ " })"
      | renderExp (StructAccess (r, slot)) =
        (renderExp r)
        ^ "."
        ^ (slot)
      | renderExp (Funcall (f, tyargs, args)) =
        let val tyargs' = if tyargs = nil then
                              ""
                          else
                              "<" ^ (commaSep (map renderType tyargs)) ^ ">"
        in
            "(" ^ f ^ tyargs' ^ "(" ^ (commaSep (map renderExp args)) ^ "))"
        end
      | renderExp (Raw s) =
        s

    fun renderBlock' d (Sequence l) =
        sepBy "\n" (map (renderBlock' d) l)
      | renderBlock' d (Block l) =
        "{\n" ^ (sepBy "\n" (map (renderBlock' d) l)) ^ "\n" ^ (pad (unindent d)) ^ "}"
      | renderBlock' d (Declare (t, n)) =
        (pad d) ^ (renderType t) ^ " " ^ (n) ^ ";"
      | renderBlock' d (Assign (var, v)) =
        (pad d) ^ (renderExp var) ^ " = " ^ (renderExp v) ^ ";"
      | renderBlock' d (Cond (t, c, a)) =
        let val cond = renderExp t
            and tblock = renderBlock' (indent d) c
            and fblock = renderBlock' (indent d) a
        in
            (pad d) ^ "if (" ^ cond ^ ") " ^ tblock ^ " else " ^ fblock
        end
      | renderBlock' d (While (t, b)) =
        (pad d) ^ "while (" ^ (renderExp t) ^ ") {\n" ^ (renderBlock' (indent d) b) ^ "\n" ^ (pad d) ^ "}"
      | renderBlock' d (VoidFuncall (f, args)) =
        (pad d) ^ f ^ "(" ^ (commaSep (map renderExp args)) ^ ");"
    and renderRes (SOME res) = (res) ^ " = "
      | renderRes NONE = ""

    fun renderBlock b =
        renderBlock' (indent 0) b

    fun renderTop (FunctionDef (name, params, rt, body, retval)) =
        let val params' = commaSep (map renderParam params)
            and rt' = renderType rt
            and name' = name
            and body' = (renderBlock body) ^ "\n  return " ^ (renderExp retval) ^ ";"
        in
            rt' ^ " " ^ name' ^ "(" ^ params' ^ ") {\n" ^ body' ^ "\n}"
        end
      | renderTop (StructDef (name, slots)) =
        let val name' = name
            and slots' = String.concatWith " " (map renderSlot slots)
        in
            "typedef struct { " ^ slots' ^ " } " ^ name' ^ ";\n"
        end

    and renderParam (Param (n, t)) =
        (renderType t) ^ " " ^ n
    and renderSlot (Slot (n, t)) =
        (renderType t) ^ " " ^ (n) ^ ";"
end
