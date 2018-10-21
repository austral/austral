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

structure AST :> AST = struct
    type typespec = Type.typespec

    (* Expression AST *)

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable
                 | Let of Symbol.variable * ast * ast
                 | Cond of ast * ast * ast
                 | ArithOp of Arith.kind * Arith.oper * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | StaticArrayLength of ast
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | The of Type.typespec * ast
                 | ForeignFuncall of string * typespec * ast list
                 | ForeignNull of typespec
                 | SizeOf of typespec
                 | Seq of ast * ast
                 | Funcall of Symbol.symbol * ast list

    (* Toplevel AST *)

    type name = Symbol.symbol
    type param_name = name
    type docstring = string option
    type symbol = Symbol.symbol
    type variable = Symbol.variable

    datatype top_ast = Defun of name * param list * typespec * docstring * ast
                     | Defgeneric of name * param_name list * param list * typespec * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * name list * docstring * typespec
                     | Defdisjunction of name * name list * docstring * variant list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
         and param = Param of variable * typespec
         and method_decl = MethodDecl of name * param list * typespec * docstring
         and method_def = MethodDef of name * param list * typespec * docstring * ast
         and instance_arg = InstanceArg of name * name list
         and variant = Variant of name * typespec option

    (* Transform alpha-renamed AST to this AST *)

    fun transform Alpha.UnitConstant =
        UnitConstant
      | transform (Alpha.BoolConstant b) =
        BoolConstant b
      | transform (Alpha.IntConstant i) =
        IntConstant i
      | transform (Alpha.FloatConstant f) =
        FloatConstant f
      | transform (Alpha.StringConstant s) =
        StringConstant s
      | transform (Alpha.Variable v) =
        Variable v
      | transform (Alpha.Let (var, value, body)) =
        Let (var, transform value, transform body)
      | transform (Alpha.The (ty, exp)) =
        The (ty, transform exp)
      | transform (Alpha.ForeignFuncall (name, rt, args)) =
        ForeignFuncall (name, rt, map transform args)
      | transform (Alpha.SizeOf tys) =
        SizeOf tys
      | transform (Alpha.Operation (f, args)) =
        transformOp f (map transform args)
    and transformOp f args =
        let val au = Symbol.au
        in
            if f = au "progn" then
                transformProgn args
            else if f = au "if" then
                transformCond args
            (* Modular arithmetic *)
            else if f = au "+" then
                transformArith Arith.Modular Arith.Add args
            else if f = au "-" then
                transformArith Arith.Modular Arith.Sub args
            else if f = au "*" then
                transformArith Arith.Modular Arith.Mul args
            else if f = au "/" then
                transformArith Arith.Modular Arith.Div args
            (* Checked arithmetic *)
            else if f = au "&+" then
                transformArith Arith.Checked Arith.Add args
            else if f = au "&-" then
                transformArith Arith.Checked Arith.Sub args
            else if f = au "&*" then
                transformArith Arith.Checked Arith.Mul args
            else if f = au "&/" then
                transformArith Arith.Checked Arith.Div args
            (* Saturation arithmetic *)
            else if f = au "^+" then
                transformArith Arith.Saturation Arith.Add args
            else if f = au "^-" then
                transformArith Arith.Saturation Arith.Sub args
            else if f = au "^*" then
                transformArith Arith.Saturation Arith.Mul args
            else if f = au "^/" then
                transformArith Arith.Saturation Arith.Div args
            (* Float arithmetic *)
            else if f = au ".+" then
                transformArith Arith.Float Arith.Add args
            else if f = au ".-" then
                transformArith Arith.Float Arith.Sub args
            else if f = au ".*" then
                transformArith Arith.Float Arith.Mul args
            else if f = au "./" then
                transformArith Arith.Float Arith.Div args
            (* Tuple operations *)
            else if f = au "tuple" then
                TupleCreate (args)
            else if f = au "proj" then
                transformProj args
           (* Arrays *)
            else if f = au "static-array-length" then
                transformSArrayLength args
           (* Pointers *)
            else if f = au "allocate" then
                transformAlloc args
            else if f = au "load" then
                transformLoad args
            else if f = au "store" then
                transformStore args
            else
                Funcall (f, args)
        end

    and transformProgn [exp] =
        exp
      | transformProgn (first::rest) =
        Seq (first, transformProgn rest)
      | transformProgn nil =
        (* An empty progn is just the unit constant *)
        UnitConstant

    and transformCond [test, cons, alt] =
        Cond (test, cons, alt)
      | transformCond _ =
        raise Fail "Invalid `if` form"

    and transformArith kind oper [lhs, rhs] =
        ArithOp (kind, oper, lhs, rhs)
      | transformArith kind oper _ =
        raise Fail "Bad arithmetic operator"

    and transformProj [ast, IntConstant i] =
        TupleProj (ast, Option.valOf (Int.fromString i))
      | transformProj _ =
        raise Fail "Bad `proj` form"

    and transformSArrayLength [arr] =
        StaticArrayLength arr
      | transformSArrayLength _ =
        raise Fail "Bad static-array-length form"

    and transformAlloc [v] =
        Allocate v
      | transformAlloc _ =
        raise Fail "Bad `allocate` form"

    and transformLoad [ptr] =
        Load ptr
      | transformLoad _ =
        raise Fail "Bad `load` form"

    and transformStore [ptr, v] =
        Store (ptr, v)
      | transformStore _ =
        raise Fail "Bad `store` form"

    (* Parse toplevel forms into the toplevel AST *)

    fun transformTop (Alpha.Defun (name, params, typespec, docstring, ast)) =
        Defun (name,
               mapParams params,
               typespec,
               docstring,
               transform ast)
      | transformTop (Alpha.Defgeneric (name, typarams, params, typespec, docstring, ast)) =
        Defgeneric (name,
                    typarams,
                    mapParams params,
                    typespec,
                    docstring,
                    transform ast)
      | transformTop (Alpha.Defclass (name, param, docstring, methods)) =
        Defclass (name,
                  param,
                  docstring,
                  map (fn (Alpha.MethodDecl (name, params, rt, docstring)) =>
                          MethodDecl (name, mapParams params, rt, docstring))
                      methods)
      | transformTop (Alpha.Definstance (name, Alpha.InstanceArg arg, docstring, methods)) =
        Definstance (name,
                     InstanceArg arg,
                     docstring,
                     map (fn (Alpha.MethodDef (name, params, rt, docstring, body)) =>
                             MethodDef (name,
                                        mapParams params,
                                        rt,
                                        docstring,
                                        transform body))
                         methods)
      | transformTop (Alpha.Deftype tydef) =
        Deftype tydef
      | transformTop (Alpha.Defdisjunction (name, typarams, docstring, variants)) =
        Defdisjunction (name,
                        typarams,
                        docstring,
                        map (fn (Alpha.Variant v) => Variant v) variants)
      | transformTop (Alpha.Deftemplate template) =
        Deftemplate template
      | transformTop (Alpha.DefineSymbolMacro mac) =
        DefineSymbolMacro mac
      | transformTop (Alpha.Defmodule module) =
        Defmodule module
      | transformTop (Alpha.InModule name) =
        InModule name

    and mapParams params =
        map (fn (Alpha.Param (name, ty)) => Param (name, ty)) params
end
