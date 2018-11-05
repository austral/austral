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

structure MTAST :> MTAST = struct
    type name = Symbol.symbol
    type ty = MonoType.ty

    (* Expression AST *)

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string * ty
                 | FloatConstant of string * ty
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable * ty
                 | Let of Symbol.variable * ast * ast
                 | Bind of Symbol.variable list * ast * ast
                 | Cond of ast * ast * ast
                 | ArithOp of Arith.kind * Arith.oper * ast * ast
                 | TupleCreate of ast list
                 | TupleProj of ast * int
                 | ArrayLength of ast
                 | ArrayPointer of ast
                 | Allocate of ast
                 | Load of ast
                 | Store of ast * ast
                 | Construct of ty * name * ast option
                 | Case of ast * variant_case list * ty
                 | ForeignFuncall of string * ast list * ty
                 | ForeignNull of ty
                 | SizeOf of ty
                 | AddressOf of Symbol.variable * ty
                 | Cast of ty * ast
                 | Seq of ast * ast
                 | Funcall of Symbol.symbol * ast list * ty
         and variant_case = VariantCase of case_name * ast
         and case_name = NameOnly of name
                       | NameBinding of { casename: name, var: Symbol.variable, ty: ty }

    (* Block AST *)

    datatype top_ast = Defun of name * param list * ty * ast
                     | ToplevelProgn of top_ast list
         and param = Param of Symbol.variable * ty

    (* Monomorphization *)

    type type_monomorphs = MonoType.type_monomorphs
    type replacements = MonoType.replacements

    datatype context = Context of type_monomorphs * replacements

    val emptyContext =
        Context (MonoType.TypeMonos Map.empty,
                 Map.empty)

    (* Monomorphization utilities *)

    fun monoType ty ctx =
        let val (Context (tm, rs)) = ctx
        in
            let val (ty', tm') = MonoType.monomorphize tm rs ty
            in
                (ty', Context (tm', rs))
            end
        end

    fun forciblyMonomorphize ctx ty =
        (* ONLY USE THIS when you can ignore resulting monomorphs, e.g. in a
           defun or some other provably-concrete context *)
        let val (Context (tm, rs)) = ctx
        in
            let val (ty', _) = MonoType.monomorphize tm
                                                     rs
                                                     ty
            in
                ty'
            end
        end

    (* Monomorphize a type with an empty replacements map, that is, in a
       concrete context. *)

    fun monomorphize ctx TAST.UnitConstant =
        (UnitConstant, ctx)
      | monomorphize ctx (TAST.BoolConstant b) =
        (BoolConstant b, ctx)
      | monomorphize ctx (TAST.IntConstant (i, ty)) =
        let val ty' = forciblyMonomorphize ctx ty
        in
            case ty' of
                (MonoType.Integer _) => (IntConstant (i, ty'), ctx)
              | _ => raise Fail "Internal error: not a valid type for an integer constant"
        end
      | monomorphize ctx (TAST.FloatConstant (f, ty)) =
        let val ty' = forciblyMonomorphize ctx ty
        in
            case ty' of
                (MonoType.Float _) => (FloatConstant (f, ty'), ctx)
              | _ => raise Fail "Internal error: not a valid type for a float constant"
        end
      | monomorphize ctx (TAST.StringConstant s) =
        (StringConstant s, ctx)
      | monomorphize ctx (TAST.Variable (var, ty)) =
        let val (ty', ctx) = monoType ty ctx
        in
            (Variable (var, ty'), ctx)
        end
      | monomorphize ctx (TAST.Let (name, value, body)) =
        let val (value', ctx) = monomorphize ctx value
        in
            let val (body', ctx) = monomorphize ctx body
            in
                (Let (name, value', body'), ctx)
            end
        end
      | monomorphize ctx (TAST.Bind (names, tup, body)) =
        let val (tup', ctx) = monomorphize ctx tup
        in
            let val (body', ctx) = monomorphize ctx body
            in
                (Bind (names, tup', body'), ctx)
            end
        end
      | monomorphize ctx (TAST.Cond (t, c, a)) =
        let val (t', ctx) = monomorphize ctx t
        in
            let val (c', ctx) = monomorphize ctx c
            in
                let val (a', ctx) = monomorphize ctx a
                in
                    (Cond (t', c', a'), ctx)
                end
            end
        end
      | monomorphize _ _ =
        raise Fail "Not implemented yet"

    fun monomorphizeTop ctx (TAST.Defun (name, params, rt, _, body)) =
        monomorphizeDefun ctx name params rt body
      | monomorphizeTop ctx (TAST.Defgeneric _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop ctx (TAST.Defclass _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop _ (TAST.Definstance (name, arg, docstring, methods)) =
        raise Fail "Not implemented yet"
      | monomorphizeTop _ (TAST.Deftype (name, params, _, ty)) =
        raise Fail "Not implemented yet"
      | monomorphizeTop _ (TAST.Defdisjunction (name, params, _, variants)) =
        raise Fail "Not implemented yet"
      | monomorphizeTop ctx (TAST.Deftemplate _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop ctx (TAST.DefineSymbolMacro _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop ctx (TAST.Defmodule _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop ctx (TAST.InModule _) =
        (ToplevelProgn [], ctx)
      | monomorphizeTop ctx (TAST.Defcfun _) =
        (ToplevelProgn [], ctx)

    and monomorphizeDefun ctx name params rt body =
        let fun mapParam (TAST.Param (var, ty)) =
                Param (var, forciblyMonomorphize ctx ty)
        in
            raise Fail "Not implemented"
            (* Defun (name,
                      map mapParam params,
                      concreteMonoType rt,
                      monomorphize ctx body) *)
        end
end
