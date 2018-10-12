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

structure Alpha :> ALPHA = struct
    type symbol = Symbol.symbol
    type variable = Symbol.variable
    type typespec = Type.typespec

    (* AST *)

    datatype ast = UnitConstant
                 | BoolConstant of bool
                 | IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of variable
                 | Let of variable * ast * ast
                 | The of typespec * ast
                 | Operation of symbol * ast list

    (* Toplevel AST *)

    type name = symbol
    type docstring = string option
    type param_name = symbol

    datatype top_ast = Defun of name * param list * typespec * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * name list * docstring * typespec
                     | Defdisjunction of name * name list * docstring * variant list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
         and param = Param of name * typespec
         and method_decl = MethodDecl of name * param list * typespec * docstring
         and method_def = MethodDef of name * param list * typespec * docstring * ast
         and instance_arg = InstanceArg of name * name Set.set
         and variant = Variant of name * typespec option

    (* Fresh variables *)

    val count = ref 0

    fun freshVar sym =
        let
        in
            count := !count + 1;
            Symbol.Var (sym, !count)
        end

    fun resetCount () =
        count := 0

    (* The stack *)

    type stack = (Symbol.symbol * Symbol.variable) list

    fun lookup ((n,v)::xs) s = if (n = s) then
                                   v
                               else
                                   lookup xs s
      | lookup nil s = raise Fail ("Alpha renaming: no such variable: '"
                                   ^ (Ident.identString (Symbol.symbolName s))
                                   ^ "'")

    (* Alpha renaming *)

    fun alphaRename _ OAST.UnitConstant =
        UnitConstant
      | alphaRename _ (OAST.BoolConstant b) =
        BoolConstant b
      | alphaRename _ (OAST.IntConstant s) =
        IntConstant s
      | alphaRename _ (OAST.FloatConstant f) =
        FloatConstant f
      | alphaRename _ (OAST.StringConstant s) =
        StringConstant s
      | alphaRename s (OAST.Symbol name) =
        let val au = Symbol.au
        in
            if name = au "nil" then
                UnitConstant
            else if name = au "true" then
                BoolConstant true
            else if name = au "false" then
                BoolConstant false
            else
                Variable (lookup s name)
        end
      | alphaRename s (OAST.Let (var, value, body)) =
        let val fresh = freshVar var
          in
              let val s' = (var, fresh) :: s
              in
                  let val body' = alphaRename s' body
                  in
                      Let (fresh, alphaRename s value, body')
                  end
              end
        end
      | alphaRename s (OAST.The (ty, exp)) =
        The (ty, alphaRename s exp)
      | alphaRename s (OAST.Operation (f, args)) =
        Operation (f, map (alphaRename s) args)

    (* Public interface *)

    type params = Symbol.symbol Set.set

    fun transform oast params =
        let
        in
            resetCount ();
            alphaRename (paramsToStack params) oast
        end
    and paramsToStack set =
        makeStack (Set.toList set)
    and makeStack (head::tail) =
        (head, freshVar head) :: (makeStack tail)
      | makeStack nil =
        nil

    (* Transform the OAST top-level AST to this top-level AST *)

    fun transformTop (OAST.Defun (name, params, rt, docstring, ast)) =
        Defun (name,
               mapParams params,
               rt,
               docstring,
               transformWithParams ast params)
      | transformTop (OAST.Defclass (name, param, docstring, methods)) =
        Defclass (name,
                  param,
                  docstring,
                  map (fn (OAST.MethodDecl (name, params, rt, docstring)) =>
                          MethodDecl (name, mapParams params, rt, docstring))
                      methods)
      | transformTop (OAST.Definstance (name, OAST.InstanceArg arg, docstring, methods)) =
        Definstance (name,
                     InstanceArg arg,
                     docstring,
                     map (fn (OAST.MethodDef (name, params, rt, docstring, body)) =>
                             MethodDef (name,
                                        mapParams params,
                                        rt,
                                        transformWithParams body params))
                         methods)
      | transformTop (OAST.Deftype tydef) =
        Deftype tydef
      | transformTop (OAST.Defdisjunction (name, typarams, docstring, variants)) =
        Defdisjunction (name,
                        typarams,
                        docstring,
                        map (fn (OAST.Variant v) => Variant v) variants)
      | transformTop (OAST.Deftemplate template) =
        Deftemplate template
      | transformTop (OAST.DefineSymbolMacro mac) =
        DefineSymbolMacro mac
      | transformTop (OAST.Defmodule module) =
        Defmodule module
      | transformTop (OAST.InModule name) =
        InModule name
      | transformTop _ =
        raise Fail "derp"

    and mapParams params =
        map (fn (OAST.Param (name, ty)) => Param (name, ty)) params

    and transformWithParams ast params =
        transform ast (Set.fromList (map (fn (OAST.Param (name, _)) => name) params))
end
