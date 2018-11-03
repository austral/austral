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

structure DAST :> DAST = struct
    type ty = Type.ty
    type name = Symbol.symbol
    type param_name = name
    type docstring = string option
    type variable = Symbol.variable
    type ast = AST.ast

    datatype top_ast = Defun of name * param list * ty * docstring * ast
                     | Defgeneric of name * Type.typarams * param list * ty * docstring * ast
                     | Defclass of name * param_name * docstring * method_decl list
                     | Definstance of name * instance_arg * docstring * method_def list
                     | Deftype of name * Type.typarams * docstring * ty
                     | Defdisjunction of name * Type.typarams * docstring * Type.variant list
                     | Deftemplate of Macro.template
                     | DefineSymbolMacro of name * RCST.rcst * docstring
                     | Defmodule of Symbol.module_name * Module.defmodule_clause list
                     | InModule of Symbol.symbol_name
                     | Defcfun of name * string * param list * ty * docstring
         and param = Param of variable * ty
         and method_decl = MethodDecl of name * param list * ty * docstring
         and method_def = MethodDef of name * param list * ty * docstring * ast
         and instance_arg = InstanceArg of name * Type.typarams

    type tenv = Type.tenv
    type fenv = Function.fenv

    fun mapParam tenv typarams (AST.Param (name, spec)) =
        Param (name, Type.resolve tenv (OrderedSet.toUnordered typarams) spec)

    fun transformTop (AST.Defun (name, params, ty, docstring, ast)) tenv fenv =
        let val params' = map (mapParam tenv OrderedSet.empty) params
        in
            Defun (name,
                   params',
                   Type.resolve tenv Set.empty ty,
                   docstring,
                   ast)
        end
      | transformTop (AST.Defgeneric (name, typarams, params, ty, docstring, ast)) tenv fenv =
        let val typarams' = OrderedSet.fromList (map (fn name => Type.TypeParam name) typarams)
        in
            let val params' = map (mapParam tenv typarams') params
            in
                Defgeneric (name,
                            typarams',
                            params',
                            Type.resolve tenv (OrderedSet.toUnordered typarams') ty,
                            docstring,
                            ast)
            end
        end
      | transformTop (AST.Defclass (name, paramName, docstring, methods)) tenv fenv =
        let val typarams = OrderedSet.singleton (Type.TypeParam paramName)
        in
            let fun augmentMethod (AST.MethodDecl (name, params, tys, docstring)) =
                    MethodDecl (name,
                                map (mapParam tenv typarams) params,
                                Type.resolve tenv (OrderedSet.toUnordered typarams) tys,
                                docstring)
            in
                Defclass (name,
                          paramName,
                          docstring,
                          map augmentMethod methods)
            end
        end
      | transformTop (AST.Definstance (name, AST.InstanceArg (arg, typarams), docstring, defs)) tenv fenv =
        let val typarams' = OrderedSet.fromList (map (fn name => Type.TypeParam name)
                                                     typarams)
        in
            let fun mapDef (AST.MethodDef (name, params, tys, docstring, ast)) =
                    let val params' = map (mapParam tenv typarams') params
                    in
                        MethodDef (name,
                                   params',
                                   Type.resolve tenv (OrderedSet.toUnordered typarams') tys,
                                   docstring,
                                   ast)
                    end
            in
                Definstance (name,
                             InstanceArg (arg, typarams'),
                             docstring,
                             map mapDef defs)
            end
        end
      | transformTop (AST.Deftype (name, params, docstring, tys)) tenv _ =
        let val params' = OrderedSet.fromList (map (fn name => Type.TypeParam name) params)
        in
            Deftype (name,
                     params',
                     docstring,
                     Type.resolve tenv (OrderedSet.toUnordered params') tys)
        end
      | transformTop (AST.Defdisjunction (name, params, docstring, variants)) tenv _ =
        let val params' = OrderedSet.fromList (map (fn name => Type.TypeParam name) params)
        in
            let fun mapVariant (AST.Variant (name, SOME tys)) =
                    Type.Variant (name, SOME (Type.resolve tenv (OrderedSet.toUnordered params') tys))
                  | mapVariant (AST.Variant (name, NONE)) =
                    Type.Variant (name, NONE)
            in
                Defdisjunction (name,
                                params',
                                docstring,
                                map mapVariant variants)
            end
        end
      | transformTop (AST.Deftemplate tmpl) _ _ =
        Deftemplate tmpl
      | transformTop (AST.DefineSymbolMacro (name, exp, docstring)) _ _ =
        DefineSymbolMacro (name, exp, docstring)
      | transformTop (AST.Defmodule clauses) _ _ =
        Defmodule clauses
      | transformTop (AST.InModule name) _ _ =
        InModule name
      | transformTop (AST.Defcfun (name, rawname, params, rt, docstring)) =
        Defcfun (name,
                 rawname,
                 map mapParam params,
                 rt,
                 docstring)
end
