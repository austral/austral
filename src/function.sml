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

structure Function :> FUNCTION = struct
    type name = Symbol.symbol
    type ty = Type.ty
    type docstring = string option

    datatype func = Function of name * param list * ty * docstring
         and param = Param of name * ty

    type param_name = Symbol.symbol

    datatype typeclass = Typeclass of name * param_name * docstring * method_decl list
         and method_decl = MethodDecl of name * param list * ty * docstring

    datatype instance = Instance of name * instance_arg * docstring * method_def list
         and instance_arg = TypeCons of name * tyvar OrderedSet.set
         and tyvar = TypeVar of name
         and method_def = MethodDef of name * param list * ty * docstring * RCST.rcst

    datatype fenv = FunctionEnv of (name, func) Map.map * typeclass list * instance list

    val emptyFenv = FunctionEnv (Map.empty, [], [])

    fun addFunction (FunctionEnv (fm, ts, is)) f =
        let val (Function (name, _, _, _)) = f
        in
            case Map.get fm name of
                SOME _ => NONE
              | NONE => SOME (FunctionEnv (Map.iadd fm (name, f), ts, is))
        end

    datatype callable = CallableFunc of func
                      | CallableMethod

    fun envGet menv name =
        raise Fail "derp"
end
