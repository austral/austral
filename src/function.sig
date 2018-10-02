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

signature FUNCTION = sig
    type name = Symbol.symbol
    type ty = Type.ty
    type typespec = Type.typespec
    type docstring = string option

    datatype func = Function of name * param list * ty * docstring
         and param = Param of name * typespec

    type param_name = Symbol.symbol

    datatype typeclass = Typeclass of name * param_name * docstring * method_decl list
         and method_decl = MethodDecl of name * param list * typespec * docstring

    datatype instance = Instance of name * instance_arg * docstring * method_def list
         and instance_arg = ConcreteType of typespec
                          | TypeCons of name * tyvar Set.set
         and tyvar = TypeVar of name
         and method_def = MethodDef of name * param list * typespec * docstring * RCST.rcst

    type fenv

    val emptyFenv : fenv

    val addFunction : fenv -> func -> fenv option
end
