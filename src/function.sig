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
    (* Type aliases *)

    type name = Symbol.symbol
    type ty = Type.ty
    type docstring = string option

    (* Concrete functions *)

    datatype func = Function of name * param list * ty * docstring
         and param = Param of name * ty

    (* Foreign functions *)

    datatype ffunc = ForeignFunction of name * param list * foreign_arity * ty * docstring
         and foreign_arity = FixedArity
                           | VariableArity

    (* Generic functions *)

    datatype gfunc = GenericFunction of name * Type.typarams * param list * ty * docstring

    val gFunctionName : gfunc -> name

    (* Return whether a function is return type-polymorphic or not. That is, if
       there are type variables in the return type that do not appear in the
       argument list (and thus, cannot be resolved in a context-independent
       manner without type-inference), return true. *)
    val isRTP : gfunc -> bool

    (* Typeclasses *)

    type param_name = Symbol.symbol

    datatype typeclass = Typeclass of name * param_name * docstring * method_decl list
         and method_decl = MethodDecl of name * param list * ty * docstring

    datatype instance = Instance of name * instance_arg * docstring * method_def list
         and instance_arg = InstanceArg of name * Type.typarams
         and method_def = MethodDef of name * param list * ty * docstring

    (* Function environments *)

    type fenv

    val defaultFenv : fenv

    val findTypeclassByMethod : fenv -> name -> typeclass option

    val addFunction : fenv -> func -> fenv option
    val addForeignFunction : fenv -> ffunc -> fenv option
    val addGenericFunction : fenv -> gfunc -> fenv option
    val addTypeclass : fenv -> typeclass -> fenv option
    val addInstance : fenv -> instance -> fenv option

    datatype callable = CallableFunc of func
                      | CallableForeign of ffunc
                      | CallableGFunc of gfunc
                      | CallableMethod

    val envGet : fenv -> name -> callable option

    (* Given a list of function parameters, and a list of types, return a map of
    type parameter names to their replacement types if the match succeeds. Fails
    otherwise. *)
    val matchParams : param list -> ty list -> (name, ty) Map.map

   (* Given:

      1. The list of a function's parameters.
      2. The function's return type.
      3. A list of function arguments.
      4. The function's asserted return type.

      Return a map of type parameter names to their replacements. *)
    val matchFunc : param list -> ty -> ty list -> ty -> (name, ty) Map.map

    (* Given an ordered set of type parameters, and a map of type parameter
       names to their replacements, return a list of types by replacing each
       type parameter in the set with its replacement. *)
    val typeArgs : Type.typarams -> (name, ty) Map.map -> ty list
end
