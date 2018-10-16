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

    fun functionName (Function (name, _, _, _)) =
        name

    datatype gfunc = GenericFunction of name * Type.typarams * param list * ty * docstring

    fun gFunctionName (GenericFunction (name, _, _, _, _)) =
        name

    type param_name = Symbol.symbol

    datatype typeclass = Typeclass of name * param_name * docstring * method_decl list
         and method_decl = MethodDecl of name * param list * ty * docstring

    datatype instance = Instance of name * instance_arg * docstring * method_def list
         and instance_arg = InstanceArg of name * Type.typarams
         and method_def = MethodDef of name * param list * ty * docstring

    datatype fenv = FunctionEnv of (name, func) Map.map
                                   * (name, gfunc) Map.map
                                   * typeclass list
                                   * instance list

    fun typeclassName (Typeclass (name, _, _, _)) =
        name

    fun instanceName (Instance (name, _, _, _)) =
        name

    val defaultFenv =
        let fun funcName (Function (name, _, _, _)) =
                name
        in
            let val notFn = Function (Symbol.au "not",
                                      [Param (Symbol.au "v", Type.Bool)],
                                      Type.Bool,
                                      NONE)
            in
                let val builtins = [notFn]
                in
                    FunctionEnv (Map.fromList (map (fn f => (funcName f, f)) builtins),
                                 Map.empty,
                                 [],
                                 [])
                end
            end
        end

    fun findTypeclassByName (FunctionEnv (_, _, ts, _)) name =
        let fun isValidTC (Typeclass (name', _, _, _)) =
                name = name'
        in
            List.find isValidTC ts
        end

    fun findTypeclassByMethod (FunctionEnv (_, _, ts, _)) name =
        let fun isValidTC (Typeclass (_, _, _, methods)) =
                Option.isSome (List.find isValidMethod methods)
            and isValidMethod (MethodDecl (name', _, _, _)) =
                name = name'
        in
            List.find isValidTC ts
        end

    fun addFunction (FunctionEnv (fm, gm, ts, is)) f =
        let val (Function (name, _, _, _)) = f
        in
            (* If there's a function with this name, fail *)
            case Map.get fm name of
                SOME _ => NONE
              | NONE =>
                (* If there's a gfunc with this name also fail *)
                case Map.get gm name of
                    SOME _ => NONE
                  | NONE => SOME (FunctionEnv (Map.iadd fm (name, f), gm, ts, is))
        end

    fun addTypeclass fenv tc =
        (case findTypeclassByName fenv (typeclassName tc) of
             SOME _ => NONE
           | _ => let val (FunctionEnv (fm, gm, ts, is)) = fenv
                  in
                      SOME (FunctionEnv (fm, gm, tc :: ts, is))
                  end)

    fun addInstance fenv ins =
        (case findTypeclassByName fenv (instanceName ins) of
             SOME _ => let val (FunctionEnv (fm, gm, ts, is)) = fenv
                       in
                           SOME (FunctionEnv (fm, gm, ts, ins :: is))
                       end
           | _ => NONE)

    datatype callable = CallableFunc of func
                      | CallableMethod

    fun envGet menv name =
        let val (FunctionEnv (funs, gfuncs, classes, instances)) = menv
        in
            case Map.get funs name of
                SOME f => SOME (CallableFunc f)
              | NONE => NONE
        end
end
