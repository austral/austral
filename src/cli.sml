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

structure Cli :> CLI = struct
    (* Utilities *)

    fun println str = print (str ^ "\n")

    fun die str =
        let
        in
            println str;
            OS.Process.terminate OS.Process.failure
        end

    (* Command line argument management stuff *)

    type arg = string
    type args = arg list

    fun getArgs args name =
        let val prefix = "--" ^ name ^ "="
        in
            List.mapPartial (fn s => Util.afterPrefix s prefix)
                            args
        end

    fun getArg args name =
        case (getArgs args name) of
            (first::rest) => SOME first
          | _ => NONE

    fun getPosArgs args =
        List.filter (fn arg => not (String.isPrefix "--" arg)) args

    (* Entrypoint functions *)

    fun entrypoint ["repl"] =
        Repl.repl ()
      | entrypoint args =
        let val files = getPosArgs args
            and output = getArg args "output"
        in
            case files of
                nil => die "No input files"
              | _ => (case output of
                          SOME output' => compileFiles files output
                        | NONE => die "Must provide a value to --output, for example, --output=file.cpp")
        end

    and compileFiles files output =
        let val compiler = Compiler.emptyCompiler
            and units = map (fn f => Compiler.FileUnit f) files
        in
            let val c' = Compiler.compileUnits compiler units
            in
                Util.writeStringToFile output (Compiler.compilerCode c')
            end
        end
end
