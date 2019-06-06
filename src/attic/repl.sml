(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of Austral.

    Austral is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Austral is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Austral.  If not, see <http://www.gnu.org/licenses/>.
*)

structure Repl :> REPL = struct
    fun readUntilBlank () =
        case (TextIO.inputLine TextIO.stdIn) of
            (SOME s) => if s = "\n" then
                            ""
                        else
                            (s ^ (readUntilBlank ()))
          | NONE => OS.Process.terminate OS.Process.success

    local
        open Module
        open Compiler
    in
    fun prompt compiler =
        let val name = moduleName (currentModule compiler)
        in
            (Ident.identString name) ^ "> "
        end
    end

    fun repl () =
        let fun repl' c =
                let
                in
                    print (prompt c);
                    let val input = readUntilBlank ()
                    in
                        let val unit = Compiler.ReplUnit input
                        in
                            let val c' = Compiler.compileUnit c unit
                            in
                                print "Code:\n";
                                print (Compiler.compilerCode c');
                                print "\n\n";
                                repl' c'
                            end
                        end handle Fail s => print ("Error: " ^ s ^ "\n");
                        repl' c
                    end
                end
        in
            repl' (Compiler.compilePrelude Compiler.emptyCompiler)
        end
end
