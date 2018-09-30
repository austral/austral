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

structure Repl :> REPL = struct
  fun readUntilBlank () =
    case (TextIO.inputLine TextIO.stdIn) of
        (SOME s) => if s = "\n" then
                        ""
                    else
                        (s ^ (readUntilBlank ()))
      | NONE => OS.Process.terminate OS.Process.success

  fun repl () =
    let fun repl' c =
          let
          in
              print "> ";
              let val input = readUntilBlank ()
              in
                  let val unit = Compiler.ReplUnit input
                  in
                      let val c' = Compiler.compileUnit c unit
                      in
                          print "Compiled";
                          repl' c'
                      end
                  end handle Fail s => print ("Error: " ^ s ^ "\n");
              repl' c
              end
          end
    in
        repl' Compiler.emptyCompiler
    end
end
