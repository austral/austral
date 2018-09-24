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

structure Util :> UTIL = struct
    datatype 'a result = Result of 'a
                       | Failure of string

    type path = string

    fun readFileToString filepath =
        let val stream = TextIO.openIn filepath
            fun loop stream =
                case TextIO.inputLine stream of
                    SOME line => line :: loop stream
                  | NONE      => []
        in
            String.concat (loop stream before TextIO.closeIn stream)
        end

    fun member x nil = false
      | member x (y::ys) = (x = y) orelse member x ys
end
