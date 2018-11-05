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

    fun valOf (Result v) = v
      | valOf (Failure f) = raise Fail ("Result.Failure: " ^ f)

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

    fun writeStringToFile filepath str =
        let val stream = TextIO.openOut filepath
        in
            TextIO.output (stream, str) handle e => (TextIO.closeOut stream; raise e);
            TextIO.closeOut stream
        end

    fun member x nil = false
      | member x (y::ys) = (x = y) orelse member x ys

    fun position elem list =
        let fun index' nil _ = NONE
              | index' (head::tail) p = if head = elem then
                                            SOME p else
                                        index' tail (p+1)
        in
            index' list 0
        end

    fun mapidx f list =
        let val indices = List.tabulate (List.length list, fn x => x)
        in
            ListPair.map f (list, indices)
        end

    fun butlast (x::nil) =
        nil
      | butlast (first::rest) =
        first :: (butlast rest)
      | butlast nil =
        raise Fail "butlast called with an empty list"

    type prefix = string

    fun afterPrefix string prefix =
        if (String.isPrefix prefix string) then
            SOME (String.extract (string,
                                  (String.size prefix),
                                  NONE))
        else
            NONE

    fun foldThread f (head::tail) ctx =
        let val (head', ctx') = f (head, ctx)
        in
            let val (rest, ctx'') = foldThread f tail ctx'
            in
                (head' :: rest, ctx'')
            end
        end
      | foldThread f nil ctx =
        (nil, ctx)
end
