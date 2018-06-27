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

structure Parser :> PARSER = struct
  structure ps = Parsimony(ParsimonyStringInput)
  open ps

  (* Integers *)

  val digitParser = anyOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]

  fun parseInt str = case (Int.fromString str) of
                         SOME i => i
                       | NONE => raise Match

  val naturalParser = pmap (parseInt o String.implode) (many1 digitParser)

  datatype sign = Positive | Negative

  val signParser = let val posParser = seqR (opt (pchar #"+")) (preturn Positive)
                       val negParser = seqR (pchar #"-") (preturn Negative)
                   in
                       or negParser posParser
                   end

  fun applySign (Positive, int) = int
    | applySign (Negative, int) = ~int

  val integerParser = pmap (CST.IntConstant o applySign) (seq signParser naturalParser)

  (* Strings *)

  val stringChar = or (seqR (pchar #"\\") (pchar #"\"")) (noneOf [#"\""])

  val quotedString = pmap (CST.StringConstant o CST.escapeString o String.implode)
                          (between (pchar #"\"") (many stringChar) (pchar #"\""))

  (* Symbols *)

  val symbolChar = anyOfString Ident.alphabet

  val symbolNameParser = pmap String.implode (many1 symbolChar)
end
