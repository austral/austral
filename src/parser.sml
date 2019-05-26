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

structure Parser :> PARSER = struct
    structure ps = Parsimony(ParsimonyStringInput)

    (* Comments *)

    val singleLineComment = ps.seqR (ps.seq (ps.pchar #"-") (ps.pchar #"-"))
                                    (ps.seqR (ps.many (ps.noneOf [#"\n"]))
                                             (ps.pchar #"\n"))

    (* Whitespace *)

    val whitespaceParser = ps.choice [ps.pchar #" ",
                                      ps.pchar #"\n",
                                      singleLineComment]

    (* Identifiers *)

    val identCharParser = ps.anyOfString Ident.alphabet

    val identParser = ps.pmap (Ident.mkIdentEx o String.implode) (ps.many1 identCharParser)

    (* Parsing declarations *)

    (* Parsing imports *)

    val importParser =
        let val fromParser = ps.seq (ps.pstring "from") whitespaceParser
            and importList = identParser
        in
            ps.seq (ps.seqR fromParser (ps.seqL identParser whitespaceParser))
                   importList
        end

    (* Interface *)

    exception ParserException of string

    fun succeedOrDie result =
        case result of
            (ps.Success (r, _)) => r
          | f => raise ParserException ("Bad parse: " ^ (ps.explain f))

    fun parseModule s =
        raise Fail "Not implemented just yet"
end
