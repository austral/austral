(*
    Copyright 2018–2019 Fernando Borretti <fernando@borretti.me>

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

structure Parser : PARSER = struct
    structure ps = Parsimony(ParsimonyStringInput)

    (* Comments *)

    val singleLineComment = ps.seqR (ps.seq (ps.pchar #"-") (ps.pchar #"-"))
                                    (ps.seqR (ps.many (ps.noneOf [#"\n"]))
                                             (ps.pchar #"\n"))

    (* Whitespace *)

    (* A single whitespace character, or a comment *)
    val whitespaceParser = ps.choice [ps.pchar #" ",
                                      ps.pchar #"\n",
                                      singleLineComment]

    (* Any positive amount of whitespace *)
    val ws1 = ps.many1 whitespaceParser
    (* Any whitespace or none at all *)
    val ws = ps.many whitespaceParser

    (* Utilities *)

    fun commaSeparatedList1 p =
        let val comma = ps.between ws (ps.pchar #",") ws
        in
            ps.pmap (fn (fst, rest) => fst :: rest)
                    (ps.seq p
                            (ps.many (ps.seqR comma (ps.seqL p ws))))
        end

    (* Identifiers *)

    val identCharParser = ps.anyOfString Ident.alphabet

    val identParser = ps.pmap (Ident.mkIdentEx o String.implode) (ps.many1 identCharParser)

    (* Parsing type specifiers *)

    val namedTypeParser = ps.pmap Syntax.NamedType identParser

    fun defineTypeSpecifierParser addressParser pointerParser tupleTypeParser =
        ps.choice [tupleTypeParser,
                   addressParser,
                   pointerParser,
                   namedTypeParser]

    val (addressParser, pointerParser, tupleTypeParser) =
        let val (typeSpecifierParser, r) = ps.wrapper ()
        in
            let val addressParser = ps.pmap Syntax.Address (ps.seqR (ps.pchar #"&") typeSpecifierParser)

                val pointerParser = ps.pmap Syntax.Pointer (ps.seqR (ps.pchar #"*") typeSpecifierParser)

                val tupleTypeParser =
                    ps.pmap Syntax.TupleType
                            (ps.between (ps.pchar #"{")
                                        (commaSeparatedList1 typeSpecifierParser)
                                        (ps.pchar #"}"))
            in
                r := defineTypeSpecifierParser addressParser pointerParser tupleTypeParser;
                (addressParser, pointerParser, tupleTypeParser)
            end
        end

    val typeSpecifierParser = defineTypeSpecifierParser addressParser pointerParser tupleTypeParser

    (* Parsing declarations *)

    (* Parsing imports *)

    val importedNameParser =
        ps.or (ps.pmap (fn (n, rn) =>
                           Syntax.ImportedNameAs { original = n,
                                                   rename = rn })
                       (ps.seq identParser
                               (ps.seqR (ps.between ws1 (ps.pstring "as") ws1)
                                        identParser)))
              (ps.pmap Syntax.ImportedName identParser)

    val importParser =
        let val from = ps.seq (ps.pstring "from") ws1
            and modName = ps.seqL identParser ws1
            and import = ps.seq (ps.pstring "import") ws1
            and importList = commaSeparatedList1 importedNameParser
        in
            let val parser = (ps.seq (ps.seqL (ps.seqR from modName) import)
                                     importList)
            in
                ps.pmap Syntax.Import parser
            end
        end

    (* Parsing expressions *)

    (* Constants *)

    (* Integer constants *)

    val digitParser = ps.anyOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]

    val naturalParser = ps.pmap String.implode (ps.many1 digitParser)

    datatype sign = Positive | Negative

    val signParser = let val posParser = ps.seqR (ps.opt (ps.pchar #"+")) (ps.preturn Positive)
                         val negParser = ps.seqR (ps.pchar #"-") (ps.preturn Negative)
                     in
                         ps.or negParser posParser
                     end

    fun applySign (Positive, int) = int
      | applySign (Negative, int) = "-" ^ int

    val integerTextParser = ps.pmap applySign (ps.seq signParser naturalParser)

    val integerParser = ps.pmap Syntax.IntConstant integerTextParser

    (* Float Constants *)

    val eParser = ps.or (ps.pchar #"e") (ps.pchar #"E")

    val exponentParser = ps.seqR eParser integerTextParser

    fun toFloat (intPart, (decPart, exponent)) =
        let val expStr = case exponent of
                             SOME e => "e" ^ e
                           | NONE => ""
        in
            intPart ^ "." ^ decPart ^ expStr
        end

    val floatParser = ps.pmap (Syntax.FloatConstant o toFloat)
                              (ps.seq integerTextParser (ps.seqR (ps.pchar #".")
                                                                 (ps.seq integerTextParser
                                                                         (ps.opt exponentParser))))

    (* Strings *)

    val stringChar = ps.or (ps.seqR (ps.pchar #"\\") (ps.pchar #"\"")) (ps.noneOf [#"\""])

    val stringParser = ps.pmap (Syntax.StringConstant o Escape.escapeString o String.implode)
                               (ps.between (ps.pchar #"\"")
                                           (ps.many stringChar)
                                           (ps.pchar #"\""))

    (* Interface *)

    exception ParserException of string

    fun succeedOrDie result =
        case result of
            (ps.Success (r, _)) => r
          | f => let val msg = "Bad parse: " ^ (ps.explain f)
                 in
                     print msg;
                     raise ParserException msg
                 end
    fun pf f s =
        succeedOrDie (ps.run f (ParsimonyStringInput.fromString s))

    val parseTypeSpecifier = pf typeSpecifierParser

    val parseInteger = pf integerParser

    val parseFloat = pf floatParser

    fun parseString s =
        succeedOrDie (ps.run stringParser (ParsimonyStringInput.fromString s))

    fun parseImport s =
        succeedOrDie (ps.run importParser (ParsimonyStringInput.fromString s))

    fun parseModule s =
        raise Fail "Not implemented just yet"
end
