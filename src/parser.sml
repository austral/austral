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

    val naturalParser = pmap String.implode (many1 digitParser)

    datatype sign = Positive | Negative

    val signParser = let val posParser = seqR (opt (pchar #"+")) (preturn Positive)
                         val negParser = seqR (pchar #"-") (preturn Negative)
                     in
                         or negParser posParser
                     end

    fun applySign (Positive, int) = int
      | applySign (Negative, int) = "-" ^ int

    val integerTextParser = pmap applySign (seq signParser naturalParser)

    val integerParser = pmap CST.IntConstant integerTextParser

    (* Floats *)

    val eParser = or (pchar #"e") (pchar #"E")

    val exponentParser = seqR eParser integerTextParser

    fun toFloat (intPart, (decPart, exponent)) =
        let val expStr = case exponent of
                             SOME e => "e" ^ e
                           | NONE => ""
        in
            intPart ^ "." ^ decPart ^ expStr
        end

    val floatParser = pmap (CST.FloatConstant o toFloat)
                           (seq integerTextParser (seqR (pchar #".")
                                                        (seq integerTextParser (opt exponentParser))))

    (* Strings *)

    val stringChar = or (seqR (pchar #"\\") (pchar #"\"")) (noneOf [#"\""])

    val quotedString = pmap (CST.StringConstant o CST.escapeString o String.implode)
                            (between (pchar #"\"") (many stringChar) (pchar #"\""))

    (* Symbols *)

    val symbolChar = anyOfString Ident.alphabet

    val symbolNameParser = pmap (Ident.mkIdentEx o String.implode) (many1 symbolChar)

    val unqualifiedSymbolParser = pmap CST.UnqualifiedSymbol symbolNameParser

    val qualifiedSymbolParser = pmap (CST.QualifiedSymbol o Symbol.mkSymbol)
                                     (seq symbolNameParser
                                          (seqR (pchar #":") symbolNameParser))

    val keywordParser = pmap CST.Keyword (seqR (pchar #":") symbolNameParser)

    (* Comments *)

    val singleLineComment = seqR (pchar #";")
                                 (seqR (many (noneOf [#"\n"]))
                                       (pchar #"\n"))

    (* S-expressions *)

    val whitespaceParser = choice [pchar #" ",
                                   pchar #"\n",
                                   singleLineComment]

    val ws = many whitespaceParser

    fun defineSexpParser listParser spliceParser =
        seqR ws (choice [floatParser,
                         integerParser,
                         quotedString,
                         qualifiedSymbolParser,
                         keywordParser,
                         unqualifiedSymbolParser,
                         listParser,
                         spliceParser])

    val (listParser, spliceParser) =
        let val (sexpParser, r) = wrapper ()
        in
            let val listParser = pmap CST.List (seqR (pchar #"(")
                                                     (between ws
                                                              (many (seqL sexpParser ws))
                                                              (pchar #")")))
                and spliceParser = pmap CST.Splice (seqR (pchar #",") sexpParser)
            in
                r := defineSexpParser listParser spliceParser;
                (listParser, spliceParser)
            end
        end

    val sexpParser = defineSexpParser listParser spliceParser

    (* Interface *)

    exception ParserException of string

    fun succeedOrDie result =
        case result of
            (Success (r, _)) => r
          | f => raise ParserException ("Bad parse: " ^ (explain f))

    fun parseString s =
        case (run sexpParser (ParsimonyStringInput.fromString s)) of
            (Success (r, _)) => r
          | f => raise ParserException ("Bad parse: " ^ (explain f))

    fun parseFile path =
        let val code = "(" ^ (Util.readFileToString path) ^ ")"
        in
            case (parseString code) of
                (CST.List l) => l
              | _ => raise ParserException "Failed to parse file: compiler error"
        end
end
