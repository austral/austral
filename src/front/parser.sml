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

    fun separatedList p sp =
        let val separator = ps.between ws sp ws
        in
            ps.pmap (fn (fst, rest) => fst :: rest)
                    (ps.seq p
                            (ps.many (ps.seqR separator (ps.seqL p ws))))
        end

    fun commaSeparatedList1 p =
        separatedList p (ps.pchar #",")

    fun commaSeparatedList0 p =
        ps.pmap (fn v =>
                    case v of
                        SOME l => l
                      | NONE => [])
                (ps.opt (commaSeparatedList1 p))

    (* Identifiers *)

    val identCharParser = ps.anyOfString Name.identAlphabet

    val identParser = ps.pmap (Name.mkIdentEx o String.implode) (ps.many1 identCharParser)

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
            let val addressParser =
                    ps.pmap Syntax.Address (ps.seqR (ps.pchar #"&") typeSpecifierParser)

                val pointerParser =
                    ps.pmap Syntax.Pointer (ps.seqR (ps.pchar #"*") typeSpecifierParser)

                val tupleTypeParser =
                    ps.pmap Syntax.TupleType
                            (ps.between (ps.seq (ps.pchar #"(") ws)
                                        (commaSeparatedList0 typeSpecifierParser)
                                        (ps.seq (ps.pchar #")") ws))
            in
                r := defineTypeSpecifierParser addressParser pointerParser tupleTypeParser;
                (addressParser, pointerParser, tupleTypeParser)
            end
        end

    val typeSpecifierParser = defineTypeSpecifierParser addressParser pointerParser tupleTypeParser

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

    (* Documentation strings *)

    val docstringChar = ps.or (ps.seqR (ps.pchar #"\\") (ps.pchar #"`")) (ps.noneOf [#"`"])

    val docstringTextParser = ps.pmap String.implode
                                      (ps.between (ps.pchar #"`")
                                                  (ps.many docstringChar)
                                                  (ps.pchar #"`"))

    val docstringParserL = (* When the docstring is followed by whitespace *)
        ps.pmap Syntax.Docstring
                (ps.opt (ps.seqL docstringTextParser ws1))

    val docstringParserR = (* When the docstring is preceded by whitespace *)
        ps.pmap Syntax.Docstring
                (ps.opt (ps.seqR ws1 docstringTextParser))

    (* Expressions *)

    fun defineExpressionParser parsers =
        ps.choice parsers

    val expParsers =
        let val (expressionParser, r) = ps.wrapper ()
        in
            let val unitConstantParser =
                    ps.seqR (ps.seq (ps.pchar #"(")
                                    (ps.pchar #")"))
                            (ps.preturn Syntax.UnitConstant)

                and boolConstantParser =
                    ps.or (ps.seqR (ps.pstring "true")
                                   (ps.preturn (Syntax.BoolConstant true)))
                          (ps.seqR (ps.pstring "false")
                                   (ps.preturn (Syntax.BoolConstant false)))

                and variableParser =
                    ps.pmap Syntax.Variable identParser

                and letParser =
                    let val letP = ps.seq (ps.pstring "let")
                                          ws1

                        and varBindP =
                            ps.pmap (fn (i, e) =>
                                        Syntax.VarBinding (i, e))
                                    (ps.seq identParser
                                            (ps.seqR ws1
                                                     (ps.seqR (ps.pchar #"=")
                                                              (ps.seqR ws1
                                                                       expressionParser))))

                        and inP = (ps.seq (ps.pstring "in")
                                          ws1)
                    in
                        let val bindP = varBindP
                        in
                            let val bindListP = (ps.seqL (commaSeparatedList1 bindP)
                                                         ws1)
                            in
                                ps.pmap (fn (binds, e) =>
                                            Syntax.Let (binds, e))
                                        (ps.seqR letP
                                                 (ps.seq bindListP
                                                         (ps.seqR inP
                                                                  expressionParser)))
                            end
                        end
                    end

                and ifParser =
                    let val If =
                            ps.seq (ps.pstring "if")
                                   ws1
                        and Then =
                            ps.seq (ps.pstring "then")
                                   ws1
                        and Else =
                            ps.seq (ps.pstring "else")
                                   ws1
                    in
                        let val ifp = ps.seq (ps.seqR If
                                                      expressionParser)
                                             (ps.seq (ps.seqR ws1
                                                              (ps.seqR Then
                                                                       expressionParser))
                                                     (ps.seqR ws1
                                                              (ps.seqR Else
                                                                       expressionParser)))
                        in
                            ps.pmap (fn (t, (c, a)) => Syntax.If (t, c, a))
                                    ifp
                        end
                    end

                and tupleParser =
                    ps.pmap (fn exps =>
                                Syntax.TupleCreate exps)
                            (ps.between (ps.pchar #"(")
                                        (commaSeparatedList0 expressionParser)
                                        (ps.pchar #")"))

                and notParser =
                    ps.pmap (fn e => Syntax.Not e)
                            (ps.seqR (ps.pstring "not")
                                     (ps.seqR ws1
                                              expressionParser))

                and blockParser =
                    ps.pmap (fn exps => Syntax.Block exps)
                            (ps.between (ps.seq (ps.pchar #"{")
                                                ws1)
                                        (separatedList expressionParser
                                                       (ps.seq (ps.pchar #";")
                                                               ws1))
                                        (ps.seq ws
                                                (ps.pchar #"}")))

                and functionCallParser =
                    ps.pmap (fn (name, args) =>
                                Syntax.Funcall (name, args))
                            (ps.seq identParser
                                    (ps.between (ps.pchar #"(")
                                                (commaSeparatedList0 expressionParser)
                                                (ps.pchar #")")))
            in
                let val (orParser, andParser) =
                        let val termParser = ps.choice [boolConstantParser,
                                                        variableParser,
                                                        ps.between (ps.pchar #"(")
                                                                   expressionParser
                                                                   (ps.pchar #")")]
                        in
                            let fun mkparser sep cons =
                                    ps.pmap (fn (lhs, rhs) =>
                                                cons (lhs, rhs))
                                            (ps.seq termParser
                                                    (ps.seqR ws1
                                                             (ps.seqR (ps.pstring sep)
                                                                      (ps.seqR ws1
                                                                               termParser))))
                            in
                                let val orParser = mkparser "or" Syntax.Or
                                    and andParser = mkparser "and" Syntax.And
                                in
                                    (orParser, andParser)
                                end
                            end
                        end

                    and comparisonParser =
                        let val compOpParser =
                                ps.choice [ps.seqR (ps.pstring "=") (ps.preturn Syntax.EqualTo),
                                           ps.seqR (ps.pstring "<>") (ps.preturn Syntax.NotEqualTo),
                                           ps.seqR (ps.pstring ">") (ps.preturn Syntax.GreaterThan),
                                           ps.seqR (ps.pstring "<") (ps.preturn Syntax.LessThan),
                                           ps.seqR (ps.pstring ">=") (ps.preturn Syntax.GreaterThanEq),
                                           ps.seqR (ps.pstring "<=") (ps.preturn Syntax.LessThanEq)]

                            and termParser = ps.choice [integerParser,
                                                        floatParser,
                                                        variableParser,
                                                        ps.between (ps.pchar #"(")
                                                                   expressionParser
                                                                   (ps.pchar #")")]
                        in
                            ps.pmap (fn (lhs, (oper, rhs)) =>
                                        Syntax.CompOp (oper, lhs, rhs))
                                    (ps.seq termParser
                                            (ps.seqR ws1
                                                     (ps.seq compOpParser
                                                             (ps.seqR ws1
                                                                      termParser))))
                        end

                    and arithParser =
                        let val termSepP =
                                ps.between ws1
                                           (ps.choice [ps.seqR (ps.pchar #"+") (ps.preturn (Arith.Add, Arith.Modular)),
                                                       ps.seqR (ps.pchar #"-") (ps.preturn (Arith.Sub, Arith.Modular)),
                                                       ps.seqR (ps.pchar #"*") (ps.preturn (Arith.Mul, Arith.Modular)),
                                                       ps.seqR (ps.pchar #"/") (ps.preturn (Arith.Div, Arith.Modular))])
                                           ws1

                            and termParser = ps.choice [integerParser,
                                                        floatParser,
                                                        variableParser,
                                                        ps.between (ps.pchar #"(")
                                                                   expressionParser
                                                                   (ps.pchar #")")]
                        in
                            ps.pmap (fn (lhs, ((oper, kind), rhs)) =>
                                        Syntax.ArithOp (kind, oper, lhs, rhs))
                                    (ps.seq termParser
                                            (ps.seq termSepP
                                                    termParser))
                        end
                in
                    let val expParsers = [
                            (* Compound expressions *)
                            letParser,
                            ifParser,
                            orParser,
                            andParser,
                            notParser,
                            tupleParser,
                            comparisonParser,
                            arithParser,
                            blockParser,
                            functionCallParser,
                            (* Constants *)
                            floatParser,
                            integerParser,
                            stringParser,
                            unitConstantParser,
                            boolConstantParser,
                            variableParser
                        ]
                    in
                        r := defineExpressionParser expParsers;
                        expParsers
                    end
                end
            end
        end

    val expressionParser = defineExpressionParser expParsers

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

    val importListParser =
        separatedList importParser ws1

    (* Visibility declarations *)

    val typeVisibilityParser =
        ps.choice [ps.seqR (ps.pstring "public") (ps.preturn Syntax.PublicType),
                   ps.seqR (ps.pstring "opaque") (ps.preturn Syntax.OpaqueType),
                   ps.seqR (ps.pstring "private") (ps.preturn Syntax.PrivateType),
                   ps.preturn Syntax.PrivateType]

    val functionVisibilityParser =
        ps.choice [ps.seqR (ps.pstring "public") (ps.preturn Syntax.PublicFunction),
                   ps.seqR (ps.pstring "private") (ps.preturn Syntax.PrivateFunction),
                   ps.preturn Syntax.PrivateFunction]

    (* Type definitions *)

    fun recordLikeDefinitionParser constructor slotCons label typeParser =
        let val slotParser =
                ps.pmap (fn (n, (t, ds)) =>
                            slotCons (n, t, ds))
                        (ps.seq identParser
                                (ps.seq typeParser
                                        docstringParserR))
        in
            ps.pmap (fn (ds, (name, slots)) =>
                        constructor (ds, name, slots))
                    (ps.seq docstringParserL
                            (ps.seqR (ps.pstring label)
                                     (ps.seq identParser
                                             (ps.seqR ws1
                                                      (ps.seqR ws1
                                                               (ps.between (ps.pchar #"{")
                                                                           (commaSeparatedList0 slotParser)
                                                                           (ps.pchar #"}")))))))
        end

    val recordDefinitionParser =
        recordLikeDefinitionParser Syntax.RecordDefinition
                                   Syntax.SlotDefinition
                                   "record"
                                   (ps.seqR (ps.pchar #":")
                                            (ps.seqR ws1
                                                     typeSpecifierParser))

    val unionDefinitionParser =
        recordLikeDefinitionParser Syntax.UnionDefinition
                                   Syntax.CaseDefinition
                                   "union"
                                   (ps.opt
                                        (ps.seqR (ps.pchar #":")
                                                 (ps.seqR ws1
                                                          typeSpecifierParser)))

    (* Function definitions *)

    val functionDefinitionParser =
        let val paramListParser =
                ps.between (ps.pchar #"(")

                           (ps.pchar #")")
        in
            ps.seq docstringParserL
                   (ps.seqR (ps.pstring "function")
                            (ps.seqR ws1
                                     (ps.seqR identParser
                                              paramListParser)))
        end

    (* Modules *)

    val moduleNameParser =
        let val modNameCharParser = ps.anyOfString Name.moduleNameAlphabet
        in
            ps.pmap (String.implode) (ps.many1 modNameCharParser)
        end

    val moduleDeclarationParser =
        ps.seqR (ps.pstring "module")
                (ps.seqR ws1
                         moduleNameParser)

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

    val parseExpression = pf expressionParser

    val parseImport = pf importParser

    fun parseModule s =
        raise Fail "Not implemented just yet"
end
