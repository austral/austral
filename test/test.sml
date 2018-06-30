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

structure BorealTest = struct
open MLUnit

  (* Test utilities *)

  structure ps = Parsimony(ParsimonyStringInput)

  fun strInput str =
    ParsimonyStringInput.fromString str

  fun isParse input output =
    is (fn () => case (Parser.parseString input) of
                     (Util.Result v) => if v = output then
                                            Pass
                                        else
                                            Fail "Parse successful, but not equal to output"
                   | Util.Failure f => Fail f)
       input

  val i = Ident.mkIdentEx

  fun unsym s = CST.UnqualifiedSymbol (i s)

  (* Test suites *)

  local
    open CST
  in
    val parserSuite = suite "Parser" [
            suite "Integers" [
                isParse "123" (IntConstant 123),
                isParse "0" (IntConstant 0),
                isParse "00" (IntConstant 0),
                isParse "10000" (IntConstant 10000),
                isParse "+10000" (IntConstant 10000),
                isParse "-10000" (IntConstant ~10000)
            ],
            suite "Strings" [
                isParse "\"derp\"" (StringConstant (escapeString "derp")),
                isParse "\"derp \\\"herp\\\" derp\"" (StringConstant (escapeString "derp \"herp\" derp"))
            ],
            suite "Symbols" [
                suite "Qualified Symbols" [
                    isParse "a:b" (QualifiedSymbol (Symbol.mkSymbol (i "a", i "b"))),
                    isParse "test:test" (QualifiedSymbol (Symbol.mkSymbol (i "test", i "test")))
                ],
                suite "Unqualified Symbols" [
                    isParse "test" (unsym "test")
                ],
                suite "Keywords" [
                    isParse ":test" (Keyword (i "test"))
                ]
            ],
            suite "S-expressions" [
                isParse "()" (List nil),
                isParse "(())" (List [List nil]),
                isParse "((()))" (List [List [List nil]]),
                isParse "(((())))" (List [List [List [List nil]]]),
                isParse "(test)" (List [unsym "test"]),
                isParse "((a))" (List [List [unsym "a"]]),
                isParse "(a b c)" (List [unsym "a", unsym "b", unsym "c"]),
                isParse "(a b (c d) e f)" (List [unsym "a",
                                                 unsym "b",
                                                 List [unsym "c", unsym "d"],
                                                 unsym "e",
                                                 unsym "f"]),
                isParse "(123)" (List [IntConstant 123]),
                isParse "(\"test\")" (List [StringConstant (escapeString "test")])
            ]
        ]
  end

  val tests = suite "Boreal Tests" [
          parserSuite
      ]

  fun runTests () = runAndQuit tests defaultReporter
end

val _ = BorealTest.runTests()
