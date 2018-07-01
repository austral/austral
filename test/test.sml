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

  fun isNotParse input =
    is (fn () => case (Parser.parseString input) of
                     (Util.Result _) => Fail "Parse successful, should have failed"
                   | _ => Pass)
       input

  fun isFailure value msg =
    is (fn () => case value of
                     (Util.Result v) => Fail "value is an instance of Util.Result"
                   | Util.Failure _ => Pass)
       msg

  val i = Ident.mkIdentEx

  fun unsym s = CST.UnqualifiedSymbol (i s)

  fun qsym m s = CST.QualifiedSymbol (Symbol.mkSymbol (i m, i s))

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
                    isParse "a:b" (qsym "a" "b"),
                    isParse "test:test" (qsym "test" "test")
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
                isParse "(m:a n:b o:c)" (List [qsym "m" "a", qsym "n" "b", qsym "o" "c"]),
                isParse "(a b (c d) e f)" (List [unsym "a",
                                                 unsym "b",
                                                 List [unsym "c", unsym "d"],
                                                 unsym "e",
                                                 unsym "f"]),
                isParse "(123)" (List [IntConstant 123]),
                isParse "(\"test\")" (List [StringConstant (escapeString "test")]),
                suite "Whitespace handling" [
                    isParse "   ()" (List nil),
                    isParse "()   " (List nil),
                    isParse "(   test)" (List [unsym "test"]),
                    isParse "(test   )" (List [unsym "test"]),
                    isParse "(   test   )" (List [unsym "test"]),
                    isParse "( a b c )" (List [unsym "a", unsym "b", unsym "c"])
                ],
                suite "Bad forms" [
                    isNotParse "(",
                    isNotParse ")"
                ]
            ]
        ]
  end

  fun rqsym m s = RCST.Symbol (Symbol.mkSymbol (i m, i s))

  local
    open Module
    open Map
  in
  val moduleSuite =
      let val a = Module (i "A",
                          empty,
                          Imports empty,
                          Exports (Set.add Set.empty (i "test")))
          and b = Module (i "B",
                          iadd empty (i "nick", i "A"),
                          Imports empty,
                          Exports Set.empty)
          and c = Module (i "C",
                          empty,
                          Imports (iadd empty (i "test", i "A")),
                          Exports Set.empty)
      in
          let val menv = addModule (addModule (addModule emptyEnv a) b) c
          in
              suite "Module System" [
                  isEqual (moduleName a) (i "A") "Module name",
                  suite "Symbol resolution" [
                      isEqual (RCST.resolve menv b (CST.IntConstant 10))
                              (Util.Result (RCST.IntConstant 10))
                              "Int constant",
                      isEqual (RCST.resolve menv b (CST.UnqualifiedSymbol (i "test")))
                              (Util.Result (rqsym "B" "test"))
                              "Unqualified symbol, internal",
                      isEqual (RCST.resolve menv b (qsym "nick" "test"))
                              (Util.Result (rqsym "A" "test"))
                              "Qualified symbol, nickname, exported",
                      isEqual (RCST.resolve menv b (qsym "A" "test"))
                              (Util.Result (rqsym "A" "test"))
                              "Qualified symbol, literal, exported",
                      isFailure (RCST.resolve menv b (qsym "nick" "test1"))
                                "Qualified symbol, nickname, unexported",
                      isFailure (RCST.resolve menv b (qsym "A" "test1"))
                                "Qualified symbol, literal, unexported",
                      isEqual (RCST.resolve menv c (CST.UnqualifiedSymbol (i "test")))
                              (Util.Result (rqsym "A" "test"))
                              "Unqualified symbol, imported"
                  ]
              ]
          end
      end
  end

  val tests = suite "Boreal Tests" [
          parserSuite,
          moduleSuite
      ]

  fun runTests () = runAndQuit tests defaultReporter
end

val _ = BorealTest.runTests()
