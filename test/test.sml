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

structure AustralTest = struct
    open MLUnit
    (* Test utilities *)

    structure ps = Parsimony(ParsimonyStringInput)

    fun strInput str =
        ParsimonyStringInput.fromString str

    fun isParseFn f input output =
        is (fn () => let val v = f input
                     in
                         if v = output then
                             Pass
                         else
                             Fail "Parse successful, but not equal to output"
                     end
                     handle _ => Fail "Bad parse")
           input

    (*fun isNotParse input =
        is (fn () => let val v = Parser.parseString input
                     in
                       Fail "Parse successful, should have failed"
                     end
                     handle _ => Pass)
           input*)

    fun isFailure value msg =
        is (fn () => case value of
                         (Util.Result v) => Fail "value is an instance of Util.Result"
                       | Util.Failure _ => Pass)
           msg

    (* Test suites *)

    local
        open Map
    in
    val mapSuite = suite "Map" [
            isEqual (size empty) 0 "empty",
            isEqual (size (iadd empty ("A", 1))) 1 "iadd",
            isEqual (size (set empty "A" 1)) 1 "set",
            isEqual (size (set (set empty "A" 1) "A" 2)) 1 "set",
            let val m = fromList [("A", 1)]
            in
                isSome (get m "A") "get"
            end,
            let val m = fromList [("A", 1)]
            in
                let val m = set m "A" 2
                in
                    isEqual (valOf (get m "A")) 2 "get"
                end
            end
        ]
    end

    local
        open Syntax
    in
    val parserSuite =
        let val i = Name.mkIdentEx
        in
            suite "Parser" [
                suite "Type Specifiers"
                      let val isParse = isParseFn Parser.parseTypeSpecifier
                      in
                          [
                            isParse "test" (NamedType (i "test")),
                            isParse "&test" (Address (NamedType (i "test"))),
                            isParse "&&test" (Address (Address (NamedType (i "test")))),
                            isParse "&&&test" (Address (Address (Address (NamedType (i "test"))))),
                            isParse "*test" (Pointer (NamedType (i "test"))),
                            isParse "**test" (Pointer (Pointer (NamedType (i "test")))),
                            isParse "***test" (Pointer (Pointer (Pointer (NamedType (i "test"))))),
                            isParse "()" (TupleType []),
                            isParse "(a)" (TupleType [NamedType (i "a")]),
                            isParse "(a,b)" (TupleType [NamedType (i "a"),
                                                        NamedType (i "b")]),
                            isParse "(a,b,c)" (TupleType [NamedType (i "a"),
                                                          NamedType (i "b"),
                                                          NamedType (i "c")]),
                            isParse "( a,b,c )" (TupleType [NamedType (i "a"),
                                                            NamedType (i "b"),
                                                            NamedType (i "c")]),
                            isParse "( a, b, c )" (TupleType [NamedType (i "a"),
                                                              NamedType (i "b"),
                                                              NamedType (i "c")]),
                            isParse "(  a  ,  b  ,  c  )" (TupleType [NamedType (i "a"),
                                                                      NamedType (i "b"),
                                                                      NamedType (i "c")]),
                            isParse "(a,&b,*c)" (TupleType [NamedType (i "a"),
                                                            Address (NamedType (i "b")),
                                                            Pointer (NamedType (i "c"))])
                          ]
                      end,
                suite "Expressions"
                      let val isParse = isParseFn Parser.parseExpression
                      in
                          [
                            suite "Integers" [
                                isParse "123" (IntConstant "123"),
                                isParse "0" (IntConstant "0"),
                                isParse "00" (IntConstant "00"),
                                isParse "10000" (IntConstant "10000"),
                                isParse "10000" (IntConstant "10000"),
                                isParse "-10000" (IntConstant "-10000")
                            ],
                            suite "Floats" [
                                isParse "0.0" (FloatConstant "0.0"),
                                isParse "-0.0" (FloatConstant "-0.0"),
                                isParse "123.0" (FloatConstant "123.0"),
                                isParse "-123.0" (FloatConstant "-123.0"),
                                isParse "123.456" (FloatConstant "123.456"),
                                isParse "-123.456" (FloatConstant "-123.456"),
                                isParse "123.456e3" (FloatConstant "123.456e3"),
                                isParse "-123.456e-3" (FloatConstant "-123.456e-3")
                            ],
                            suite "Strings"
                                  let fun escape s = Escape.escapedToString (Escape.escapeString s)
                                  in
                                      [
                                        isParse "\"derp\"" (StringConstant (Escape.escapeString "derp")),
                                        isParse "\"derp \\\"herp\\\" derp\"" (StringConstant (Escape.escapeString "derp \"herp\" derp")),
                                        isEqual' (escape "line\\nline") "line\nline",
                                        isEqual' (escape "line\\rline") "line\rline",
                                        isEqual' (escape "line\\tline") "line\tline",
                                        isEqual' (escape "line\\\\line") "line\\line",
                                        isEqual' (escape "line\\ \\line") "lineline",
                                        isEqual' (escape "line\\  \\line") "lineline",
                                        isEqual' (escape "line\\   \\line") "lineline",
                                        isEqual' (escape "line\\    \\line") "lineline",
                                        isEqual' (escape "line\\\n\\line") "lineline",
                                        isEqual' (escape "line\\\n \n\\line") "lineline",
                                        isEqual' (escape "line\\\n\n\n\\line") "lineline",
                                        isEqual' (escape "line\\\n\n\n   \\line") "lineline"
                                      ]
                                  end,
                            suite "Arithmetic" [
                                isParse "1 + 1" (ArithOp (Arith.Modular, Arith.Add, IntConstant "1", IntConstant "1"))
                            ],
                            suite "Comparisons" [
                                isParse "1 < 2" (CompOp (LessThan, IntConstant "1", IntConstant "2")),
                                isParse "x > y" (CompOp (GreaterThan, Variable (i "x"), Variable (i "y")))

                            ],
                            suite "Logical" [
                                isParse "not true" (Not (BoolConstant true)),
                                isParse "true or false" (Or (BoolConstant true, BoolConstant false)),
                                isParse "true and false" (And (BoolConstant true, BoolConstant false))
                            ],
                            suite "Tuple" [
                                isParse "(1,2,3)" (TupleCreate [IntConstant "1", IntConstant "2", IntConstant "3"])
                            ],
                            suite "Block" [
                                isParse "{ 1 }" (Block [IntConstant "1"]),
                                isParse "{ 1; 2 }" (Block [IntConstant "1", IntConstant "2"]),
                                isParse "{ 1; 2; a }" (Block [IntConstant "1", IntConstant "2", Variable (i "a")])
                            ],
                            suite "Function Call" [
                                isParse "f(1,2,3)" (Funcall (i "f",
                                                             [
                                                               IntConstant "1",
                                                               IntConstant "2",
                                                               IntConstant "3"
                                                             ])),
                                isParse "f()" (Funcall (i "f", []))
                            ],
                            suite "Let" [
                                isParse "let x = 1 in 1"
                                        (Let ([VarBinding (i "x", IntConstant "1")],
                                              IntConstant "1"))
                            ]
                          ]
                      end,
                suite "Declarations" [
                    suite "Imports"
                          let val isParse = isParseFn Parser.parseImport
                              and mn = Name.mkModuleNameEx
                          in
                              [
                                isParse "from a import b"
                                        (Import (mn "a",
                                                 [ImportedName (i "b")])),
                                isParse "from   a   import   b    "
                                        (Import (mn "a",
                                                 [ImportedName (i "b")])),
                                isParse "from a import b,c, d, e ,  f  "
                                        (Import (mn "a",
                                                 [ImportedName (i "b"),
                                                  ImportedName (i "c"),
                                                  ImportedName (i "d"),
                                                  ImportedName (i "e"),
                                                  ImportedName (i "f")])),
                                isParse "from    a   import    b,c, d, e ,  f  "
                                        (Import (mn "a",
                                                 [ImportedName (i "b"),
                                                  ImportedName (i "c"),
                                                  ImportedName (i "d"),
                                                  ImportedName (i "e"),
                                                  ImportedName (i "f")])),
                                isParse "from mod import a as b, c as d"
                                        (Import (mn "mod",
                                                 [ImportedNameAs { original = i "a", rename = i "b" },
                                                  ImportedNameAs { original = i "c", rename = i "d" }])),
                                isParse "from mod import name, a as b, c as d, name2"
                                        (Import (mn "mod",
                                                 [ImportedName (i "name"),
                                                  ImportedNameAs { original = i "a", rename = i "b" },
                                                  ImportedNameAs { original = i "c", rename = i "d" },
                                                  ImportedName (i "name2")]))
                              ]
                          end
                ],
                suite "Comments"
                      let val isParse = isParseFn Parser.parseTypeSpecifier
                      in
                          [
                            isParse "test ;; comment" (NamedType (i "test")),
                            isParse "(a, -- comment\n b)"
                                    (TupleType [NamedType (i "a"),
                                                NamedType (i "b")])
                          ]
                      end
            ]
        end
    end

    val tests = suite "Austral Tests" [
            mapSuite,
            parserSuite
        ]

    fun runTests () = runAndQuit tests defaultReporter
end

val _ = AustralTest.runTests()
