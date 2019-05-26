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
        let val i = Ident.mkIdentEx
        in
            suite "Parser" [
                suite "Type Specifiers"
                      let val isParse = isParseFn Parser.parseTypeSpecifier
                      in
                          [
                            isParse "test" (NamedType (i "test")),
                            isParse "&test" (Address (NamedType (i "test"))),
                            isParse "*test" (Pointer (NamedType (i "test"))),
                            isParse "{}" (TupleType []),
                            isParse "{a}" (TupleType [NamedType (i "a")]),
                            isParse "{a,b}" (TupleType [NamedType (i "a"),
                                                        NamedType (i "b")]),
                            isParse "{a,b,c}" (TupleType [NamedType (i "a"),
                                                          NamedType (i "b"),
                                                          NamedType (i "c")]),
                            isParse "{ a,b,c }" (TupleType [NamedType (i "a"),
                                                            NamedType (i "b"),
                                                            NamedType (i "c")])
                          ]
                      end,
                suite "Expressions" [
                    suite "Integers"
                          let val isParse = isParseFn Parser.parseInteger
                          in
                              [
                                isParse "123" (IntConstant "123"),
                                isParse "0" (IntConstant "0"),
                                isParse "00" (IntConstant "00"),
                                isParse "10000" (IntConstant "10000"),
                                isParse "10000" (IntConstant "10000"),
                                isParse "-10000" (IntConstant "-10000")
                              ]
                          end,
                    suite "Floats"
                          let val isParse = isParseFn Parser.parseFloat
                          in
                              [
                                isParse "0.0" (FloatConstant "0.0"),
                                isParse "-0.0" (FloatConstant "-0.0"),
                                isParse "123.0" (FloatConstant "123.0"),
                                isParse "-123.0" (FloatConstant "-123.0"),
                                isParse "123.456" (FloatConstant "123.456"),
                                isParse "-123.456" (FloatConstant "-123.456"),
                                isParse "123.456e3" (FloatConstant "123.456e3"),
                                isParse "-123.456e-3" (FloatConstant "-123.456e-3")
                              ]
                          end,
                    suite "Strings"
                          let val isParse = isParseFn Parser.parseString
                          in
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
                              end
                          end,
                    suite "Symbols" [
                        suite "Qualified Symbols" [
                            (*isParse "a:b" (qsym "a" "b"),
                    isParse "test:test" (qsym "test" "test")*)
                        ],
                        suite "Unqualified Symbols" [
                            (*isParse "test" (unsym "test")*)
                        ]
                    ]
                ],
                suite "Declarations" [
                    suite "Imports"
                          let val isParse = isParseFn Parser.parseImport
                          in
                              [
                                isParse "from a import b"
                                        (Import (i "a",
                                                 [ImportedName (i "b")])),
                                isParse "from   a   import   b    "
                                        (Import (i "a",
                                                 [ImportedName (i "b")])),
                                isParse "from a import b,c, d, e ,  f  "
                                        (Import (i "a",
                                                 [ImportedName (i "b"),
                                                  ImportedName (i "c"),
                                                  ImportedName (i "d"),
                                                  ImportedName (i "e"),
                                                  ImportedName (i "f")])),
                                isParse "from mod import a as b, c as d"
                                        (Import (i "mod",
                                                 [ImportedNameAs { original = i "a", rename = i "b" },
                                                  ImportedNameAs { original = i "c", rename = i "d" }])),
                                isParse "from mod import name, a as b, c as d, name2"
                                        (Import (i "mod",
                                                 [ImportedName (i "name"),
                                                  ImportedNameAs { original = i "a", rename = i "b" },
                                                  ImportedNameAs { original = i "c", rename = i "d" },
                                                  ImportedName (i "name2")]))
                              ]
                          end
                ]
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
