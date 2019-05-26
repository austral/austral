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

    fun isParse f input output =
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
    val parserSuite = suite "Parser" [
            suite "Integers" [
                isParse "123" (IntConstant "123"),
                isParse "0" (IntConstant "0"),
                isParse "00" (IntConstant "00"),
                isParse "10000" (IntConstant "10000"),
                isParse "10000" (IntConstant "10000"),
                isParse "-10000" (IntConstant "-10000")
            ],
            suite "Floats" [
                (*isParse "0.0" (FloatConstant "0.0"),
                isParse "-0.0" (FloatConstant "-0.0"),
                isParse "123.0" (FloatConstant "123.0"),
                isParse "-123.0" (FloatConstant "-123.0"),
                isParse "123.456" (FloatConstant "123.456"),
                isParse "-123.456" (FloatConstant "-123.456"),
                isParse "123.456e3" (FloatConstant "123.456e3"),
                isParse "-123.456e-3" (FloatConstant "-123.456e-3")*)
            ],
            suite "Strings" [
                (*isParse "\"derp\"" (StringConstant (escapeString "derp")),
                isParse "\"derp \\\"herp\\\" derp\"" (StringConstant (escapeString "derp \"herp\" derp")),
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
                isEqual' (escape "line\\\n\n\n   \\line") "lineline"*)
            ],
            suite "Symbols" [
                suite "Qualified Symbols" [
                    (*isParse "a:b" (qsym "a" "b"),
                    isParse "test:test" (qsym "test" "test")*)
                ],
                suite "Unqualified Symbols" [
                    (*isParse "test" (unsym "test")*)
                ]
            ],
            suite "Splices" [
                (*isParse ",123" (Splice (IntConstant "123"))*)
            ],
            suite "S-expressions" [
                (*isParse "()" (List nil),
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
                isParse "(123)" (List [IntConstant "123"]),
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
                ]*)
            ]
        ]
    end

    val tests = suite "Austral Tests" [
            mapSuite,
            parserSuite
        ]

    fun runTests () = runAndQuit tests defaultReporter
end

val _ = AustralTest.runTests()
