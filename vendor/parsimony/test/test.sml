structure ParsimonyTest = struct
  open MLUnit

  (* Test utilities *)

  structure ps = Parsimony(ParsimonyStringInput)

  fun strInput str = ParsimonyStringInput.fromString str

  fun isParse p str out = is (fn () => case (ps.run p (strInput str)) of
                                           (ps.Success (res, _)) => if (res = out) then Pass else Fail "Bad parse"
                                         | ps.Failure _ => Fail "Bad parse")
                             str

  fun isNotParse p str = is (fn () => case (ps.run p (strInput str)) of
                                          (ps.Success (res, _)) => Fail "Good parse"
                                        | ps.Failure _ => Pass)
                            str

  (* Test parsers *)

  val digitParser = ps.anyOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]

  fun parseInt str = case (Int.fromString str) of
                         SOME i => i
                       | NONE => raise Match

  val naturalParser = ps.pmap (parseInt o String.implode) (ps.many1 digitParser)

  datatype sign = Positive | Negative

  val signParser = let val posParser = ps.seqR (ps.opt (ps.pchar #"+")) (ps.preturn Positive)
                       val negParser = ps.seqR (ps.pchar #"-") (ps.preturn Negative)
                   in
                       ps.or negParser posParser
                   end;

  fun applySign (Positive, int) = int
    | applySign (Negative, int) = ~int

  val integerParser = ps.pmap applySign (ps.seq signParser naturalParser)

  val stringChar = ps.or (ps.seqR (ps.pchar #"\\") (ps.pchar #"\"")) (ps.noneOf [#"\""])

  val quotedString = ps.pmap String.implode (ps.between (ps.pchar #"\"") (ps.many stringChar) (ps.pchar #"\""))

  val symbolChar = ps.anyOfString "abcdefghijklmnoprstuvwxyz-0123456789"

  val symbolParser = ps.pmap String.implode (ps.many1 symbolChar)

  val whitespaceParser = ps.anyOf [#" ", #"\n"]

  val ws = ps.many whitespaceParser

  datatype sexp = Integer of int
                | String of string
                | Symbol of string
                | SList of sexp list

  fun defineSexpParser listParser =
    ps.choice [ps.pmap Integer integerParser,
               ps.pmap String quotedString,
               ps.pmap Symbol symbolParser,
               listParser]

  val listParser =
      let val (sexpParser: sexp ps.parser, r: sexp ps.parser ref) = ps.wrapper ()
      in
          let val listParser = ps.pmap SList (ps.between (ps.pchar #"(")
                                                         (ps.many (ps.seqL sexpParser ws))
                                                         (ps.pchar #")"))
          in
              r := defineSexpParser listParser;
              listParser
          end
      end

  val sexpParser = defineSexpParser listParser

  (* Tests *)

  val tests = suite "Parsimony Tests" [
          suite "Built-in parsers" [
              suite "pchar" [
                  isParse (ps.pchar #"a") "abc" #"a",
                  isNotParse (ps.pchar #"b") "abc"
              ],
              suite "or" [
                  isParse (ps.or (ps.pchar #"b") (ps.pchar #"a")) "abc" #"a"
              ],
              suite "anyOf" [
                  isParse (ps.anyOf [#"1", #"2"]) "1" #"1",
                  isParse (ps.anyOf [#"1", #"2"]) "2" #"2"
              ],
              suite "noneOf" [
                  isParse (ps.noneOf [#"A", #"B"]) "C" #"C",
                  isNotParse (ps.noneOf [#"A", #"B"]) "A",
                  isNotParse (ps.noneOf [#"A", #"B"]) "B"
              ],
              suite "plist" [
                  isParse (ps.plist [ps.pchar #"a", ps.pchar #"b"]) "ab" [#"a", #"b"]
              ],
              suite "pstring" [
                  isParse (ps.pstring "test") "test" "test"
              ],
              suite "many" [
                  isParse (ps.many (ps.pchar #"a")) "aaa" [#"a", #"a", #"a"],
                  isParse (ps.many (ps.pchar #"a")) "aab" [#"a", #"a"]
              ],
              suite "seqL" [
                  isParse (ps.seqL (ps.pchar #"a") (ps.pchar #"b")) "ab" #"a"
              ],
              suite "seqR" [
                  isParse (ps.seqR (ps.pchar #"a") (ps.pchar #"b")) "ab" #"b"
              ],
              suite "between" [
                  isParse (ps.between (ps.pchar #"a") (ps.pchar #"b") (ps.pchar #"c")) "abc" #"b"
              ]
          ],
          suite "Custom parsers" [
              suite "Integer parsing" [
                  suite "Digits" [
                      isParse digitParser "1" #"1",
                      isParse digitParser "2" #"2"
                  ],
                  suite "Naturals" [
                      isParse naturalParser "123" 123,
                      isParse naturalParser "1000" 1000
                  ],
                  suite "Sign" [
                      isParse signParser "+" Positive,
                      isParse signParser "-" Negative,
                      isParse signParser "1" Positive,
                      isParse signParser "" Positive
                  ],
                  suite "Integer" [
                      isParse integerParser "123" 123,
                      isParse integerParser "-123" ~123
                  ]
              ],
              suite "Strings" [
                  isParse quotedString "\"derp\"" "derp",
                  isParse quotedString "\"derp \\\"herp\\\" derp\"" "derp \"herp\" derp"
              ],
              suite "Symbols" [
                  isParse symbolParser "abc123" "abc123"
              ],
              suite "S-expressions" [
                  isParse sexpParser "123" (Integer 123),
                  isParse sexpParser "-123" (Integer ~123),
                  isParse sexpParser "derp" (Symbol "derp"),
                  isParse sexpParser "\"derp\"" (String "derp"),
                  isParse sexpParser "()" (SList nil),
                  isParse sexpParser "(())" (SList [SList nil]),
                  isParse sexpParser "((()))" (SList [SList [SList nil]]),
                  isParse sexpParser "(((())))" (SList [SList [SList [SList nil]]]),
                  isParse sexpParser "(derp)" (SList [Symbol "derp"]),
                  isParse sexpParser "((a))" (SList [SList [Symbol "a"]]),
                  isParse sexpParser "(a b c)" (SList [Symbol "a", Symbol "b", Symbol "c"]),
                  isParse sexpParser "(a b (c d) e f)" (SList [Symbol "a", Symbol "b",
                                                               SList [Symbol "c", Symbol "d"],
                                                               Symbol "e", Symbol "f"]),
                  isParse sexpParser "(123)" (SList [Integer 123]),
                  isParse sexpParser "(\"derp\")" (SList [String "derp"])
              ]
          ]
      ]

  fun runTests () = runAndQuit tests defaultReporter
end

val _ = ParsimonyTest.runTests ()
