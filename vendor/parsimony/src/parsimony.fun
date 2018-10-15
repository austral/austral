functor Parsimony(i: PARSIMONY_INPUT): PARSIMONY = struct
  type parser_name = string
  type pos = i.pos

  datatype 'a result = Success of 'a
                     | Failure of parser_name * pos * string

  datatype 'a parser = Parser of i.input -> (('a * i.input) result)

  type input = i.input

  fun run (Parser f) i = f i

  fun explain (Success _) = "Success"
    | explain (Failure (n, p, s)) = n
                                    ^ ": parse failed at line "
                                    ^ Int.toString (i.posLine p)
                                    ^ ", column "
                                    ^ Int.toString (i.posCol p)
                                    ^ ": "
                                    ^ s

  fun preturn a =
    Parser (fn input => Success (a, input))

  fun pmap f p =
    Parser (fn input => case (run p input) of
                            (Success (v, input')) => (Success (f v, input'))
                          | (Failure f) => (Failure f))

  fun pmap2 f p p' =
    Parser (fn input => case (run p input) of
                            (Success (v, input')) => (case (run p' input') of
                                                          (Success (v', input'')) => Success (f v v', input'')
                                                        | (Failure f) => (Failure f))
                          | (Failure f) => (Failure f))

  val pfail = Parser (fn input => Failure ("fail", i.inputPos input, "Always-fail parser"))

  fun pchar char =
    Parser (fn input => let val p = i.inputPos input
                        in
                            case i.inputHead input of
                                SOME char' => if char = char' then
                                                  Success (char, i.inputRest input)
                                              else
                                                  Failure ("pchar",
                                                           p,
                                                           "Character did not match. Expected '"
                                                           ^ str char
                                                           ^ "', but got '"
                                                           ^ str char'
                                                           ^ "'.")

                              | NONE => Failure ("pchar", p, "Input is empty")
                        end)

  fun seq p1 p2 =
    Parser (fn input => case (run p1 input) of
                            (Success (v, input')) => (case (run p2 input') of
                                                          (Success (v', input'')) => (Success ((v, v'), input''))
                                                        | (Failure f) => (Failure f))
                          | (Failure f) => (Failure f))

  fun or p1 p2 =
    Parser (fn input => case (run p1 input) of
                            (Success s) => (Success s)
                          | (Failure _) => (case (run p2 input) of
                                                (Success s) => (Success s)
                                              | (Failure f) => (Failure f)))

  fun choice (a::b::rest) = or a (choice (b::rest))
    | choice [a] = a
    | choice nil = raise Fail "choice must be given a non-empty list"

  fun satisfy f =
    Parser (fn input => let val p = i.inputPos input
                        in
                            case i.inputHead input of
                                SOME c => if f c then
                                              Success (c, i.inputRest input)
                                          else
                                              Failure ("satisfy",
                                                       p,
                                                       "Character '"
                                                       ^ str c
                                                       ^ "' did not satisfy predicate.")

                              | NONE => Failure ("satisfy", p, "Input is empty")
                        end)

  fun anyOf ss = choice (map pchar ss)

  fun noneOf chars =
    satisfy (fn char => not (Option.isSome (List.find (fn c => c = char) chars)))

  fun anyOfString s = anyOf (String.explode s)

  fun noneOfString s = noneOf (String.explode s)

  fun opt p =
    Parser (fn input => case (run p input) of
                            (Success (v, input')) => (Success (SOME v, input'))
                          | (Failure f) => (Success (NONE, input)))

  fun optV p v = or p (preturn v)

  fun plist (head::tail) = let fun cons a b = a :: b
                           in
                               (pmap2 cons) head (plist tail)
                           end
    | plist nil = preturn nil

  fun pstring str = pmap implode (plist (map pchar (explode str)))

  local
      fun many' p input =
        case (run p input) of
            (Success (v, input')) => let val (l, input'') = many' p input'
                                     in
                                         (v :: l, input'')
                                     end
          | (Failure f) => (nil, input)
  in
    fun many p =
      Parser (fn input => Success (many' p input))

    fun many1 p =
      Parser (fn input => case (run p input) of
                              (Success (v, input')) => let val (l, input'') = many' p input'
                                                       in
                                                           Success (v :: l, input'')
                                                       end
                            | (Failure f) => (Failure f))
  end

  fun wrapper () =
    let val p = Parser (fn input => Failure ("wrapper", i.inputPos input, "Forward reference"))
    in
        let val pref = ref p
        in
            (Parser (fn input => run (!pref) input), pref)
        end
    end

  fun seqL p1 p2 = pmap (fn (a, b) => a) (seq p1 p2)

  fun seqR p1 p2 = pmap (fn (a, b) => b) (seq p1 p2)

  fun between p1 p2 p3 = seqR p1 (seqL p2 p3)
end
