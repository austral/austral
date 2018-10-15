structure ps = Parsimony(ParsimonyStringInput)

(* Data *)

datatype exp = Constant of int
             | Addition of exp * exp
             | Subtraction of exp * exp

(* Parsers *)

val digitParser = ps.anyOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]

fun parseInt str = case (Int.fromString str) of
                       SOME i => i
                     | NONE => raise Match

val naturalParser = ps.pmap (Constant o parseInt o String.implode) (ps.many1 digitParser)

fun operands c p = ps.pmap (fn (a,(b,c)) => (a, c)) (ps.seq p (ps.seq (ps.pchar c) p))

fun defineTermParser p =
    ps.choice [ps.between (ps.pchar #"(") p (ps.pchar #")"),
               naturalParser]

val expressionParser =
    let val (p, r) = ps.wrapper ()
    in
        let val expressionParser =
                ps.choice [ps.pmap Addition (operands #"+" p),
                           ps.pmap Subtraction (operands #"-" p),
                           naturalParser]
        in
            r := defineTermParser expressionParser;
            expressionParser
        end
    end

fun str s = ParsimonyStringInput.fromString s;

fun r s = ps.run expressionParser (str s);

r "1";

r "1+1";

r "1-1";

r "1+(2-3)";
