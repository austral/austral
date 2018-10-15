# Parsimony

**Parsimony** is a parser combinator library for Standard ML.

It features:

- Pluggable inputs.
- Source location tracking.
- Extensive example code.

# Example Session

In lieu of a 'Hello, World!' example, here is an example of using Parsimony in
an interactive SML/NJ session (output elided/edited as needed):

~~~sml
> sml -m parsimony.cm

- structure ps = Parsimony(ParsimonyStringInput);
structure ps : PARSIMONY

- fun str s = ParsimonyStringInput.fromString s;
val str = fn : string -> ParsimonyStringInput.input

- ps.run (ps.pchar #"a") (str "abc");
val it = Success (#"a",-) : (char * ps.input) ps.result

- ps.run (ps.or (ps.pchar #"b") (ps.pchar #"a")) (str "abc");
val it = Success (#"a",-) : (char * ps.input) ps.result

- val digitParser = ps.anyOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"];
val digitParser = Parser fn : char ps.parser

- ps.run digitParser (str "123");
val it = Success (#"1",-) : (char * ps.input) ps.result
~~~

# Examples

Run `make examples` to load these into an interactive SML/NJ session.

## Natural Numbers

~~~sml
structure ps = Parsimony(ParsimonyStringInput)

val digitParser = ps.anyOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]

fun parseInt str = case (Int.fromString str) of
                       SOME i => i
                     | NONE => raise Match

val naturalParser = ps.pmap (parseInt o String.implode) (ps.many1 digitParser)

ps.run naturalParser (ParsimonyStringInput.fromString "123")
(* val it = Success (123,-) *)
~~~

## Integers

Continuing from the previous example:

~~~sml
datatype sign = Positive | Negative

val signParser = let val posParser = ps.seqR (ps.opt (ps.pchar #"+")) (ps.preturn Positive)
                     val negParser = ps.seqR (ps.pchar #"-") (ps.preturn Negative)
                 in
                     ps.or negParser posParser
                 end;

fun applySign (Positive, int) = int
  | applySign (Negative, int) = ~int

val integerParser = ps.pmap applySign (ps.seq signParser naturalParser)

ps.run integerParser (ParsimonyStringInput.fromString "-123")
(* val it = Success (~123,-) *)
~~~

## Quoted Strings

~~~sml
structure ps = Parsimony(ParsimonyStringInput)

val stringChar = ps.or (ps.seqR (ps.pchar #"\\") (ps.pchar #"\"")) (ps.noneOf [#"\""])

val quotedString = ps.pmap String.implode (ps.between (ps.pchar #"\"") (ps.many stringChar) (ps.pchar #"\""))

ps.run quotedString (ParsimonyStringInput.fromString "\"test\"")
(* val it = Success ("test",-) *)

ps.run quotedString (ParsimonyStringInput.fromString "\"test \\\"inner\\\" test\"")
(* val it = Success ("test \"inner\" test",-) *)
~~~

# API Reference

## The `PARSIMONY_INPUT` Signature

### Synopsis

~~~sml
signature PARSIMONY_INPUT = sig
  type pos

  val posLine : pos -> int
  val posCol : pos -> int

  type input

  val inputHead : input -> char option
  val inputRest : input -> input
  val inputPos : input -> pos

  val fromString : string -> input
end
~~~

### Type `pos`

Values of this type represent a position in the input.

### Function `posLine`

Given a position, return its line number. Lines are counted starting at 1.

### Function `posCol`

Given a position, return its column number. Columns are counted starting at 1.

### Type `input`

Values of this type represent an input stream with position tracking.

### Function `inputHead`

Return the first character of the input, or `NONE` if the input is empty.

### Function `inputRest`

Return a new input that contains the all but the current head character.

### Function `inputPos`

Return the current position in the input.

### Function `fromString`

Given a string, return an `input` value.

## The `ParsimonyStringInput` Structure

The `ParsimonyStringInput` structure implements the `PARSIMONY_INPUT` signature
for string input streams.

## The `PARSIMONY` Signature

### Synopsis

~~~sml
signature PARSIMONY = sig
  type parser_name
  type pos

  datatype 'a result = Success of 'a
                     | Failure of parser_name * pos * string

  type 'a parser

  type input

  val run : 'a parser -> input -> ('a * input) result
  val explain : 'a result -> string

  val preturn : 'a -> 'a parser
  val pmap : ('a -> 'b) -> 'a parser -> 'b parser
  val pmap2 : ('a -> 'b -> 'c) -> 'a parser -> 'b parser -> 'c parser

  val pfail : 'a parser
  val pchar : char -> char parser
  val seq : 'a parser -> 'b parser -> ('a * 'b) parser
  val or : 'a parser -> 'a parser -> 'a parser

  val choice : 'a parser list -> 'a parser
  val satisfy : (char -> bool) -> char parser

  val anyOf : char list -> char parser
  val noneOf : char list -> char parser
  val anyOfString : string -> char parser
  val noneOfString : string -> char parser

  val opt : 'a parser -> 'a option parser
  val optV : 'a parser -> 'a -> 'a parser

  val plist : 'a parser list -> 'a list parser
  val pstring : string -> string parser

  val many : 'a parser -> 'a list parser
  val many1 : 'a parser -> 'a list parser

  val wrapper : unit -> ('a parser * 'a parser ref)
  val seqL : 'a parser -> 'b parser -> 'a parser
  val seqR : 'a parser -> 'b parser -> 'b parser
  val between : 'a parser -> 'b parser -> 'c parser -> 'b parser
end
~~~

### Function `run`

Given a parser and an input stream, run the parser on it and return a result.

### Function `explain`

Given a result value, return a human-readable string explaining it. In the case
of a `Failure` result, this prints an appropriate error message with position
information. Useful for error reporting.

### Constant `pfail`

A parser that always fails.

### Function `pchar`

Given a character, returns a parser for that specific character.

### Function `seq`

Given two parsers, returns a parser that parsers them in sequence, and returns
their results as a pair.

### Function `or`

Given two parsers, returns a parser which tries to run the first, and returns
its value if successful. Otherwise, it runs the second parser.

### Function `choice`

Given a list of parsers, returns a parser which runs each parser from the list
from left to right until one succeeds, returning its result.

### Function `anyOf`

Return a parser that matches any character in the list.

### Function `anyOfString`

Like `anyOf`, but a string is used instead of a list of characters for brevity.

### Function `opt`

Try to run the given parser, if it succeds, return `SOME x`, otherwise, return
`NONE`.

### Function `optV`

Like `opt`, but instead of using an `option` value, you provide a default value
to use in the failure case.

### Function `seqL`

Like `seq`, but only returns the result of the first parser.

### Function `seqR`

Like `seq`, but only returns the result of the second parser.

### Function `between`

Given three parsers, it parses them all in sequence, but returns the value in
the middle. Useful, for instance, to parse a value wrapped in parentheses.

## The `Parsimony` Functor

The `Parsimony` functor takes a structure implementing the `PARSIMONY_INPUT`
signature and returns a structure containing Parsimony's parsers specialized for
the input structure.

### Examples

~~~sml
val p = Parsimony(ParsimonyStringInput)
~~~

# License

Copyright (c) 2018 Fernando Borretti

Licensed under the MIT License.
