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
