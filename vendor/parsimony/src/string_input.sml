structure ParsimonyStringInput :> PARSIMONY_INPUT = struct
  datatype pos = Position of int * int

  fun posLine (Position (l, _)) = l
  fun posCol (Position (_, c)) = c

  datatype input = StringStream of string * pos
                 | EmptyStream of pos

  fun inputHead (StringStream (str, _)) = SOME (String.sub (str, 0))
    | inputHead (EmptyStream _) = NONE

  fun inputRest (StringStream (str, pos)) =
    if (String.size str = 1) then
        EmptyStream pos
    else
        let fun forwardPos (Position (l, c)) = Position (l, c+1)
            and nextLinePos (Position (l, c)) = Position(l+1, 1)
            and nextPos c = if c = #"\n" then nextLinePos else forwardPos
            and strRest str = String.extract (str, 1, NONE)
        in
            StringStream (strRest str, nextPos (String.sub (str, 0)) pos)
        end
    | inputRest (EmptyStream pos) = EmptyStream pos

  fun inputPos (StringStream (_, pos)) = pos
    | inputPos (EmptyStream pos) = pos

  fun fromString str =
    let val pos = Position (1, 1)
    in
        if String.size str = 0 then
            EmptyStream pos
        else
            StringStream (str, pos)
    end
end
