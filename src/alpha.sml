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

structure Alpha :> ALPHA = struct
    (* Utils *)

    fun au name =
        Symbol.mkSymbol (Ident.mkIdentEx "austral",
                         Ident.mkIdentEx name)

    (* AST *)

    datatype ast = IntConstant of string
                 | FloatConstant of string
                 | StringConstant of CST.escaped_string
                 | Variable of Symbol.variable
                 | Let of Symbol.variable * ast * ast
                 | The of Type.typespec * ast
                 | Progn of ast list
                 | Operation of Symbol.symbol * ast list

    (* Fresh variables *)

    val count = ref 0

    fun freshVar sym =
        let
        in
            count := !count + 1;
            Symbol.Var (sym, !count)
        end

    fun resetCount () =
        count := 0

    (* The stack *)

    type stack = (Symbol.symbol * Symbol.variable) list

    fun lookup ((n,v)::xs) s = if (n = s) then
                                   v
                               else
                                   lookup xs s
      | lookup nil s = raise Fail ("No such variable: '"
                                   ^ (Ident.identString (Symbol.symbolName s))
                                   ^ "'")

    (* Alpha renaming *)

    fun alphaRename _ (OAST.IntConstant s) =
        IntConstant s
      | alphaRename _ (OAST.FloatConstant f) =
        FloatConstant f
      | alphaRename _ (OAST.StringConstant s) =
        StringConstant s
      | alphaRename s (OAST.Symbol name) =
        Variable (lookup s name)
      | alphaRename s (OAST.Let (var, value, body)) =
        let val fresh = freshVar var
          in
              let val s' = (var, fresh) :: s
              in
                  let val body' = alphaRename s' body
                  in
                      Let (fresh, alphaRename s value, body')
                  end
              end
        end
      | alphaRename s (OAST.The (ty, exp)) =
        The (ty, alphaRename s exp)
      | alphaRename s (OAST.Operation (f, args)) =
        let val args' = map (alphaRename s) args
        in
            if f = au "progn" then
                Progn args'
            else
                Operation (f, args')
        end

    fun transform oast =
        let
        in
            resetCount ();
            alphaRename [] oast
        end
end
