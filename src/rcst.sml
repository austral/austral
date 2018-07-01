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

structure RCST :> RCST = struct
  datatype rcst = IntConstant of int
                | StringConstant of CST.escaped_string
                | Symbol of Symbol.symbol
                | Keyword of Symbol.symbol_name
                | List of rcst list

  fun resolve _ _ (CST.IntConstant i) = IntConstant i
    | resolve _ _ (CST.StringConstant es) = StringConstant es
    | resolve menv m (CST.QualifiedSymbol s) = resolveQualified menv
                                                                m
                                                                (Symbol.symbolModuleName s)
                                                                (Symbol.symbolName s)
    | resolve menv m (CST.UnqualifiedSymbol s) = resolveUnqualified menv m s
    | resolve _ _ (CST.Keyword n) = Keyword n
    | resolve menv m (CST.List l) = List (map (fn e => resolve menv m e) l)
  and resolveQualified menv module (mn: Symbol.module_name) (sn: Symbol.symbol_name) =
      let val truename = Module.resolveNickname module mn
      in
          case (Module.menvGet menv truename) of
              SOME formod => Symbol (Symbol.mkSymbol (Module.moduleName formod, sn))
            | NONE => raise Fail ("No module named " ^ (Ident.identString truename))
      end
  and resolveUnqualified _ m (s: Symbol.symbol_name) =
      Symbol (Symbol.mkSymbol (Module.sourceModule m s, s))
end
