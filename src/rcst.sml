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
    datatype rcst = IntConstant of string
                  | FloatConstant of string
                  | StringConstant of CST.escaped_string
                  | Symbol of Symbol.symbol
                  | Keyword of Symbol.symbol_name
                  | Splice of rcst
                  | List of rcst list

    local
        open Symbol
    in
        fun resolveNicknames _ (CST.IntConstant i) = CST.IntConstant i
          | resolveNicknames _ (CST.FloatConstant f) = CST.FloatConstant f
          | resolveNicknames _ (CST.StringConstant s) = CST.StringConstant s
          | resolveNicknames m (CST.QualifiedSymbol s) = CST.QualifiedSymbol (resolveSymbol s m)
          | resolveNicknames _ (CST.UnqualifiedSymbol n) = CST.UnqualifiedSymbol n
          | resolveNicknames _ (CST.Keyword n) = CST.Keyword n
          | resolveNicknames _ (CST.Splice e) = CST.Splice (resolveNicknames m) e
          | resolveNicknames m (CST.List l) = CST.List (map (resolveNicknames m) l)
        and resolveSymbol sym module =
            let val modName = symbolModuleName sym
            in
                mkSymbol (Module.resolveNickname module modName,
                          symbolName sym)
            end
    end

    fun innResolve _ _ (CST.IntConstant i) = IntConstant i
      | innResolve _ _ (CST.FloatConstant f) = FloatConstant f
      | innResolve _ _ (CST.StringConstant es) = StringConstant es
      | innResolve menv m (CST.QualifiedSymbol s) = resolveQualified menv
                                                                     m
                                                                     (Symbol.symbolModuleName s)
                                                                     (Symbol.symbolName s)
      | innResolve menv m (CST.UnqualifiedSymbol s) = resolveUnqualified menv m s
      | innResolve _ _ (CST.Keyword n) = Keyword n
      | innResolve menv m (CST.Splice e) = Splice (innResolve menv m e)
      | innResolve menv m (CST.List l) = List (map (fn e => innResolve menv m e) l)
    and resolveQualified menv module (modName: Symbol.module_name) (symName: Symbol.symbol_name) =
        (case (Module.envGet menv modName) of
             SOME formod => if Module.doesModuleExport formod symName then
                                Symbol (Symbol.mkSymbol (Module.moduleName formod, symName))
                            else
                                raise Fail ("Module "
                                            ^ (Ident.identString modName)
                                            ^ " does not export a symbol named "
                                            ^ (Ident.identString symName))
           | NONE => raise Fail ("No module named " ^ (Ident.identString modName)))
    and resolveUnqualified menv m (s: Symbol.symbol_name) =
        (* This is an unqualified symbol, so either it was imported from another
           module or it belongs to the current module. We use sourceModule to
           figure this out *)
        Symbol (Symbol.mkSymbol (Module.sourceModule menv m s, s))

    fun resolve menv m e =
        let val e' = resolveNicknames m e
        in
            Util.Result (innResolve menv m e') handle Fail msg => Util.Failure msg
        end
end
