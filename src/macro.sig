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

signature MACRO = sig
    datatype match = Match of match_exp list * match_rest
         and match_exp = MatchVariable of Symbol.symbol
                       | MatchKeyword of Symbol.symbol_name
                       | MatchList of match_exp list * match_rest
         and match_rest = MatchRest
                        | MatchFixed

    datatype template_exp = TemplateExp of RCST.rcst

    datatype template_case = TemplateCase of match * template_exp

    datatype template = Template of Symbol.symbol * string option * template_case list

    datatype symbol_macro = SymbolMacro of Symbol.symbol * RCST.rcst * string option

    type macroenv

    val emptyMacroEnv : macroenv

    val getSymbolMacro : macroenv -> Symbol.symbol -> symbol_macro option

    val macroexpandSymbolMacros : macroenv -> RCST.rcst -> RCST.rcst
end
