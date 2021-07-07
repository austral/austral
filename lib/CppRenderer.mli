open Cpp

type indentation

type line

val render_line : line -> string

val render_decl : indentation -> cpp_decl -> line list

val render_stmt : indentation -> cpp_stmt -> line list

val render_switch_case : indentation -> cpp_switch_case -> line list

val render_expr : cpp_expr -> string

val render_module : cpp_decl -> string
