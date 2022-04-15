open CRepr

type indentation

type line

val render_line : line -> string

val render_unit : c_unit -> string

val render_decl : indentation -> c_decl -> line list

val render_stmt : indentation -> c_stmt -> line list

val render_switch_case : indentation -> c_switch_case -> line list

val render_expr : c_expr -> string
