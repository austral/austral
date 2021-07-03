%{
open Identifier
open Common
open Cst
%}

/* Brackets */
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
/* Arithmetic operators */
%token PLUS
%token MINUS
%token MUL
%token DIV
/* Comparison operators */
%token EQ
%token NEQ
%token LT
%token LTE
%token GT
%token GTE
/* Logical operators */
%token AND
%token OR
%token NOT
/* Keywords */
%token MODULE
%token IS
%token BODY
%token IMPORT
%token END
%token CONSTANT
%token TYPE
%token FUNCTION
%token GENERIC
%token RECORD
%token UNION
%token CASE
%token OF
%token WHEN
%token INTERFACE
%token IMPLEMENTATION
%token METHOD
%token IF
%token THEN
%token ELSE
%token LET
%token WHILE
%token FOR
%token DO
%token FROM
%token TO
%token RETURN
%token SKIP
/* Symbols */
%token SEMI
%token COMMA
%token COLON
%token RIGHT_ARROW
/* Identifiers and constants */
%token NIL
%token TRUE
%token FALSE
%token <string> INT_CONSTANT
%token <string> FLOAT_CONSTANT
%token <string> IDENTIFIER
/* etc. */
%token EOF

/* Types */

%type <Cst.cexpr> expression
%type <Cst.cexpr> atomic_expression
%type <Cst.cexpr> variable
%type <Cst.cexpr> funcall
%type <Cst.cexpr> parenthesized_expr
%type <Cst.concrete_arglist> argument_list
%type <Cst.cexpr list> positional_arglist
%type <(Identifier.identifier * Cst.cexpr) list> named_arglist
%type <(Identifier.identifier * Cst.cexpr)> named_arg

%type <Cst.cexpr> compound_expression
%type <Common.comparison_operator> comp_op
%type <Cst.cexpr> arith_expr
%type <Cst.cexpr> term
%type <Cst.cexpr> factor

%start expression

%%

expression:
  | atomic_expression { $1 }
  | compound_expression { $1 }
  ;

atomic_expression:
  | NIL { CNilConstant }
  | TRUE { CBoolConstant true }
  | FALSE { CBoolConstant false }
  | variable { $1 }
  | funcall { $1 }
  | parenthesized_expr { $1 }
  ;

variable:
  | IDENTIFIER { CVariable (make_ident $1) }
  ;

funcall:
  | IDENTIFIER LPAREN argument_list RPAREN { CFuncall (make_ident $1, $3) }
  ;

parenthesized_expr:
  | LPAREN expression RPAREN { $2 }
  ;

argument_list:
  | positional_arglist  { ConcretePositionalArgs $1 }
  | named_arglist { ConcreteNamedArgs $1 }
  ;

positional_arglist:
  | expression COMMA positional_arglist { $1 :: $3 }
  | expression { [$1] }
  ;

named_arglist:
  | named_arg COMMA named_arglist { $1 :: $3 }
  | named_arg { [$1] }
  ;

named_arg:
  | IDENTIFIER RIGHT_ARROW expression { (make_ident $1, $3) }

compound_expression:
  | atomic_expression comp_op atomic_expression { CComparison ($2, $1, $3) }
  | atomic_expression AND atomic_expression { CConjunction ($1, $3) }
  | atomic_expression OR atomic_expression { CDisjunction ($1, $3) }
  | NOT atomic_expression { CNegation $2 }
  | arith_expr { $1 }
  | IF expression THEN expression ELSE expression { CIfExpression ($2, $4, $6) }
  ;

comp_op:
  | EQ { Equal }
  | NEQ { NotEqual }
  | LT { LessThan }
  | LTE { LessThanOrEqual }
  | GT { GreaterThanOrEqual }
  | GTE { GreaterThanOrEqual }

arith_expr:
  | term PLUS arith_expr { CArith (Add, $1, $3) }
  | term MINUS arith_expr { CArith (Subtract, $1, $3) }
  | term { $1 }
  ;

term:
  | factor MUL term { CArith (Multiply, $1, $3) }
  | factor DIV term { CArith (Divide, $1, $3) }
  | factor  { $1 }
  | MINUS factor  { CArith (Subtract, CIntConstant "0", $2) }
  ;

factor:
  | variable { $1 }
  | funcall { $1 }
  | parenthesized_expr { $1 }
  ;
