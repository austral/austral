%{
open Identifier
open Common
open Cst
open Error

let parse_error _ =
  let s = Parsing.symbol_start ()
  and e = Parsing.symbol_end ()
  in
  raise (AustralParseError (s, e))
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
%token ASSIGN
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

%type <Cst.cstmt> statement
%type <Cst.cexpr> expression

%start statement expression

%%

/* Declarations *)

/* Statements */

statement:
  | if_statement { $1 }
  | LET identifier COLON typespec ASSIGN expression SEMI { CLet ($2, $4, $6) }
  | identifier ASSIGN expression SEMI { CAssign ($1, $3) }
  | expression SEMI { CDiscarding $1 }
  | CASE expression OF whenlist END CASE SEMI { CCase ($2, $4) }
  | WHILE expression DO block END WHILE SEMI { CWhile ($2, $4) }
  | FOR identifier FROM expression TO expression DO block END FOR SEMI { CFor ($2, $4, $6, $8) }
  | RETURN expression SEMI { CReturn $2 }
  | SKIP SEMI { CSkip }
  ;

if_statement:
  | IF expression THEN block else_clause { CIf ($2, $4, $5) }
  ;

else_clause:
  | ELSE IF expression THEN block else_clause { CIf ($3, $5, $6) }
  | ELSE block END IF SEMI { $2 }
  | END IF SEMI { CSkip }

whenlist:
  | when_stmt whenlist { $1 :: $2 }
  | when_stmt { [$1] }
  ;

when_stmt:
  | WHEN identifier LPAREN parameter_list RPAREN DO block SEMI { ConcreteWhen ($2, $4, $7) }
  | WHEN identifier DO block SEMI { ConcreteWhen ($2, [], $4) }
  ;

block:
  | blocklist { CBlock $1 }

blocklist:
  | statement blocklist { $1 :: $2 }
  | statement { [$1] }
  ;

/* Expressions */

expression:
  | atomic_expression { $1 }
  | compound_expression { $1 }
  ;

atomic_expression:
  | NIL { CNilConstant }
  | TRUE { CBoolConstant true }
  | FALSE { CBoolConstant false }
  | int_constant { $1 }
  | float_constant { $1 }
  | variable { $1 }
  | funcall { $1 }
  | parenthesized_expr { $1 }
  ;

int_constant:
  | INT_CONSTANT { CIntConstant $1 }
  ;

float_constant:
  | FLOAT_CONSTANT { CFloatConstant $1 }
  ;

variable:
  | identifier { CVariable $1 }
  ;

funcall:
  | identifier LPAREN argument_list RPAREN { CFuncall ($1, $3) }
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
  | identifier RIGHT_ARROW expression { ($1, $3) }

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
  | int_constant { $1 }
  | float_constant { $1 }
  | variable { $1 }
  | funcall { $1 }
  | parenthesized_expr { $1 }
  ;


/* Common */

identifier:
  | IDENTIFIER { make_ident $1 }
  ;

typespec:
  | identifier LBRACKET typearglist RBRACKET { TypeSpecifier ($1, $3) }
  | identifier { TypeSpecifier ($1, []) }
  ;

typearglist:
  | typespec COMMA typearglist { $1 :: $3 }
  | typespec { [$1] }
  ;

parameter_list:
  | parameter COMMA parameter_list { $1 :: $3 }
  | parameter { [$1] }

parameter:
  | identifier COLON typespec { ConcreteParam ($1, $3) }
