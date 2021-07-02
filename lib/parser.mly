%{
open Identifier
open Cst
%}

/* Identifiers and constants */
%token <string> IDENTIFIER
%token NIL
%token TRUE
%token FALSE
%token <string> INT_CONSTANT
%token <string> FLOAT_CONSTANT
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
/* etc. */
%token EOF

/* Types */

%type <cexpr> expression
%type <cexpr> atomic_expression
%type <concrete_arglist> argument_list
%type <cexpr list> positional_arglist

%start expression

%%

expression:
  | atomic_expression { $1 }
  ;

atomic_expression:
  | NIL { CNilConstant }
  | IDENTIFIER LPAREN argument_list RPAREN { CFuncall (make_ident $1, $3) }
  | LPAREN expression RPAREN { $2 }
  ;

argument_list:
  | positional_arglist  { ConcretePositionalArgs $1 }
  | named_arglist { ConcreteNamedArgs $1 }
  ;

positional_arglist:
  | expression COMMA positional_arglist { $1 :: $3 }
  | expression { $1 }
  ;

named_arglist:
  | named_arg COMMA named_arglist { $1 :: $3 }
  | named_arg { $1 }
  ;

named_arg:
  | IDENTIFIER RIGHT_ARROW expression { (make_ident $1, $3) }
