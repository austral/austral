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
%type <identifier * cexpr> named_arg

%start expression

%%

expression:
  | v=atomic_expression { v }
  ;

atomic_expression:
  | NIL { CNilConstant }
  | TRUE { CBoolConstant true }
  | FALSE { CBoolConstant false }
  | i=INT_CONSTANT { CIntConstant i }
  | f=FLOAT_CONSTANT { CFloatConstant f }
  | v=IDENTIFIER { CVariable (make_ident v) }
  | n=IDENTIFIER LPAREN args=argument_list RPAREN { CFuncall (make_ident n, args) }
  | LPAREN e=expression RPAREN { e }

argument_list:
  | args=separated_list(COMMA, named_arg) { ConcreteNamedArgs args }
  | args=separated_list(COMMA, expression) { ConcretePositionalArgs args }

named_arg:
  | n=IDENTIFIER RIGHT_ARROW v=expression { (make_ident n, v) }
