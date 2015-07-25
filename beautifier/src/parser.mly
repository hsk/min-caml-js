%{
open Syntax
%}

%token <string> ID
%token EQUAL
%token IF THEN ELSE
%token LET REC IN
%token COMMA
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN RPAREN
%token BEGIN END
%token MATCH WITH WHEN ARROW BAR
%token TYPE OF SEMISEMI AST
%token LBRACK RBRACK
%token MUTABLE LBRACE RBRACE COLON
%token EXCLAM COLON_EQUAL
%token OPEN
%token EOF

%right prec_let
%right SEMICOLON CONS AT AS
%right prec_if
%right LESS_MINUS COLON_EQUAL
%left COMMA
%left EQUAL
%right prec_unary_minus
%left prec_capp
%left prec_app
%left DOT

%type <Syntax.e> exp
%start exp

%%

simple_exp:
| LBRACK exps RBRACK { $2 }
| LPAREN exp RPAREN { $2 }
| LBRACE fields RBRACE { Ls $2 }
| LBRACE fields SEMICOLON RBRACE { Ls $2 }
| BEGIN exp END { $2 }
| LPAREN RPAREN { Ls[] }
| ID { S $1 }
| simple_exp DOT LPAREN exp RPAREN { Ls[$1; $4] }
| simple_exp DOT ID { Ls[$1; S $3] }
| EXCLAM exp %prec DOT { Ls[S "!"; $2] }

field:
| ID EQUAL exp { Ls[S $1; $3] }
fields:
| field { [$1] }
| field SEMICOLON fields { $1::$3 }

exps:
| { Ls[] }
| exp { $1 }
| exp SEMICOLON exps { Ls[$1;S ";"; $3] }

exp:
| simple_exp { $1 }
| IF exp THEN exp ELSE exp %prec prec_if { Ls[$2; $4; $6] }
| MATCH exp WITH BAR cases %prec prec_if { Ls($2 :: $5) }
| MATCH exp WITH cases %prec prec_if { Ls($2 :: $4) }
| TYPE ID EQUAL type1 SEMISEMI exp %prec prec_if { $6 }
| LET ID EQUAL exp IN exp %prec prec_let { Ls[S $2; $4; $6] }
| LET REC fundef IN exp %prec prec_let { Ls[$3;$5]}
| OPEN ID exp %prec prec_capp { Ls[S $2; $3] }
| ID %prec prec_capp { S $1 }

| exp actual_args %prec prec_app { Ls[$1; Ls $2] }
| elems { Ls $1 }
| LET LPAREN exp RPAREN EQUAL exp IN exp { Ls[$6; $3; $8] }
| LET LBRACE fields RBRACE EQUAL exp IN exp { Ls[$6; Ls $3; $8] }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp { Ls[$1; $4; $7] }
| simple_exp DOT ID LESS_MINUS exp { Ls[$1; S $3; $5] }
| simple_exp COLON_EQUAL exp { Ls[$1; S "ref"; $3] }
| exp SEMICOLON exp { Ls[S ""; $1; $3] }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }

fundef:
| ID formal_args EQUAL exp
    { Ls[S $1; Ls $2; $4] }

formal_args:
| ID formal_args { S $1 :: $2 }
| ID { [S $1] }

actual_args:
| actual_args simple_exp %prec prec_app { $1 @ [$2] }
| simple_exp %prec prec_app { [$1] }

elems:
| elems COMMA exp { $1 @ [$3] }
| exp COMMA exp { [$1; $3] }


when1:
| { S "" }
| WHEN exp { $2 }
cases:
| exp when1 ARROW exp
    { [$1; $2; $4] }
| exp when1 ARROW exp BAR cases { $1:: $2:: $4::$6 }


tyrec:
| MUTABLE ID COLON type1 { "" }
| ID COLON type1 { "" }

tyrecs:
| tyrec {[$1]}
| tyrec SEMICOLON tyrecs { $1::$3 }
type1:
| ID { $1 }
| LBRACE tyrecs RBRACE { "{record}" }
| consts { "" }
| BAR consts { "" }
types:
| type1 { [$1] }
| type1 AST types { $1::$3 }

const:
| ID OF types { [$1,$3] }
| ID { [$1,[]] }

consts:
| const { [$1] }
| const BAR consts { $1::$3 }
