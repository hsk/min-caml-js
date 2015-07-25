%{
open Syntax
%}

%token <string> BOOL
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <string> IDENT
%token <string> CIDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token BEGIN END
%token MATCH WITH WHEN ARROW BAR
%token TYPE OF SEMISEMI AST
%token LBRACK RBRACK CONS AT AS
%token MUTABLE LBRACE RBRACE COLON
%token EXCLAM COLON_EQUAL
%token OPEN
%token EOF

%right prec_let
%right SEMICOLON CONS AT AS
%right prec_if
%right LESS_MINUS COLON_EQUAL
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
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
| BOOL { S $1 }
| INT { S $1 }
| FLOAT { S $1 }
| STRING { S $1 }
| IDENT { S $1 }
| simple_exp DOT LPAREN exp RPAREN { Ls[$1; $4] }
| simple_exp DOT IDENT { Ls[$1; S $3] }
| EXCLAM exp %prec DOT { Ls[S "!"; $2] }

field:
| IDENT EQUAL exp { Ls[S $1; $3] }
fields:
| field { [$1] }
| field SEMICOLON fields { $1::$3 }

exps:
| { Ls[] }
| exp { $1 }
| exp SEMICOLON exps { Ls[$1;S ";"; $3] }
exp:
| simple_exp { $1 }
| NOT exp %prec prec_app { $2 }
| MINUS exp %prec prec_unary_minus { $2 }
| exp CONS exp { Ls[$1; $3] }
| exp AT exp { Ls[$1; $3] }
| exp AS IDENT { Ls[$1; S $3] }

| exp PLUS exp { Ls[$1; S "+"; $3] }
| exp MINUS exp { Ls[$1; S "-"; $3] }
| exp EQUAL exp { Ls[$1; S "=="; $3] }
| exp LESS_GREATER exp { Ls[$1; S "!="; $3] }
| exp LESS exp { Ls[$1; S "<"; $3] }
| exp GREATER exp { Ls[$1; S ">"; $3] }
| exp LESS_EQUAL exp { Ls[$1; S "<="; $3] }
| exp GREATER_EQUAL exp { Ls[$1; S ">="; $3] }
| IF exp THEN exp ELSE exp %prec prec_if { Ls[$2; $4; $6] }
| MATCH exp WITH BAR cases %prec prec_if { Ls($2 :: $5) }
| MATCH exp WITH cases %prec prec_if { Ls($2 :: $4) }
| TYPE IDENT EQUAL type1 SEMISEMI exp %prec prec_if { $6 }

| MINUS_DOT exp %prec prec_unary_minus { Ls[S "-"; $2] }
| exp PLUS_DOT exp { Ls[$1; S "+"; $3] }
| exp MINUS_DOT exp { Ls[$1; S "-"; $3] }
| exp AST_DOT exp { Ls[$1; S "*"; $3] }
| exp SLASH_DOT exp { Ls[$1; S "/"; $3] }
| LET IDENT EQUAL exp IN exp %prec prec_let { Ls[S $2; $4; $6] }
| LET REC fundef IN exp %prec prec_let { Ls[$3;$5]}

| OPEN CIDENT exp
    %prec prec_capp
    { Ls[S $2; $3] }
| CIDENT
    %prec prec_capp
    { S $1 }
| CIDENT exp
    %prec prec_capp
    { Ls[S $1; $2] }

| exp actual_args %prec prec_app { Ls[$1; Ls $2] }
| elems { Ls $1 }
| LET LPAREN exp RPAREN EQUAL exp IN exp { Ls[$6; $3; $8] }
| LET LBRACE fields RBRACE EQUAL exp IN exp { Ls[$6; Ls $3; $8] }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp { Ls[$1; $4; $7] }
| simple_exp DOT IDENT LESS_MINUS exp { Ls[$1; S $3; $5] }
| simple_exp COLON_EQUAL exp { Ls[$1; S "ref"; $3] }
| exp SEMICOLON exp { Ls[S ""; $1; $3] }
| ARRAY_CREATE simple_exp simple_exp %prec prec_app { Ls[$2; $3] }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }

fundef:
| IDENT formal_args EQUAL exp
    { Ls[S $1; Ls $2; $4] }

formal_args:
| IDENT formal_args { S $1 :: $2 }
| IDENT { [S $1] }

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
| MUTABLE IDENT COLON type1 { "" }
| IDENT COLON type1 { "" }

tyrecs:
| tyrec {[$1]}
| tyrec SEMICOLON tyrecs { $1::$3 }
type1:
| IDENT { $1 }
| LBRACE tyrecs RBRACE { "{record}" }
| consts { "" }
| BAR consts { "" }
types:
| type1 { [$1] }
| type1 AST types { $1::$3 }

const:
| CIDENT OF types { [$1,$3] }
| CIDENT { [$1,[]] }

consts:
| const { [$1] }
| const BAR consts { $1::$3 }
