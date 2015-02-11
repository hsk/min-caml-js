%{
open Syntax
%}

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
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
| LBRACE fields RBRACE { Rec($2) }
| LBRACE fields SEMICOLON RBRACE { Rec($2) }
| BEGIN exp END { $2 }
| LPAREN RPAREN { Unit }
| BOOL { Bool($1) }
| INT { Int($1) }
| FLOAT { Float($1) }
| STRING { Str($1) }
| IDENT { Var($1) }
| simple_exp DOT LPAREN exp RPAREN { Get($1, $4) }
| simple_exp DOT IDENT { Get($1, Str $3) }
| EXCLAM exp %prec DOT { Bin($2,".",Var "ref") }

field:
| IDENT EQUAL exp { ($1, $3) }
fields:
| field { [$1] }
| field SEMICOLON fields { $1::$3 }

exps:
| { CApp("Nil", Unit) }
| exp { CApp("Cons", Tuple[$1; CApp("Nil", Unit)]) }
| exp SEMICOLON exps { CApp("Cons", Tuple[$1; $3]) }
exp:
| simple_exp { $1 }
| NOT exp %prec prec_app { Pre("!", $2) }
| MINUS exp %prec prec_unary_minus {
    match $2 with
    | Float(f) -> Float(-.f)
    | e -> Pre("-", e)
}
| exp CONS exp { CApp("Cons", Tuple[$1; $3]) }
| exp AT exp { App(Var "concat", [$1; $3]) }
| exp AS IDENT { Bin($1, "as", Var $3) }

| exp PLUS exp { Bin($1, "+", $3) }
| exp MINUS exp { Bin($1, "-", $3) }
| exp EQUAL exp { Bin($1, "==", $3) }
| exp LESS_GREATER exp { Bin($1, "!=", $3) }
| exp LESS exp { Bin($1, "<", $3) }
| exp GREATER exp { Bin($1, ">", $3) }
| exp LESS_EQUAL exp { Bin($1, "<=", $3) }
| exp GREATER_EQUAL exp { Bin($1, ">=", $3) }
| IF exp THEN exp ELSE exp %prec prec_if { If($2, $4, $6) }
| MATCH exp WITH BAR cases %prec prec_if { Match($2, $5) }
| MATCH exp WITH cases %prec prec_if { Match($2, $4) }
| TYPE IDENT EQUAL type1 SEMISEMI exp %prec prec_if { $6 }

| MINUS_DOT exp %prec prec_unary_minus { Pre("-", $2) }
| exp PLUS_DOT exp { Bin($1, "+", $3) }
| exp MINUS_DOT exp { Bin($1, "-", $3) }
| exp AST_DOT exp { Bin($1, "*", $3) }
| exp SLASH_DOT exp { Bin($1, "/", $3) }
| LET IDENT EQUAL exp IN exp %prec prec_let { Let($2, $4, $6) }
| LET REC fundef IN exp %prec prec_let {
    match $3 with
    | (a,b,c) -> LetRec(a, Fun(b, c), $5)
}

| CIDENT
    %prec prec_capp
    { CApp($1, Unit) }
| CIDENT exp
    %prec prec_capp
    { CApp($1, $2) }

| exp actual_args %prec prec_app { App($1, $2) }
| elems { Tuple($1) }
| LET LPAREN exp RPAREN EQUAL exp IN exp { Match($6, [$3, None, $8]) }
| LET LBRACE fields RBRACE EQUAL exp IN exp { Match($6, [Rec $3, None, $8]) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp { Put($1, $4, $7) }
| simple_exp DOT IDENT LESS_MINUS exp { Put($1, Str $3, $5) }
| simple_exp COLON_EQUAL exp { Put($1, Str "ref", $3) }
| exp SEMICOLON exp { Let("", $1, $3) }
| ARRAY_CREATE simple_exp simple_exp %prec prec_app { Array($2, $3) }
| error
    { failwith
	(Printf.sprintf "parse error near characters %d-%d"
	   (Parsing.symbol_start ())
	   (Parsing.symbol_end ())) }

fundef:
| IDENT formal_args EQUAL exp
    { ($1, $2, $4) }

formal_args:
| IDENT formal_args { $1 :: $2 }
| IDENT { [$1] }

actual_args:
| actual_args simple_exp %prec prec_app { $1 @ [$2] }
| simple_exp %prec prec_app { [$1] }

elems:
| elems COMMA exp { $1 @ [$3] }
| exp COMMA exp { [$1; $3] }

celem:
| { [] }
| exp { [$1] }

celems:
| exp { [$1] }
| exp COMMA celems { $1::$3 }

when1:
| { None }
| WHEN exp { Some $2 }
cases:
| exp when1 ARROW exp
    { [($1, $2, $4)] }
| exp when1 ARROW exp BAR cases { ($1, $2, $4)::$6 }


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
| CIDENT OF types { ($1,$3) }
| CIDENT { ($1,[]) }

consts:
| const { [$1] }
| const BAR consts { $1::$3 }
