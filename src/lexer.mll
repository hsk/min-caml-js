{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+ { token lexbuf }
| "(*" { comment lexbuf; token lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| "::" { CONS }
| '@' { AT }
| "as" { AS }
| "begin" { BEGIN }
| "end" { END }
| "match" { MATCH }
| "with" { WITH }
| "when" { WHEN }
| "->" { ARROW }
| "|" { BAR }
| "type" { TYPE }
| "of" { OF }
| ";;" { SEMISEMI }
| "true" { BOOL(true) }
| "false" { BOOL(false) }
| "not" { NOT }
| digit+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-' { MINUS }
| '+' { PLUS }
| '*' { AST }
| "-." { MINUS_DOT }
| "+." { PLUS_DOT }
| "*." { AST_DOT }
| "/." { SLASH_DOT }
| '=' { EQUAL }
| "<>" { LESS_GREATER }
| "<=" { LESS_EQUAL }
| ">=" { GREATER_EQUAL }
| '<' { LESS }
| '>' { GREATER }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "let" { LET }
| "in" { IN }
| "rec" { REC }
| "mutable" { MUTABLE }
| '{' { LBRACE }
| '}' { RBRACE }
| ':' { COLON }
| ',' { COMMA }
| '_' { IDENT(Syntax.gentmp ()) }
| "Array.create" { ARRAY_CREATE }
| '.' { DOT }
| "<-" { LESS_MINUS }
| ';' { SEMICOLON }
| eof { EOF }
| '"' ([^ '"' '\\'] | '\\' _)* '"' { let s = Lexing.lexeme lexbuf in STRING(String.sub s 1 ((String.length s)-2))}
| upper (digit|lower|upper|'_')* { CIDENT(Lexing.lexeme lexbuf) }
| lower (digit|lower|upper|'_')* { IDENT(Lexing.lexeme lexbuf) }
| _ {
  failwith
    (Printf.sprintf "unknown token %s near characters %d-%d"
      (Lexing.lexeme lexbuf)
      (Lexing.lexeme_start lexbuf)
      (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)" { () }
| "(*" { comment lexbuf; comment lexbuf }
| eof { Format.eprintf "warning: unterminated comment@." }
| _ { comment lexbuf }
