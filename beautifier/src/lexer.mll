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
| "::" { ID("::") }
| '@' { ID "@" }
| "as" { ID("as") }
| "begin" { BEGIN }
| "end" { END }
| "match" { MATCH }
| "with" { WITH }
| "when" { WHEN }
| "->" { ID "->" }
| "|" { BAR }
| "type" { TYPE }
| "of" { OF }
| ";;" { SEMISEMI }
| "true" { ID "true" }
| "false" { ID "false" }
| "not" { ID "not" }
| digit+ { ID(Lexing.lexeme lexbuf) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { ID(Lexing.lexeme lexbuf) }
| '-' { ID "-" }
| '+' { ID "+" }
| '*' { ID "*" }
| "-." { ID "-." }
| "+." { ID "+." }
| "*." { ID "*." }
| "/." { ID "/." }
| '=' { ID "=" }
| "<>" { ID "<>" }
| "<=" { ID "<=" }
| ">=" { ID ">=" }
| '<' { ID "<" }
| '>' { ID ">" }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "let" { LET }
| "in" { IN }
| "rec" { REC }
| "mutable" { MUTABLE }
| "open" { OPEN }
| '{' { LBRACE }
| '}' { RBRACE }
| ':' { ID ":" }
| ',' { ID "," }
| '_' { ID "_" }
| '.' { DOT }
| "<-" { ID "<-" }
| ":=" { ID ":=" }
| '!' { EXCLAM }
| ';' { SEMICOLON }
| eof { EOF }
| '"' ([^ '"' '\\'] | '\\' _)* '"' { ID(Lexing.lexeme lexbuf)}

| upper (digit|lower|upper|'_')* '.' lower (digit|lower|upper|'_')* { ID(Lexing.lexeme lexbuf) }
| upper (digit|lower|upper|'_')* { ID(Lexing.lexeme lexbuf) }
| lower (digit|lower|upper|'_')* { ID(Lexing.lexeme lexbuf) }
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
