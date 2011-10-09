type token =
  | CALL_OF
  | RETURN_OF
  | CALLORRETURN_OF
  | IDENTIFIER of (string)
  | INT of (string)
  | LCURLY
  | RCURLY
  | LPAREN
  | RPAREN
  | LSQUARE
  | RSQUARE
  | LBRACELBRACE
  | RBRACERBRACE
  | RARROW
  | TRUE
  | FALSE
  | NOT
  | DOT
  | AMP
  | COLON
  | SEMI_COLON
  | COMMA
  | PIPE
  | CARET
  | QUESTION
  | COLUMNCOLUMN
  | EQ
  | LT
  | GT
  | LE
  | GE
  | NEQ
  | PLUS
  | MINUS
  | SLASH
  | STAR
  | PERCENT
  | OR
  | AND
  | OTHERWISE
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Promelaast.parsed_automaton
