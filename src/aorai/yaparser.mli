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
  | RARROW
  | TRUE
  | FALSE
  | FUNC
  | NOT
  | DOT
  | AMP
  | COLON
  | SEMI_COLON
  | COMMA
  | PIPE
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
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t)
