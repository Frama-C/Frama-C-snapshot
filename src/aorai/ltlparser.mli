type token =
  | LTL_TRUE
  | LTL_FALSE
  | LTL_LPAREN
  | LTL_RPAREN
  | LTL_OR
  | LTL_IMPLIES
  | LTL_LEFT_RIGHT_ARROW
  | LTL_AND
  | LTL_NOT
  | LTL_GLOBALLY
  | LTL_FATALLY
  | LTL_UNTIL
  | LTL_RELEASE
  | LTL_NEXT
  | LTL_EQ
  | LTL_LT
  | LTL_GT
  | LTL_LE
  | LTL_GE
  | LTL_NEQ
  | LTL_PLUS
  | LTL_MINUS
  | LTL_DIV
  | LTL_STAR
  | LTL_MODULO
  | LTL_RIGHT_ARROW
  | LTL_DOT
  | LTL_LEFT_SQUARE
  | LTL_RIGHT_SQUARE
  | LTL_ADRESSE
  | LTL_CALL
  | LTL_RETURN
  | LTL_CALL_OR_RETURN
  | LTL_INT of (string)
  | LTL_LABEL of (string)
  | EOF

val ltl :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Ltlast.formula * (string, (Logic_ptree.relation *  Promelaast.expression * Promelaast.expression)) Hashtbl.t)
