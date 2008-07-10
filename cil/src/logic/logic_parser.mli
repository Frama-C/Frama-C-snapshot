type token =
  | IDENTIFIER of (string)
  | STRING_LITERAL of (string)
  | TYPENAME of (string)
  | CONSTANT of (Logic_ptree.constant)
  | LPAR
  | RPAR
  | IF
  | ELSE
  | COLON
  | COLON2
  | COLONCOLON
  | DOT
  | DOTDOT
  | DOTDOTDOT
  | INT
  | INTEGER
  | REAL
  | FLOAT
  | LT
  | GT
  | LE
  | GE
  | EQ
  | NE
  | COMMA
  | ARROW
  | EQUAL
  | FORALL
  | EXISTS
  | IFF
  | IMPLIES
  | AND
  | OR
  | NOT
  | TRUE
  | FALSE
  | OLD
  | AT
  | RESULT
  | BLOCK_LENGTH
  | BASE_ADDR
  | VALID
  | VALID_INDEX
  | VALID_RANGE
  | FRESH
  | DOLLAR
  | QUESTION
  | MINUS
  | PLUS
  | STAR
  | AMP
  | SLASH
  | PERCENT
  | LSQUARE
  | RSQUARE
  | EOF
  | GLOBAL
  | INVARIANT
  | VARIANT
  | DECREASES
  | FOR
  | LABEL
  | ASSERT
  | SEMICOLON
  | NULL
  | EMPTY
  | REQUIRES
  | ENSURES
  | ASSIGNS
  | LOOP
  | NOTHING
  | SLICE
  | IMPACT
  | PRAGMA
  | FROM
  | READS
  | LOGIC
  | PREDICATE
  | AXIOM
  | LEMMA
  | LBRACE
  | RBRACE
  | GHOST
  | VOID
  | CHAR
  | SIGNED
  | UNSIGNED
  | SHORT
  | LONG
  | DOUBLE
  | STRUCT
  | ENUM
  | UNION
  | BSUNION
  | INTER
  | LTCOLON
  | COLONGT
  | TYPE
  | BEHAVIOR
  | BEHAVIORS
  | ASSUMES
  | COMPLETE
  | DISJOINT
  | TERMINATES
  | HAT
  | HATHAT
  | PIPE
  | TILDE
  | GTGT
  | LTLT
  | SIZEOF
  | LAMBDA
  | TYPEOF
  | BSTYPE

val lexpr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic_ptree.lexpr
val annot :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic_ptree.annot
val spec :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic_ptree.spec * Cabs.cabsloc
