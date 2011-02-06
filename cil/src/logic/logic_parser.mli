type token =
  | MODULE
  | FUNCTION
  | CONTRACT
  | INCLUDE
  | EXT_AT
  | EXT_LET
  | IDENTIFIER of (string)
  | TYPENAME of (string)
  | STRING_LITERAL of (bool*string)
  | CONSTANT of (Logic_ptree.constant)
  | CONSTANT10 of (string)
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
  | BOOLEAN
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
  | SEPARATED
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
  | EXITS
  | BREAKS
  | CONTINUES
  | RETURNS
  | VOLATILE
  | READS
  | WRITES
  | LOGIC
  | PREDICATE
  | INDUCTIVE
  | AXIOMATIC
  | AXIOM
  | LEMMA
  | LBRACE
  | RBRACE
  | GHOST
  | CASE
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
  | LET
  | TYPEOF
  | BSTYPE
  | WITH
  | CONST

val lexpr_eof :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic_ptree.lexpr
val annot :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic_ptree.annot
val spec :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic_ptree.spec * Cabs.cabsloc
val ext_spec :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Logic_ptree.ext_spec
