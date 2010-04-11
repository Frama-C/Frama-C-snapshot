type token =
  | SPEC of (Lexing.position * string)
  | DECL of (Logic_ptree.decl list)
  | CODE_ANNOT of (Logic_ptree.code_annot * Cabs.cabsloc)
  | LOOP_ANNOT of (Logic_ptree.code_annot list * Cabs.cabsloc)
  | ATTRIBUTE_ANNOT of (string * Cabs.cabsloc)
  | IDENT of (string * Cabs.cabsloc)
  | CST_CHAR of (int64 list * Cabs.cabsloc)
  | CST_WCHAR of (int64 list * Cabs.cabsloc)
  | CST_INT of (string * Cabs.cabsloc)
  | CST_FLOAT of (string * Cabs.cabsloc)
  | NAMED_TYPE of (string * Cabs.cabsloc)
  | CST_STRING of (int64 list * Cabs.cabsloc)
  | CST_WSTRING of (int64 list * Cabs.cabsloc)
  | EOF
  | BOOL of (Cabs.cabsloc)
  | CHAR of (Cabs.cabsloc)
  | INT of (Cabs.cabsloc)
  | DOUBLE of (Cabs.cabsloc)
  | FLOAT of (Cabs.cabsloc)
  | VOID of (Cabs.cabsloc)
  | INT64 of (Cabs.cabsloc)
  | INT32 of (Cabs.cabsloc)
  | ENUM of (Cabs.cabsloc)
  | STRUCT of (Cabs.cabsloc)
  | TYPEDEF of (Cabs.cabsloc)
  | UNION of (Cabs.cabsloc)
  | SIGNED of (Cabs.cabsloc)
  | UNSIGNED of (Cabs.cabsloc)
  | LONG of (Cabs.cabsloc)
  | SHORT of (Cabs.cabsloc)
  | VOLATILE of (Cabs.cabsloc)
  | EXTERN of (Cabs.cabsloc)
  | STATIC of (Cabs.cabsloc)
  | CONST of (Cabs.cabsloc)
  | RESTRICT of (Cabs.cabsloc)
  | AUTO of (Cabs.cabsloc)
  | REGISTER of (Cabs.cabsloc)
  | THREAD of (Cabs.cabsloc)
  | SIZEOF of (Cabs.cabsloc)
  | ALIGNOF of (Cabs.cabsloc)
  | EQ
  | PLUS_EQ
  | MINUS_EQ
  | STAR_EQ
  | SLASH_EQ
  | PERCENT_EQ
  | AND_EQ
  | PIPE_EQ
  | CIRC_EQ
  | INF_INF_EQ
  | SUP_SUP_EQ
  | ARROW
  | DOT
  | EQ_EQ
  | EXCLAM_EQ
  | INF
  | SUP
  | INF_EQ
  | SUP_EQ
  | PLUS of (Cabs.cabsloc)
  | MINUS of (Cabs.cabsloc)
  | STAR of (Cabs.cabsloc)
  | SLASH
  | PERCENT
  | TILDE of (Cabs.cabsloc)
  | AND of (Cabs.cabsloc)
  | PIPE
  | CIRC
  | EXCLAM of (Cabs.cabsloc)
  | AND_AND of (Cabs.cabsloc)
  | PIPE_PIPE
  | INF_INF
  | SUP_SUP
  | PLUS_PLUS of (Cabs.cabsloc)
  | MINUS_MINUS of (Cabs.cabsloc)
  | RPAREN
  | LPAREN of (Cabs.cabsloc)
  | RBRACE of (Cabs.cabsloc)
  | LBRACE of (Cabs.cabsloc)
  | LBRACKET
  | RBRACKET
  | COLON
  | SEMICOLON of (Cabs.cabsloc)
  | COMMA
  | ELLIPSIS
  | QUEST
  | BREAK of (Cabs.cabsloc)
  | CONTINUE of (Cabs.cabsloc)
  | GOTO of (Cabs.cabsloc)
  | RETURN of (Cabs.cabsloc)
  | SWITCH of (Cabs.cabsloc)
  | CASE of (Cabs.cabsloc)
  | DEFAULT of (Cabs.cabsloc)
  | WHILE of (Cabs.cabsloc)
  | DO of (Cabs.cabsloc)
  | FOR of (Cabs.cabsloc)
  | IF of (Cabs.cabsloc)
  | TRY of (Cabs.cabsloc)
  | EXCEPT of (Cabs.cabsloc)
  | FINALLY of (Cabs.cabsloc)
  | ELSE
  | ATTRIBUTE of (Cabs.cabsloc)
  | INLINE of (Cabs.cabsloc)
  | ASM of (Cabs.cabsloc)
  | TYPEOF of (Cabs.cabsloc)
  | FUNCTION__ of (Cabs.cabsloc)
  | PRETTY_FUNCTION__ of (Cabs.cabsloc)
  | LABEL__
  | BUILTIN_VA_ARG of (Cabs.cabsloc)
  | ATTRIBUTE_USED of (Cabs.cabsloc)
  | BUILTIN_VA_LIST
  | BLOCKATTRIBUTE
  | BUILTIN_TYPES_COMPAT of (Cabs.cabsloc)
  | BUILTIN_OFFSETOF of (Cabs.cabsloc)
  | DECLSPEC of (Cabs.cabsloc)
  | MSASM of (string * Cabs.cabsloc)
  | MSATTR of (string * Cabs.cabsloc)
  | PRAGMA_LINE of (string * Cabs.cabsloc)
  | PRAGMA of (Cabs.cabsloc)
  | PRAGMA_EOL
  | AT_TRANSFORM of (Cabs.cabsloc)
  | AT_TRANSFORMEXPR of (Cabs.cabsloc)
  | AT_SPECIFIER of (Cabs.cabsloc)
  | AT_EXPR of (Cabs.cabsloc)
  | AT_NAME
  | LGHOST
  | RGHOST

val interpret :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (bool*Cabs.definition) list
val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (bool*Cabs.definition) list
