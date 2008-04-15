type token =
  | ID of (Dot_ast.id)
  | COLON
  | COMMA
  | EQUAL
  | SEMICOLON
  | EDGEOP
  | STRICT
  | GRAPH
  | DIGRAPH
  | LBRA
  | RBRA
  | LSQ
  | RSQ
  | NODE
  | EDGE
  | SUBGRAPH
  | EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Dot_ast.file
