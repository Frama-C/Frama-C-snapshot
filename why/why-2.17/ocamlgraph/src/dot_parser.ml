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

open Parsing;;
# 23 "src/dot_parser.mly"
  open Dot_ast
  open Parsing

  let compass_pt = function
    | Ident "n" -> N
    | Ident "ne" -> Ne
    | Ident "e" -> E
    | Ident "se" -> Se
    | Ident "s" -> S
    | Ident "sw" -> Sw
    | Ident "w" -> W
    | Ident "nw" -> Nw
    | _ -> invalid_arg "compass_pt"

# 37 "src/dot_parser.ml"
let yytransl_const = [|
  258 (* COLON *);
  259 (* COMMA *);
  260 (* EQUAL *);
  261 (* SEMICOLON *);
  262 (* EDGEOP *);
  263 (* STRICT *);
  264 (* GRAPH *);
  265 (* DIGRAPH *);
  266 (* LBRA *);
  267 (* RBRA *);
  268 (* LSQ *);
  269 (* RSQ *);
  270 (* NODE *);
  271 (* EDGE *);
  272 (* SUBGRAPH *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\005\000\005\000\006\000\
\006\000\008\000\008\000\007\000\007\000\007\000\007\000\007\000\
\009\000\010\000\011\000\011\000\011\000\016\000\018\000\018\000\
\015\000\015\000\013\000\019\000\019\000\020\000\020\000\014\000\
\014\000\017\000\017\000\004\000\004\000\021\000\021\000\022\000\
\022\000\023\000\023\000\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\007\000\000\000\001\000\001\000\001\000\000\000\001\000\002\000\
\003\000\000\000\001\000\001\000\001\000\001\000\003\000\001\000\
\002\000\003\000\002\000\002\000\002\000\003\000\000\000\003\000\
\001\000\001\000\002\000\000\000\001\000\002\000\004\000\000\000\
\001\000\003\000\004\000\000\000\001\000\002\000\003\000\001\000\
\003\000\000\000\001\000\002\000\005\000\004\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\003\000\048\000\000\000\004\000\005\000\000\000\
\037\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\000\000\012\000\013\000\014\000\000\000\
\000\000\000\000\000\000\000\000\027\000\029\000\000\000\019\000\
\000\000\020\000\021\000\000\000\000\000\000\000\011\000\000\000\
\017\000\033\000\000\000\000\000\000\000\015\000\000\000\000\000\
\000\000\047\000\000\000\000\000\001\000\009\000\000\000\026\000\
\025\000\000\000\018\000\000\000\000\000\000\000\043\000\000\000\
\000\000\046\000\000\000\022\000\031\000\041\000\035\000\039\000\
\045\000\000\000\024\000"

let yydgoto = "\002\000\
\004\000\005\000\008\000\010\000\018\000\019\000\020\000\040\000\
\021\000\022\000\023\000\024\000\025\000\041\000\026\000\044\000\
\042\000\068\000\029\000\030\000\048\000\049\000\064\000"

let yysindex = "\009\000\
\024\255\000\000\000\000\000\000\000\255\000\000\000\000\031\255\
\000\000\029\255\131\255\011\255\033\255\131\255\033\255\033\255\
\051\255\040\255\000\000\048\255\000\000\000\000\000\000\000\000\
\033\255\050\255\057\255\067\255\000\000\000\000\069\255\000\000\
\062\255\000\000\000\000\070\255\131\255\091\000\000\000\131\255\
\000\000\000\000\018\255\033\255\090\255\000\000\099\255\081\255\
\101\255\000\000\131\255\095\255\000\000\000\000\107\255\000\000\
\000\000\110\255\000\000\114\255\117\255\033\255\000\000\069\255\
\111\255\000\000\018\255\000\000\000\000\000\000\000\000\000\000\
\000\000\110\255\000\000"

let yyrindex = "\000\000\
\074\255\000\000\000\000\000\000\000\000\000\000\000\000\116\255\
\000\000\000\000\118\255\006\255\000\000\118\255\000\000\000\000\
\000\000\000\000\000\000\120\255\000\000\000\000\000\000\049\255\
\061\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\073\255\118\255\000\000\000\000\122\255\
\000\000\000\000\000\000\097\255\032\255\000\000\023\255\000\000\
\022\255\000\000\118\255\000\000\000\000\000\000\006\255\000\000\
\000\000\085\255\000\000\000\000\000\000\109\255\000\000\124\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\085\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\246\255\087\000\000\000\000\000\
\000\000\000\000\000\000\214\255\218\255\094\000\219\255\000\000\
\243\255\066\000\000\000\000\000\078\000\000\000\000\000"

let yytablesize = 147
let yytable = "\032\000\
\056\000\034\000\035\000\033\000\057\000\058\000\028\000\006\000\
\007\000\001\000\028\000\028\000\027\000\028\000\028\000\028\000\
\028\000\028\000\055\000\028\000\028\000\028\000\042\000\040\000\
\056\000\040\000\052\000\014\000\057\000\074\000\003\000\009\000\
\030\000\017\000\042\000\040\000\030\000\030\000\011\000\030\000\
\065\000\030\000\030\000\030\000\031\000\030\000\030\000\030\000\
\071\000\016\000\038\000\036\000\039\000\016\000\026\000\043\000\
\016\000\045\000\016\000\016\000\037\000\032\000\016\000\016\000\
\016\000\032\000\025\000\046\000\032\000\047\000\032\000\032\000\
\050\000\044\000\032\000\032\000\032\000\044\000\044\000\051\000\
\044\000\002\000\002\000\044\000\044\000\023\000\044\000\044\000\
\044\000\023\000\053\000\060\000\023\000\062\000\023\000\023\000\
\023\000\032\000\023\000\023\000\023\000\032\000\061\000\063\000\
\032\000\066\000\032\000\032\000\027\000\034\000\032\000\032\000\
\032\000\034\000\069\000\067\000\034\000\070\000\034\000\034\000\
\010\000\073\000\034\000\034\000\034\000\036\000\054\000\010\000\
\006\000\010\000\010\000\012\000\008\000\010\000\010\000\010\000\
\038\000\059\000\013\000\075\000\014\000\072\000\000\000\000\000\
\015\000\016\000\017\000"

let yycheck = "\013\000\
\043\000\015\000\016\000\014\000\043\000\043\000\001\001\008\001\
\009\001\001\000\005\001\006\001\002\001\008\001\004\001\010\001\
\011\001\012\001\001\001\014\001\015\001\016\001\001\001\001\001\
\067\000\003\001\037\000\010\001\067\000\067\000\007\001\001\001\
\001\001\016\001\013\001\013\001\005\001\006\001\010\001\008\001\
\051\000\010\001\011\001\012\001\012\001\014\001\015\001\016\001\
\062\000\001\001\011\001\001\001\005\001\005\001\006\001\006\001\
\008\001\001\001\010\001\011\001\010\001\001\001\014\001\015\001\
\016\001\005\001\006\001\001\001\008\001\001\001\010\001\011\001\
\011\001\001\001\014\001\015\001\016\001\005\001\006\001\010\001\
\008\001\008\001\009\001\011\001\012\001\001\001\014\001\015\001\
\016\001\005\001\000\000\002\001\008\001\013\001\010\001\011\001\
\012\001\001\001\014\001\015\001\016\001\005\001\004\001\003\001\
\008\001\011\001\010\001\011\001\002\001\001\001\014\001\015\001\
\016\001\005\001\001\001\006\001\008\001\001\001\010\001\011\001\
\001\001\011\001\014\001\015\001\016\001\010\001\040\000\008\001\
\011\001\010\001\011\001\001\001\011\001\014\001\015\001\016\001\
\013\001\044\000\008\001\074\000\010\001\064\000\255\255\255\255\
\014\001\015\001\016\001"

let yynames_const = "\
  COLON\000\
  COMMA\000\
  EQUAL\000\
  SEMICOLON\000\
  EDGEOP\000\
  STRICT\000\
  GRAPH\000\
  DIGRAPH\000\
  LBRA\000\
  RBRA\000\
  LSQ\000\
  RSQ\000\
  NODE\000\
  EDGE\000\
  SUBGRAPH\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'strict_opt) in
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'graph_or_digraph) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'id_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt_list) in
    Obj.repr(
# 49 "src/dot_parser.mly"
    ( { strict = _1; digraph = _2; id = _3; stmts = _5 } )
# 199 "src/dot_parser.ml"
               : Dot_ast.file))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "src/dot_parser.mly"
                ( false )
# 205 "src/dot_parser.ml"
               : 'strict_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "src/dot_parser.mly"
                ( true )
# 211 "src/dot_parser.ml"
               : 'strict_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "src/dot_parser.mly"
          ( false )
# 217 "src/dot_parser.ml"
               : 'graph_or_digraph))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "src/dot_parser.mly"
          ( true )
# 223 "src/dot_parser.ml"
               : 'graph_or_digraph))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "src/dot_parser.mly"
                ( [] )
# 229 "src/dot_parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list1_stmt) in
    Obj.repr(
# 64 "src/dot_parser.mly"
                ( _1 )
# 236 "src/dot_parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'semicolon_opt) in
    Obj.repr(
# 68 "src/dot_parser.mly"
                     ( [_1] )
# 244 "src/dot_parser.ml"
               : 'list1_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'semicolon_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list1_stmt) in
    Obj.repr(
# 69 "src/dot_parser.mly"
                                ( _1 :: _3 )
# 253 "src/dot_parser.ml"
               : 'list1_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "src/dot_parser.mly"
                ( () )
# 259 "src/dot_parser.ml"
               : 'semicolon_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "src/dot_parser.mly"
                ( () )
# 265 "src/dot_parser.ml"
               : 'semicolon_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'node_stmt) in
    Obj.repr(
# 78 "src/dot_parser.mly"
            ( _1 )
# 272 "src/dot_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'edge_stmt) in
    Obj.repr(
# 79 "src/dot_parser.mly"
            ( _1 )
# 279 "src/dot_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'attr_stmt) in
    Obj.repr(
# 80 "src/dot_parser.mly"
            ( _1 )
# 286 "src/dot_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Dot_ast.id) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Dot_ast.id) in
    Obj.repr(
# 81 "src/dot_parser.mly"
              ( Equal (_1, _3) )
# 294 "src/dot_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'subgraph) in
    Obj.repr(
# 82 "src/dot_parser.mly"
            ( Subgraph _1 )
# 301 "src/dot_parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'node_id) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list_opt) in
    Obj.repr(
# 86 "src/dot_parser.mly"
                        ( Node_stmt (_1, _2) )
# 309 "src/dot_parser.ml"
               : 'node_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'node) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'edge_rhs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list_opt) in
    Obj.repr(
# 90 "src/dot_parser.mly"
                              ( Edge_stmt (_1, _2, _3) )
# 318 "src/dot_parser.ml"
               : 'edge_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 94 "src/dot_parser.mly"
                  ( Attr_graph _2 )
# 325 "src/dot_parser.ml"
               : 'attr_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 95 "src/dot_parser.mly"
                  ( Attr_node _2 )
# 332 "src/dot_parser.ml"
               : 'attr_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 96 "src/dot_parser.mly"
                  ( Attr_edge _2 )
# 339 "src/dot_parser.ml"
               : 'attr_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'node) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'edge_rhs_opt) in
    Obj.repr(
# 100 "src/dot_parser.mly"
                           ( _2 :: _3 )
# 347 "src/dot_parser.ml"
               : 'edge_rhs))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "src/dot_parser.mly"
                ( [] )
# 353 "src/dot_parser.ml"
               : 'edge_rhs_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'node) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'edge_rhs_opt) in
    Obj.repr(
# 105 "src/dot_parser.mly"
                           ( _2 :: _3 )
# 361 "src/dot_parser.ml"
               : 'edge_rhs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'node_id) in
    Obj.repr(
# 109 "src/dot_parser.mly"
           ( NodeId _1 )
# 368 "src/dot_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'subgraph) in
    Obj.repr(
# 110 "src/dot_parser.mly"
           ( NodeSub _1 )
# 375 "src/dot_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Dot_ast.id) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'port_opt) in
    Obj.repr(
# 114 "src/dot_parser.mly"
              ( _1, _2 )
# 383 "src/dot_parser.ml"
               : 'node_id))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "src/dot_parser.mly"
                ( None )
# 389 "src/dot_parser.ml"
               : 'port_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'port) in
    Obj.repr(
# 119 "src/dot_parser.mly"
                ( Some _1 )
# 396 "src/dot_parser.ml"
               : 'port_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Dot_ast.id) in
    Obj.repr(
# 123 "src/dot_parser.mly"
           ( try PortC (compass_pt _2)
             with Invalid_argument _ -> PortId (_2, None) )
# 404 "src/dot_parser.ml"
               : 'port))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Dot_ast.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Dot_ast.id) in
    Obj.repr(
# 126 "src/dot_parser.mly"
      ( let cp = 
  	  try compass_pt _4 with Invalid_argument _ -> raise Parse_error 
	in
	PortId (_2, Some cp) )
# 415 "src/dot_parser.ml"
               : 'port))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "src/dot_parser.mly"
                ( [] )
# 421 "src/dot_parser.ml"
               : 'attr_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 134 "src/dot_parser.mly"
               ( _1 )
# 428 "src/dot_parser.ml"
               : 'attr_list_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'a_list) in
    Obj.repr(
# 138 "src/dot_parser.mly"
                 ( [_2] )
# 435 "src/dot_parser.ml"
               : 'attr_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'a_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attr_list) in
    Obj.repr(
# 139 "src/dot_parser.mly"
                           ( _2 :: _4 )
# 443 "src/dot_parser.ml"
               : 'attr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "src/dot_parser.mly"
                ( None )
# 449 "src/dot_parser.ml"
               : 'id_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Dot_ast.id) in
    Obj.repr(
# 144 "src/dot_parser.mly"
                ( Some _1 )
# 456 "src/dot_parser.ml"
               : 'id_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'equality) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'comma_opt) in
    Obj.repr(
# 148 "src/dot_parser.mly"
                     ( [_1] )
# 464 "src/dot_parser.ml"
               : 'a_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'equality) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'comma_opt) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'a_list) in
    Obj.repr(
# 149 "src/dot_parser.mly"
                            ( _1 :: _3 )
# 473 "src/dot_parser.ml"
               : 'a_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Dot_ast.id) in
    Obj.repr(
# 153 "src/dot_parser.mly"
     ( _1, None )
# 480 "src/dot_parser.ml"
               : 'equality))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Dot_ast.id) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Dot_ast.id) in
    Obj.repr(
# 154 "src/dot_parser.mly"
              ( _1, Some _3 )
# 488 "src/dot_parser.ml"
               : 'equality))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "src/dot_parser.mly"
                ( () )
# 494 "src/dot_parser.ml"
               : 'comma_opt))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "src/dot_parser.mly"
                ( () )
# 500 "src/dot_parser.ml"
               : 'comma_opt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Dot_ast.id) in
    Obj.repr(
# 164 "src/dot_parser.mly"
              ( SubgraphId _2 )
# 507 "src/dot_parser.ml"
               : 'subgraph))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Dot_ast.id) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 165 "src/dot_parser.mly"
                                  ( SubgraphDef (Some _2, _4) )
# 515 "src/dot_parser.ml"
               : 'subgraph))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 166 "src/dot_parser.mly"
                               ( SubgraphDef (None, _3) )
# 522 "src/dot_parser.ml"
               : 'subgraph))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 167 "src/dot_parser.mly"
                      ( SubgraphDef (None, _2) )
# 529 "src/dot_parser.ml"
               : 'subgraph))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Dot_ast.file)
