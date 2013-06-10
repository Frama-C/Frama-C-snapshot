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

open Parsing;;
let _ = parse_error;;
# 30 "src/aorai/ltlparser.mly"
open Promelaast
open Logic_ptree

let observed_expressions=Hashtbl.create 97

let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))
# 52 "src/aorai/ltlparser.ml"
let yytransl_const = [|
  257 (* LTL_TRUE *);
  258 (* LTL_FALSE *);
  259 (* LTL_LPAREN *);
  260 (* LTL_RPAREN *);
  261 (* LTL_OR *);
  262 (* LTL_IMPLIES *);
  263 (* LTL_LEFT_RIGHT_ARROW *);
  264 (* LTL_AND *);
  265 (* LTL_NOT *);
  266 (* LTL_GLOBALLY *);
  267 (* LTL_FATALLY *);
  268 (* LTL_UNTIL *);
  269 (* LTL_RELEASE *);
  270 (* LTL_NEXT *);
  271 (* LTL_EQ *);
  272 (* LTL_LT *);
  273 (* LTL_GT *);
  274 (* LTL_LE *);
  275 (* LTL_GE *);
  276 (* LTL_NEQ *);
  277 (* LTL_PLUS *);
  278 (* LTL_MINUS *);
  279 (* LTL_DIV *);
  280 (* LTL_STAR *);
  281 (* LTL_MODULO *);
  282 (* LTL_RIGHT_ARROW *);
  283 (* LTL_DOT *);
  284 (* LTL_LEFT_SQUARE *);
  285 (* LTL_RIGHT_SQUARE *);
  286 (* LTL_ADRESSE *);
  287 (* LTL_CALL *);
  288 (* LTL_RETURN *);
  289 (* LTL_CALL_OR_RETURN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  290 (* LTL_INT *);
  291 (* LTL_LABEL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\004\000\005\000\005\000\005\000\005\000\
\006\000\006\000\006\000\006\000\007\000\007\000\007\000\008\000\
\008\000\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\002\000\002\000\003\000\003\000\
\002\000\003\000\003\000\002\000\003\000\003\000\004\000\004\000\
\004\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\003\000\001\000\
\001\000\002\000\001\000\003\000\003\000\003\000\001\000\004\000\
\001\000\002\000\002\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
\044\000\046\000\000\000\018\000\000\000\000\000\032\000\000\000\
\000\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\034\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\036\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\020\000\021\000\022\000\023\000\
\024\000\026\000\027\000\029\000\030\000\031\000\037\000\038\000\
\000\000\015\000\016\000\017\000\000\000\040\000"

let yydgoto = "\002\000\
\018\000\019\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000"

let yysindex = "\010\000\
\066\255\000\000\000\000\000\000\066\255\066\255\066\255\066\255\
\066\255\224\254\002\255\002\255\027\255\031\255\063\255\000\000\
\000\000\000\000\131\000\000\000\045\255\070\255\000\000\014\255\
\243\254\000\000\102\255\067\255\012\255\039\255\039\255\039\255\
\039\255\000\000\002\255\014\255\014\255\007\255\037\255\043\255\
\066\255\066\255\066\255\066\255\066\255\066\255\000\000\001\255\
\001\255\001\255\001\255\001\255\001\255\001\255\001\255\001\255\
\001\255\001\255\046\255\054\255\001\255\000\000\000\000\000\000\
\012\255\098\255\099\255\100\255\111\255\111\255\111\255\016\255\
\039\255\039\255\001\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\082\255\000\000\000\000\000\000\108\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\122\000\105\000\000\000\079\000\
\001\000\000\000\000\000\120\255\157\255\141\000\146\000\151\000\
\156\000\000\000\000\000\027\000\053\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\007\000\008\000\171\000\
\161\000\166\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\013\000\000\000\251\255\000\000\212\255\254\255\000\000\
\000\000"

let yytablesize = 434
let yytable = "\028\000\
\039\000\034\000\029\000\075\000\035\000\010\000\013\000\014\000\
\036\000\037\000\001\000\084\000\085\000\086\000\061\000\064\000\
\089\000\027\000\030\000\031\000\032\000\033\000\010\000\044\000\
\011\000\011\000\043\000\045\000\046\000\038\000\012\000\012\000\
\065\000\039\000\016\000\017\000\017\000\059\000\060\000\059\000\
\060\000\066\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\083\000\045\000\046\000\042\000\069\000\070\000\071\000\
\072\000\073\000\074\000\048\000\049\000\050\000\051\000\052\000\
\053\000\040\000\003\000\004\000\005\000\093\000\063\000\067\000\
\029\000\000\000\006\000\007\000\008\000\068\000\035\000\009\000\
\087\000\048\000\049\000\050\000\051\000\052\000\053\000\010\000\
\088\000\011\000\054\000\055\000\056\000\057\000\058\000\012\000\
\013\000\014\000\015\000\016\000\017\000\090\000\091\000\092\000\
\028\000\062\000\041\000\042\000\043\000\044\000\094\000\063\000\
\000\000\045\000\046\000\041\000\042\000\043\000\044\000\000\000\
\000\000\025\000\045\000\046\000\025\000\025\000\025\000\025\000\
\000\000\000\000\047\000\025\000\025\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\007\000\035\000\035\000\035\000\035\000\008\000\000\000\000\000\
\035\000\035\000\011\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\039\000\039\000\039\000\
\039\000\010\000\013\000\014\000\039\000\039\000\000\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\000\000\039\000\043\000\043\000\
\043\000\043\000\043\000\000\000\000\000\000\000\043\000\043\000\
\000\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\043\000\000\000\000\000\043\000\043\000\
\042\000\042\000\042\000\042\000\042\000\000\000\000\000\000\000\
\042\000\042\000\000\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\000\000\000\000\
\042\000\042\000\035\000\035\000\035\000\035\000\035\000\000\000\
\000\000\000\000\035\000\035\000\000\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
\000\000\000\000\000\000\035\000\028\000\028\000\028\000\028\000\
\028\000\000\000\000\000\000\000\028\000\028\000\000\000\028\000\
\028\000\028\000\028\000\028\000\028\000\025\000\025\000\025\000\
\025\000\025\000\000\000\000\000\000\000\025\000\025\000\041\000\
\042\000\043\000\044\000\000\000\000\000\000\000\045\000\046\000\
\012\000\012\000\012\000\012\000\012\000\005\000\005\000\005\000\
\005\000\005\000\006\000\006\000\006\000\006\000\006\000\009\000\
\009\000\009\000\009\000\009\000\007\000\007\000\007\000\007\000\
\007\000\008\000\008\000\008\000\008\000\008\000\011\000\011\000\
\011\000\011\000"

let yycheck = "\005\000\
\000\000\034\001\005\000\003\001\003\001\000\000\000\000\000\000\
\011\000\012\000\001\000\056\000\057\000\058\000\028\001\004\001\
\061\000\005\000\006\000\007\000\008\000\009\000\022\001\008\001\
\024\001\024\001\000\000\012\001\013\001\003\001\030\001\030\001\
\035\000\003\001\034\001\035\001\035\001\026\001\027\001\026\001\
\027\001\035\001\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\012\001\013\001\000\000\041\000\042\000\043\000\
\044\000\045\000\046\000\015\001\016\001\017\001\018\001\019\001\
\020\001\003\001\001\001\002\001\003\001\075\000\004\001\035\001\
\075\000\255\255\009\001\010\001\011\001\035\001\000\000\014\001\
\035\001\015\001\016\001\017\001\018\001\019\001\020\001\022\001\
\035\001\024\001\021\001\022\001\023\001\024\001\025\001\030\001\
\031\001\032\001\033\001\034\001\035\001\004\001\004\001\004\001\
\000\000\004\001\005\001\006\001\007\001\008\001\029\001\004\001\
\255\255\012\001\013\001\005\001\006\001\007\001\008\001\255\255\
\255\255\000\000\012\001\013\001\005\001\006\001\007\001\008\001\
\255\255\255\255\000\000\012\001\013\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\000\000\005\001\006\001\007\001\008\001\000\000\255\255\255\255\
\012\001\013\001\000\000\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\006\001\007\001\
\008\001\004\001\004\001\004\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\029\001\004\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\255\255\255\255\028\001\029\001\
\004\001\005\001\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\255\255\255\255\
\028\001\029\001\004\001\005\001\006\001\007\001\008\001\255\255\
\255\255\255\255\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\255\255\255\255\255\255\029\001\004\001\005\001\006\001\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\004\001\005\001\006\001\
\007\001\008\001\255\255\255\255\255\255\012\001\013\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\004\001\005\001\006\001\007\001\008\001\004\001\005\001\006\001\
\007\001\008\001\004\001\005\001\006\001\007\001\008\001\004\001\
\005\001\006\001\007\001\008\001\004\001\005\001\006\001\007\001\
\008\001\004\001\005\001\006\001\007\001\008\001\004\001\005\001\
\006\001\007\001"

let yynames_const = "\
  LTL_TRUE\000\
  LTL_FALSE\000\
  LTL_LPAREN\000\
  LTL_RPAREN\000\
  LTL_OR\000\
  LTL_IMPLIES\000\
  LTL_LEFT_RIGHT_ARROW\000\
  LTL_AND\000\
  LTL_NOT\000\
  LTL_GLOBALLY\000\
  LTL_FATALLY\000\
  LTL_UNTIL\000\
  LTL_RELEASE\000\
  LTL_NEXT\000\
  LTL_EQ\000\
  LTL_LT\000\
  LTL_GT\000\
  LTL_LE\000\
  LTL_GE\000\
  LTL_NEQ\000\
  LTL_PLUS\000\
  LTL_MINUS\000\
  LTL_DIV\000\
  LTL_STAR\000\
  LTL_MODULO\000\
  LTL_RIGHT_ARROW\000\
  LTL_DOT\000\
  LTL_LEFT_SQUARE\000\
  LTL_RIGHT_SQUARE\000\
  LTL_ADRESSE\000\
  LTL_CALL\000\
  LTL_RETURN\000\
  LTL_CALL_OR_RETURN\000\
  EOF\000\
  "

let yynames_block = "\
  LTL_INT\000\
  LTL_LABEL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 84 "src/aorai/ltlparser.mly"
                      ((_1,observed_expressions))
# 325 "src/aorai/ltlparser.ml"
               : (Ltlast.formula * (string, (Logic_ptree.relation *  Promelaast.expression * Promelaast.expression)) Hashtbl.t)))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "src/aorai/ltlparser.mly"
            (Ltlast.LTrue)
# 331 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "src/aorai/ltlparser.mly"
     (Ltlast.LFalse)
# 337 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 94 "src/aorai/ltlparser.mly"
     ( _2 )
# 344 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 97 "src/aorai/ltlparser.mly"
     ( Ltlast.LGlobally(_2) )
# 351 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 99 "src/aorai/ltlparser.mly"
     ( Ltlast.LFatally(_2) )
# 358 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 101 "src/aorai/ltlparser.mly"
     ( Ltlast.LUntil(_1,_3) )
# 366 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 103 "src/aorai/ltlparser.mly"
     ( Ltlast.LRelease(_1,_3) )
# 374 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 105 "src/aorai/ltlparser.mly"
     ( Ltlast.LNext(_2) )
# 381 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 108 "src/aorai/ltlparser.mly"
     ( Ltlast.LOr(_1,_3) )
# 389 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 110 "src/aorai/ltlparser.mly"
     ( Ltlast.LAnd(_1,_3) )
# 397 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 112 "src/aorai/ltlparser.mly"
     ( Ltlast.LNot(_2) )
# 404 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 114 "src/aorai/ltlparser.mly"
     ( Ltlast.LImplies(_1,_3) )
# 412 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 116 "src/aorai/ltlparser.mly"
     ( Ltlast.LIff(_1,_3) )
# 420 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 119 "src/aorai/ltlparser.mly"
     ( Ltlast.LCall(_3))
# 427 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 121 "src/aorai/ltlparser.mly"
     ( Ltlast.LReturn(_3))
# 434 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "src/aorai/ltlparser.mly"
     ( Ltlast.LCallOrReturn(_3))
# 441 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 127 "src/aorai/ltlparser.mly"
     (
	      let id = get_fresh_ident () in
	        Hashtbl.add observed_expressions id _1;
	        Ltlast.LIdent(id)
	    )
# 452 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 135 "src/aorai/ltlparser.mly"
                                         ( Eq, _1 , _3)
# 460 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 136 "src/aorai/ltlparser.mly"
                                         ( Lt, _1, _3 )
# 468 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 137 "src/aorai/ltlparser.mly"
                                         ( Gt, _1, _3 )
# 476 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 138 "src/aorai/ltlparser.mly"
                                         ( Le, _1, _3 )
# 484 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 139 "src/aorai/ltlparser.mly"
                                         ( Ge, _1, _3 )
# 492 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 140 "src/aorai/ltlparser.mly"
                                         ( Neq, _1, _3 )
# 500 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 141 "src/aorai/ltlparser.mly"
                  ( Neq, _1, PCst (IntConstant "0") )
# 507 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 145 "src/aorai/ltlparser.mly"
                                                     ( PBinop(Badd,_1,_3) )
# 515 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 146 "src/aorai/ltlparser.mly"
                                               ( PBinop(Bsub,_1,_3) )
# 523 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 147 "src/aorai/ltlparser.mly"
                      ( _1 )
# 530 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 152 "src/aorai/ltlparser.mly"
                                              ( PBinop(Bdiv,_1,_3) )
# 538 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 153 "src/aorai/ltlparser.mly"
                                               ( PBinop(Bmul,_1,_3) )
# 546 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 154 "src/aorai/ltlparser.mly"
                                                 ( PBinop(Bmod,_1,_3))
# 554 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 155 "src/aorai/ltlparser.mly"
                   ( _1 )
# 561 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "src/aorai/ltlparser.mly"
                  ( PCst (IntConstant _1) )
# 568 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 161 "src/aorai/ltlparser.mly"
                            ( PUnop (Uminus,PCst (IntConstant _2)) )
# 575 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 162 "src/aorai/ltlparser.mly"
          ( _1 )
# 582 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 163 "src/aorai/ltlparser.mly"
                                        ( _2 )
# 589 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 169 "src/aorai/ltlparser.mly"
                                    ( PField (PUnop(Ustar,_1),_3) )
# 597 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 170 "src/aorai/ltlparser.mly"
                            ( PField(_1,_3) )
# 605 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_array) in
    Obj.repr(
# 171 "src/aorai/ltlparser.mly"
                (_1)
# 612 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 175 "src/aorai/ltlparser.mly"
     ( PArrget(_1,_3) )
# 620 "src/aorai/ltlparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 176 "src/aorai/ltlparser.mly"
                   (_1)
# 627 "src/aorai/ltlparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 180 "src/aorai/ltlparser.mly"
                             ( PUnop (Uamp,_2) )
# 634 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 181 "src/aorai/ltlparser.mly"
                   ( PUnop (Ustar, _2 ) )
# 641 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 182 "src/aorai/ltlparser.mly"
             ( PVar _1 )
# 648 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 183 "src/aorai/ltlparser.mly"
                                ( _2 )
# 655 "src/aorai/ltlparser.ml"
               : 'access_leaf))
(* Entry ltl *)
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
let ltl (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Ltlast.formula * (string, (Logic_ptree.relation *  Promelaast.expression * Promelaast.expression)) Hashtbl.t))
