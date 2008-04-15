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
# 26 "src/ltl_to_acsl/ltlparser.mly"
open Parsing
open Cil_types


let observed_expressions=Hashtbl.create 97

let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))

# 53 "src/ltl_to_acsl/ltlparser.ml"
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
\006\000\006\000\006\000\007\000\007\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\003\000\002\000\002\000\003\000\003\000\
\002\000\003\000\003\000\002\000\003\000\003\000\004\000\004\000\
\004\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\003\000\001\000\
\001\000\001\000\003\000\003\000\003\000\001\000\004\000\001\000\
\002\000\002\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\003\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\043\000\
\045\000\000\000\018\000\000\000\000\000\032\000\034\000\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\042\000\041\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\035\000\044\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\020\000\021\000\022\000\023\000\024\000\026\000\
\027\000\029\000\030\000\031\000\036\000\037\000\000\000\015\000\
\016\000\017\000\000\000\039\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000"

let yysindex = "\009\000\
\088\255\000\000\000\000\000\000\088\255\088\255\088\255\088\255\
\088\255\036\255\036\255\253\254\002\255\010\255\000\000\000\000\
\000\000\053\000\000\000\140\255\092\255\000\000\000\000\048\255\
\000\000\141\255\090\255\014\255\246\254\246\254\246\254\246\254\
\036\255\000\000\000\000\240\254\247\254\254\254\088\255\088\255\
\088\255\088\255\088\255\088\255\000\000\008\255\008\255\008\255\
\008\255\008\255\008\255\008\255\008\255\008\255\008\255\008\255\
\036\255\036\255\008\255\000\000\000\000\000\000\014\255\021\255\
\030\255\037\255\023\255\023\255\023\255\004\255\246\254\246\254\
\008\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\255\000\000\
\000\000\000\000\060\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\044\000\027\000\000\000\000\000\001\000\
\000\000\000\000\074\255\119\255\063\000\068\000\073\000\078\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\007\000\008\000\093\000\083\000\088\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\015\000\000\000\255\255\000\000\074\000\004\000\000\000\
\000\000"

let yytablesize = 356
let yytable = "\036\000\
\038\000\043\000\044\000\027\000\037\000\010\000\013\000\014\000\
\028\000\001\000\073\000\042\000\038\000\034\000\035\000\043\000\
\044\000\062\000\064\000\026\000\029\000\030\000\031\000\032\000\
\088\000\065\000\028\000\039\000\040\000\041\000\042\000\010\000\
\066\000\089\000\043\000\044\000\063\000\011\000\033\000\092\000\
\090\000\015\000\016\000\025\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\045\000\067\000\068\000\069\000\
\070\000\071\000\072\000\010\000\085\000\086\000\012\000\061\000\
\000\000\011\000\000\000\005\000\000\000\000\000\016\000\091\000\
\006\000\057\000\058\000\059\000\028\000\009\000\025\000\025\000\
\025\000\025\000\007\000\000\000\000\000\025\000\025\000\008\000\
\003\000\004\000\005\000\000\000\011\000\061\000\000\000\000\000\
\006\000\007\000\008\000\000\000\000\000\009\000\000\000\000\000\
\046\000\047\000\048\000\049\000\050\000\051\000\000\000\010\000\
\052\000\053\000\054\000\055\000\056\000\011\000\012\000\013\000\
\014\000\015\000\016\000\034\000\034\000\034\000\034\000\082\000\
\083\000\084\000\034\000\034\000\087\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\060\000\039\000\040\000\041\000\042\000\000\000\000\000\000\000\
\043\000\044\000\046\000\047\000\048\000\049\000\050\000\051\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\000\038\000\038\000\038\000\
\038\000\010\000\013\000\014\000\038\000\038\000\000\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\038\000\038\000\000\000\000\000\000\000\038\000\028\000\028\000\
\028\000\028\000\028\000\000\000\000\000\000\000\028\000\028\000\
\000\000\028\000\028\000\028\000\028\000\028\000\028\000\025\000\
\025\000\025\000\025\000\025\000\000\000\000\000\000\000\025\000\
\025\000\039\000\040\000\041\000\042\000\000\000\000\000\000\000\
\043\000\044\000\012\000\012\000\012\000\012\000\012\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\009\000\009\000\009\000\009\000\009\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\008\000\008\000\
\011\000\011\000\011\000\011\000"

let yycheck = "\003\001\
\000\000\012\001\013\001\005\000\003\001\000\000\000\000\000\000\
\005\000\001\000\003\001\008\001\003\001\010\000\011\000\012\001\
\013\001\004\001\035\001\005\000\006\000\007\000\008\000\009\000\
\004\001\035\001\000\000\005\001\006\001\007\001\008\001\024\001\
\035\001\004\001\012\001\013\001\033\000\030\001\003\001\029\001\
\004\001\034\001\035\001\000\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\000\000\039\000\040\000\041\000\
\042\000\043\000\044\000\024\001\057\000\058\000\000\000\004\001\
\255\255\030\001\255\255\000\000\255\255\255\255\035\001\073\000\
\000\000\026\001\027\001\028\001\073\000\000\000\005\001\006\001\
\007\001\008\001\000\000\255\255\255\255\012\001\013\001\000\000\
\001\001\002\001\003\001\255\255\000\000\004\001\255\255\255\255\
\009\001\010\001\011\001\255\255\255\255\014\001\255\255\255\255\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\024\001\
\021\001\022\001\023\001\024\001\025\001\030\001\031\001\032\001\
\033\001\034\001\035\001\005\001\006\001\007\001\008\001\054\000\
\055\000\056\000\012\001\013\001\059\000\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\004\001\005\001\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\015\001\016\001\017\001\018\001\019\001\020\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\024\001\025\001\255\255\255\255\255\255\029\001\004\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\255\255\012\001\
\013\001\005\001\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\004\001\005\001\006\001\007\001\008\001\004\001\
\005\001\006\001\007\001\008\001\004\001\005\001\006\001\007\001\
\008\001\004\001\005\001\006\001\007\001\008\001\004\001\005\001\
\006\001\007\001\008\001\004\001\005\001\006\001\007\001\008\001\
\004\001\005\001\006\001\007\001"

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
# 82 "src/ltl_to_acsl/ltlparser.mly"
                      ((_1,observed_expressions))
# 306 "src/ltl_to_acsl/ltlparser.ml"
               : (Ltlast.formula * (string, (Cil_types.exp* string*Cil_types.predicate)) Hashtbl.t)))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "src/ltl_to_acsl/ltlparser.mly"
            (Ltlast.LTrue)
# 312 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "src/ltl_to_acsl/ltlparser.mly"
     (Ltlast.LFalse)
# 318 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 92 "src/ltl_to_acsl/ltlparser.mly"
     ( _2 )
# 325 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 95 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LGlobally(_2) )
# 332 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 97 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LFatally(_2) )
# 339 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 99 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LUntil(_1,_3) )
# 347 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 101 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LRelease(_1,_3) )
# 355 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 103 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LNext(_2) )
# 362 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 106 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LOr(_1,_3) )
# 370 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 108 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LAnd(_1,_3) )
# 378 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 110 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LNot(_2) )
# 385 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 112 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LImplies(_1,_3) )
# 393 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 114 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LIff(_1,_3) )
# 401 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 117 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LCall(_3))
# 408 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 119 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LReturn(_3))
# 415 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 121 "src/ltl_to_acsl/ltlparser.mly"
     ( Ltlast.LCallOrReturn(_3))
# 422 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 125 "src/ltl_to_acsl/ltlparser.mly"
     ( 
	      let id = get_fresh_ident () in 
	      let (pred,exp) = _1 in
	        Hashtbl.add observed_expressions id (exp, (Cil.fprintf_to_string "%a" Cil.d_exp exp), pred);
	        Ltlast.LIdent(id)
	    )
# 434 "src/ltl_to_acsl/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 137 "src/ltl_to_acsl/ltlparser.mly"
            ( (	Prel(Cil_types.Req, Logic_const.expr_to_term _1 ,Logic_const.expr_to_term  _3),
		BinOp(Cil_types.Eq, _1 , _3 , Cil.intType) )
	    )
# 444 "src/ltl_to_acsl/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 141 "src/ltl_to_acsl/ltlparser.mly"
            ( (	Prel(Cil_types.Rlt, Logic_const.expr_to_term _1 , Logic_const.expr_to_term _3),
		BinOp(Cil_types.Lt, _1 , _3 , Cil.intType) )
	    )
# 454 "src/ltl_to_acsl/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 145 "src/ltl_to_acsl/ltlparser.mly"
            ( (	Prel(Cil_types.Rgt, Logic_const.expr_to_term _1 , Logic_const.expr_to_term _3),
		BinOp(Cil_types.Gt, _1 , _3 , Cil.intType) )
	    )
# 464 "src/ltl_to_acsl/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 149 "src/ltl_to_acsl/ltlparser.mly"
            ( (	Prel(Cil_types.Rle, Logic_const.expr_to_term _1 , Logic_const.expr_to_term _3),
		BinOp(Cil_types.Le, _1 , _3 , Cil.intType) )
	    )
# 474 "src/ltl_to_acsl/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 153 "src/ltl_to_acsl/ltlparser.mly"
            ( (	Prel(Cil_types.Rge, Logic_const.expr_to_term _1 , Logic_const.expr_to_term _3),
		BinOp(Cil_types.Ge, _1 , _3 , Cil.intType) )
	    )
# 484 "src/ltl_to_acsl/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 157 "src/ltl_to_acsl/ltlparser.mly"
            ( (	Prel(Cil_types.Rneq,Logic_const.expr_to_term _1 , Logic_const.expr_to_term _3),
		BinOp(Cil_types.Ne , _1 , _3 , Cil.intType) )
	    )
# 494 "src/ltl_to_acsl/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 161 "src/ltl_to_acsl/ltlparser.mly"
     ( (	Prel(Cil_types.Rneq,Logic_const.expr_to_term _1 , 
		     Logic_const.mk_dummy_term 
		       (TConst( CInt64(Int64.of_int 0,IInt,Some("0"))))  
		       Cil.intType),
		_1)
	    )
# 506 "src/ltl_to_acsl/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 173 "src/ltl_to_acsl/ltlparser.mly"
            ( BinOp(Cil_types.PlusA, _1 , _3 , Cil.intType) )
# 514 "src/ltl_to_acsl/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 175 "src/ltl_to_acsl/ltlparser.mly"
            ( BinOp(Cil_types.MinusA, _1 , _3 , Cil.intType) )
# 522 "src/ltl_to_acsl/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 177 "src/ltl_to_acsl/ltlparser.mly"
     ( _1 )
# 529 "src/ltl_to_acsl/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 183 "src/ltl_to_acsl/ltlparser.mly"
            ( BinOp(Cil_types.Div, _1 , _3 , Cil.intType) )
# 537 "src/ltl_to_acsl/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 185 "src/ltl_to_acsl/ltlparser.mly"
            ( BinOp(Cil_types.Mult, _1 , _3 , Cil.intType) )
# 545 "src/ltl_to_acsl/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 187 "src/ltl_to_acsl/ltlparser.mly"
            ( BinOp(Cil_types.Mod, _1 , _3 , Cil.intType) )
# 553 "src/ltl_to_acsl/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 189 "src/ltl_to_acsl/ltlparser.mly"
     ( _1 )
# 560 "src/ltl_to_acsl/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 195 "src/ltl_to_acsl/ltlparser.mly"
            ( Const(CInt64(Int64.of_string _1,IInt, Some(_1))))
# 567 "src/ltl_to_acsl/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 197 "src/ltl_to_acsl/ltlparser.mly"
     ( Lval(_1) )
# 574 "src/ltl_to_acsl/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 199 "src/ltl_to_acsl/ltlparser.mly"
     ( _2 )
# 581 "src/ltl_to_acsl/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 206 "src/ltl_to_acsl/ltlparser.mly"
            ( Format.printf "NOT YET IMPLEMENTED : A->B pointed structure filed access."; assert false )
# 589 "src/ltl_to_acsl/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 208 "src/ltl_to_acsl/ltlparser.mly"
            ( Format.printf "NOT YET IMPLEMENTED : A.B structure filed access."; assert false )
# 597 "src/ltl_to_acsl/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_array) in
    Obj.repr(
# 210 "src/ltl_to_acsl/ltlparser.mly"
     (_1)
# 604 "src/ltl_to_acsl/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 214 "src/ltl_to_acsl/ltlparser.mly"
     ( Cil.addOffsetLval (Index (_3,NoOffset)) _1)
# 612 "src/ltl_to_acsl/ltlparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 220 "src/ltl_to_acsl/ltlparser.mly"
     (_1)
# 619 "src/ltl_to_acsl/ltlparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 225 "src/ltl_to_acsl/ltlparser.mly"
            ( Format.printf "NOT YET IMPLEMENTED : &A 'address of' access."; assert false )
# 626 "src/ltl_to_acsl/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 227 "src/ltl_to_acsl/ltlparser.mly"
            ( Format.printf "NOT YET IMPLEMENTED : *A dereferencement access."; assert false )
# 633 "src/ltl_to_acsl/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 229 "src/ltl_to_acsl/ltlparser.mly"
     ( Cil.var ( Data_for_ltl.get_varinfo _1) )
# 640 "src/ltl_to_acsl/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 231 "src/ltl_to_acsl/ltlparser.mly"
     ( _2 )
# 647 "src/ltl_to_acsl/ltlparser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Ltlast.formula * (string, (Cil_types.exp* string*Cil_types.predicate)) Hashtbl.t))
