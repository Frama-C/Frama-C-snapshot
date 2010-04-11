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
# 28 "src/aorai/ltlparser.mly"
open Parsing
open Cil_types
open Cil

let observed_expressions=Hashtbl.create 97

let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))

# 53 "src/aorai/ltlparser.ml"
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
\045\000\000\000\018\000\000\000\000\000\032\000\000\000\000\000\
\040\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\035\000\044\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\020\000\021\000\022\000\023\000\024\000\026\000\
\027\000\029\000\030\000\031\000\037\000\000\000\000\000\015\000\
\016\000\017\000\000\000\039\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000"

let yysindex = "\021\000\
\105\255\000\000\000\000\000\000\105\255\105\255\105\255\105\255\
\105\255\000\255\000\255\028\255\049\255\053\255\000\000\000\000\
\000\000\157\000\000\000\058\255\015\255\000\000\031\255\023\255\
\000\000\007\255\065\255\001\255\013\255\013\255\013\255\013\255\
\000\255\031\255\031\255\029\255\030\255\032\255\105\255\105\255\
\105\255\105\255\105\255\105\255\000\000\255\254\255\254\255\254\
\255\254\255\254\255\254\255\254\255\254\255\254\255\254\255\254\
\035\255\000\255\255\254\000\000\000\000\000\000\001\255\062\255\
\067\255\082\255\115\255\115\255\115\255\042\255\013\255\013\255\
\255\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\031\255\059\255\000\000\
\000\000\000\000\083\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\148\000\131\000\000\000\105\000\079\000\
\000\000\000\000\137\255\183\255\167\000\172\000\177\000\182\000\
\000\000\001\000\027\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\007\000\008\000\197\000\187\000\192\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\053\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\054\000\000\000\251\255\000\000\218\255\255\255\000\000\
\000\000"

let yytablesize = 460
let yytable = "\027\000\
\042\000\073\000\033\000\028\000\062\000\010\000\013\000\014\000\
\034\000\035\000\060\000\039\000\040\000\041\000\042\000\082\000\
\083\000\084\000\043\000\044\000\087\000\001\000\010\000\010\000\
\043\000\044\000\041\000\057\000\011\000\011\000\036\000\063\000\
\015\000\016\000\016\000\052\000\053\000\054\000\055\000\056\000\
\074\000\075\000\076\000\077\000\078\000\079\000\080\000\081\000\
\058\000\042\000\059\000\037\000\036\000\043\000\044\000\038\000\
\086\000\057\000\026\000\029\000\030\000\031\000\032\000\064\000\
\065\000\088\000\066\000\091\000\061\000\085\000\089\000\028\000\
\046\000\047\000\048\000\049\000\050\000\051\000\038\000\046\000\
\047\000\048\000\049\000\050\000\051\000\090\000\061\000\092\000\
\000\000\000\000\000\000\000\000\067\000\068\000\069\000\070\000\
\071\000\072\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\000\003\000\004\000\005\000\000\000\000\000\000\000\000\000\
\000\000\006\000\007\000\008\000\000\000\000\000\009\000\039\000\
\040\000\041\000\042\000\000\000\000\000\000\000\043\000\044\000\
\010\000\000\000\028\000\000\000\000\000\000\000\011\000\012\000\
\013\000\014\000\015\000\016\000\000\000\025\000\025\000\025\000\
\025\000\000\000\000\000\025\000\025\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\007\000\034\000\034\000\034\000\034\000\008\000\
\000\000\000\000\034\000\034\000\011\000\034\000\034\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\042\000\042\000\042\000\042\000\
\042\000\010\000\013\000\014\000\042\000\042\000\000\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\000\000\042\000\042\000\041\000\041\000\
\041\000\041\000\041\000\000\000\000\000\000\000\041\000\041\000\
\000\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\000\000\041\000\041\000\
\036\000\036\000\036\000\036\000\036\000\000\000\000\000\000\000\
\036\000\036\000\000\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\000\000\
\036\000\036\000\038\000\038\000\038\000\038\000\038\000\000\000\
\000\000\000\000\038\000\038\000\000\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
\000\000\038\000\000\000\038\000\034\000\034\000\034\000\034\000\
\034\000\000\000\000\000\000\000\034\000\034\000\000\000\034\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\000\000\000\000\000\000\034\000\028\000\028\000\
\028\000\028\000\028\000\000\000\000\000\000\000\028\000\028\000\
\000\000\028\000\028\000\028\000\028\000\028\000\028\000\025\000\
\025\000\025\000\025\000\025\000\000\000\000\000\000\000\025\000\
\025\000\039\000\040\000\041\000\042\000\000\000\000\000\000\000\
\043\000\044\000\012\000\012\000\012\000\012\000\012\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\009\000\009\000\009\000\009\000\009\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\008\000\008\000\
\011\000\011\000\011\000\011\000"

let yycheck = "\005\000\
\000\000\003\001\003\001\005\000\004\001\000\000\000\000\000\000\
\010\000\011\000\004\001\005\001\006\001\007\001\008\001\054\000\
\055\000\056\000\012\001\013\001\059\000\001\000\024\001\024\001\
\012\001\013\001\000\000\027\001\030\001\030\001\003\001\033\000\
\034\001\035\001\035\001\021\001\022\001\023\001\024\001\025\001\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\026\001\008\001\028\001\003\001\000\000\012\001\013\001\003\001\
\058\000\027\001\005\000\006\000\007\000\008\000\009\000\035\001\
\035\001\004\001\035\001\073\000\004\001\035\001\004\001\073\000\
\015\001\016\001\017\001\018\001\019\001\020\001\000\000\015\001\
\016\001\017\001\018\001\019\001\020\001\004\001\004\001\029\001\
\255\255\255\255\255\255\255\255\039\000\040\000\041\000\042\000\
\043\000\044\000\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\001\001\002\001\003\001\255\255\255\255\255\255\255\255\
\255\255\009\001\010\001\011\001\255\255\255\255\014\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\024\001\255\255\000\000\255\255\255\255\255\255\030\001\031\001\
\032\001\033\001\034\001\035\001\255\255\005\001\006\001\007\001\
\008\001\255\255\255\255\000\000\012\001\013\001\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\000\000\005\001\006\001\007\001\008\001\000\000\
\255\255\255\255\012\001\013\001\000\000\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\004\001\005\001\006\001\007\001\
\008\001\004\001\004\001\004\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\255\255\028\001\029\001\004\001\005\001\
\006\001\007\001\008\001\255\255\255\255\255\255\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\255\255\028\001\029\001\
\004\001\005\001\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\255\255\
\028\001\029\001\004\001\005\001\006\001\007\001\008\001\255\255\
\255\255\255\255\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\255\255\027\001\255\255\029\001\004\001\005\001\006\001\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\255\255\015\001\
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
# 84 "src/aorai/ltlparser.mly"
                      ((_1,observed_expressions))
# 332 "src/aorai/ltlparser.ml"
               : (Ltlast.formula * (string, (Cil_types.exp* string*Cil_types.predicate)) Hashtbl.t)))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "src/aorai/ltlparser.mly"
            (Ltlast.LTrue)
# 338 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "src/aorai/ltlparser.mly"
     (Ltlast.LFalse)
# 344 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 94 "src/aorai/ltlparser.mly"
     ( _2 )
# 351 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 97 "src/aorai/ltlparser.mly"
     ( Ltlast.LGlobally(_2) )
# 358 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 99 "src/aorai/ltlparser.mly"
     ( Ltlast.LFatally(_2) )
# 365 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 101 "src/aorai/ltlparser.mly"
     ( Ltlast.LUntil(_1,_3) )
# 373 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 103 "src/aorai/ltlparser.mly"
     ( Ltlast.LRelease(_1,_3) )
# 381 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 105 "src/aorai/ltlparser.mly"
     ( Ltlast.LNext(_2) )
# 388 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 108 "src/aorai/ltlparser.mly"
     ( Ltlast.LOr(_1,_3) )
# 396 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 110 "src/aorai/ltlparser.mly"
     ( Ltlast.LAnd(_1,_3) )
# 404 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 112 "src/aorai/ltlparser.mly"
     ( Ltlast.LNot(_2) )
# 411 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 114 "src/aorai/ltlparser.mly"
     ( Ltlast.LImplies(_1,_3) )
# 419 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 116 "src/aorai/ltlparser.mly"
     ( Ltlast.LIff(_1,_3) )
# 427 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 119 "src/aorai/ltlparser.mly"
     ( Ltlast.LCall(_3))
# 434 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 121 "src/aorai/ltlparser.mly"
     ( Ltlast.LReturn(_3))
# 441 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "src/aorai/ltlparser.mly"
     ( Ltlast.LCallOrReturn(_3))
# 448 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 127 "src/aorai/ltlparser.mly"
     (
	      let id = get_fresh_ident () in
	      let (pred,exp) = _1 in
	        Hashtbl.add observed_expressions id 
		  (exp, (Pretty_utils.sfprintf "%a" Cil.d_exp exp), pred);
	        Ltlast.LIdent(id)
	    )
# 461 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 140 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Req, Logic_utils.expr_to_term ~cast:true _1 ,Logic_utils.expr_to_term ~cast:true  _3),
		new_exp (BinOp(Cil_types.Eq, _1 , _3 , Cil.intType)) )
	    )
# 471 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 144 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rlt, Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp (BinOp(Cil_types.Lt, _1 , _3 , Cil.intType)) )
	    )
# 481 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 148 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rgt, Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp(BinOp(Cil_types.Gt, _1 , _3 , Cil.intType)) )
	    )
# 491 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 152 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rle, Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp (BinOp(Cil_types.Le, _1 , _3 , Cil.intType) ))
	    )
# 501 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 156 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rge, Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp (BinOp(Cil_types.Ge, _1 , _3 , Cil.intType) ))
	    )
# 511 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 160 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp (BinOp(Cil_types.Ne , _1 , _3 , Cil.intType) ))
	    )
# 521 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 164 "src/aorai/ltlparser.mly"
     ( (	Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true _1 ,
		     Logic_const.term
		       (TConst( CInt64(Int64.of_int 0,IInt,Some("0"))))
		       (Ctype Cil.intType)),
		_1)
	    )
# 533 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 176 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.PlusA, _1 , _3 , Cil.intType)) )
# 541 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 178 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.MinusA, _1 , _3 , Cil.intType)) )
# 549 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 180 "src/aorai/ltlparser.mly"
     ( _1 )
# 556 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 186 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.Div, _1 , _3 , Cil.intType)) )
# 564 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 188 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.Mult, _1 , _3 , Cil.intType)) )
# 572 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 190 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.Mod, _1 , _3 , Cil.intType)) )
# 580 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 192 "src/aorai/ltlparser.mly"
     ( _1 )
# 587 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 198 "src/aorai/ltlparser.mly"
            ( new_exp (Const(CInt64(Int64.of_string _1,IInt, Some(_1)))))
# 594 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 200 "src/aorai/ltlparser.mly"
            ( new_exp (Lval(_1)) )
# 601 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 202 "src/aorai/ltlparser.mly"
     ( _2 )
# 608 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 209 "src/aorai/ltlparser.mly"
            ( Aorai_option.fatal "NOT YET IMPLEMENTED : A->B pointed structure filed access." )
# 616 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 211 "src/aorai/ltlparser.mly"
            ( let (my_host,my_offset) = (_1) in
              
              let new_offset = Utils_parser.add_offset my_offset (Utils_parser.get_new_offset my_host my_offset _3) in
              (my_host,new_offset))
# 627 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_array) in
    Obj.repr(
# 216 "src/aorai/ltlparser.mly"
     (_1)
# 634 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 220 "src/aorai/ltlparser.mly"
     ( Cil.addOffsetLval (Index (_3,NoOffset)) _1)
# 642 "src/aorai/ltlparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 222 "src/aorai/ltlparser.mly"
     (_1)
# 649 "src/aorai/ltlparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 227 "src/aorai/ltlparser.mly"
            ( Aorai_option.fatal "NOT YET IMPLEMENTED : &A 'address of' access." )
# 656 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 229 "src/aorai/ltlparser.mly"
            ( Aorai_option.fatal "NOT YET IMPLEMENTED : *A dereferencement access.")
# 663 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 231 "src/aorai/ltlparser.mly"
     ( Cil.var ( Data_for_aorai.get_varinfo _1) )
# 670 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 233 "src/aorai/ltlparser.mly"
     ( _2 )
# 677 "src/aorai/ltlparser.ml"
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
