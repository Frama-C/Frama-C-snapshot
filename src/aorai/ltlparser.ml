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

let new_exp =  new_exp ~loc:(CurrentLoc.get())(*TODO: give a proper loc*)
# 54 "src/aorai/ltlparser.ml"
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
\024\000\026\000\027\000\029\000\030\000\031\000\038\000\000\000\
\000\000\015\000\016\000\017\000\000\000\040\000"

let yydgoto = "\002\000\
\018\000\019\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000"

let yysindex = "\004\000\
\079\255\000\000\000\000\000\000\079\255\079\255\079\255\079\255\
\079\255\234\254\048\255\048\255\011\255\021\255\037\255\000\000\
\000\000\000\000\157\000\000\000\100\255\137\255\000\000\025\255\
\241\254\000\000\139\255\000\255\254\254\026\255\026\255\026\255\
\026\255\000\000\048\255\025\255\025\255\024\255\028\255\036\255\
\079\255\079\255\079\255\079\255\079\255\079\255\000\000\072\255\
\072\255\072\255\072\255\072\255\072\255\072\255\072\255\072\255\
\072\255\072\255\039\255\048\255\072\255\000\000\000\000\000\000\
\254\254\056\255\073\255\080\255\029\255\029\255\029\255\087\255\
\026\255\026\255\072\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\025\255\
\047\255\000\000\000\000\000\000\081\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\148\000\131\000\000\000\105\000\
\079\000\000\000\000\000\049\255\117\255\167\000\172\000\177\000\
\182\000\000\000\000\000\001\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\007\000\008\000\197\000\
\187\000\192\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\053\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\023\000\000\000\251\255\000\000\221\255\254\255\000\000\
\000\000"

let yytablesize = 460
let yytable = "\028\000\
\043\000\064\000\029\000\063\000\001\000\010\000\013\000\014\000\
\036\000\037\000\060\000\034\000\061\000\038\000\048\000\049\000\
\050\000\051\000\052\000\053\000\084\000\085\000\086\000\039\000\
\059\000\089\000\042\000\027\000\030\000\031\000\032\000\033\000\
\065\000\041\000\042\000\043\000\044\000\045\000\046\000\040\000\
\045\000\046\000\076\000\077\000\078\000\079\000\080\000\081\000\
\082\000\083\000\035\000\059\000\037\000\025\000\025\000\025\000\
\025\000\088\000\066\000\090\000\025\000\025\000\067\000\069\000\
\070\000\071\000\072\000\073\000\074\000\093\000\068\000\011\000\
\029\000\087\000\075\000\094\000\091\000\012\000\039\000\003\000\
\004\000\005\000\017\000\092\000\063\000\000\000\000\000\006\000\
\007\000\008\000\000\000\000\000\009\000\010\000\044\000\011\000\
\000\000\000\000\045\000\046\000\010\000\012\000\011\000\000\000\
\035\000\016\000\017\000\000\000\012\000\013\000\014\000\015\000\
\016\000\017\000\048\000\049\000\050\000\051\000\052\000\053\000\
\000\000\035\000\035\000\035\000\035\000\000\000\000\000\000\000\
\035\000\035\000\028\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\062\000\041\000\
\042\000\043\000\044\000\025\000\000\000\000\000\045\000\046\000\
\000\000\000\000\000\000\000\000\047\000\054\000\055\000\056\000\
\057\000\058\000\000\000\000\000\000\000\000\000\012\000\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000\000\000\
\006\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\007\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\043\000\043\000\043\000\043\000\
\043\000\010\000\013\000\014\000\043\000\043\000\000\000\043\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\000\000\043\000\043\000\042\000\042\000\
\042\000\042\000\042\000\000\000\000\000\000\000\042\000\042\000\
\000\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\000\000\042\000\042\000\
\037\000\037\000\037\000\037\000\037\000\000\000\000\000\000\000\
\037\000\037\000\000\000\037\000\037\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\000\000\
\037\000\037\000\039\000\039\000\039\000\039\000\039\000\000\000\
\000\000\000\000\039\000\039\000\000\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\000\000\039\000\000\000\039\000\035\000\035\000\035\000\035\000\
\035\000\000\000\000\000\000\000\035\000\035\000\000\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\000\000\000\000\000\000\035\000\028\000\028\000\
\028\000\028\000\028\000\000\000\000\000\000\000\028\000\028\000\
\000\000\028\000\028\000\028\000\028\000\028\000\028\000\025\000\
\025\000\025\000\025\000\025\000\000\000\000\000\000\000\025\000\
\025\000\041\000\042\000\043\000\044\000\000\000\000\000\000\000\
\045\000\046\000\012\000\012\000\012\000\012\000\012\000\005\000\
\005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\009\000\009\000\009\000\009\000\009\000\007\000\007\000\
\007\000\007\000\007\000\008\000\008\000\008\000\008\000\008\000\
\011\000\011\000\011\000\011\000"

let yycheck = "\005\000\
\000\000\004\001\005\000\004\001\001\000\000\000\000\000\000\000\
\011\000\012\000\026\001\034\001\028\001\003\001\015\001\016\001\
\017\001\018\001\019\001\020\001\056\000\057\000\058\000\003\001\
\027\001\061\000\000\000\005\000\006\000\007\000\008\000\009\000\
\035\000\005\001\006\001\007\001\008\001\012\001\013\001\003\001\
\012\001\013\001\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\003\001\027\001\000\000\005\001\006\001\007\001\
\008\001\060\000\035\001\004\001\012\001\013\001\035\001\041\000\
\042\000\043\000\044\000\045\000\046\000\075\000\035\001\024\001\
\075\000\035\001\003\001\029\001\004\001\030\001\000\000\001\001\
\002\001\003\001\035\001\004\001\004\001\255\255\255\255\009\001\
\010\001\011\001\255\255\255\255\014\001\022\001\008\001\024\001\
\255\255\255\255\012\001\013\001\022\001\030\001\024\001\255\255\
\000\000\034\001\035\001\255\255\030\001\031\001\032\001\033\001\
\034\001\035\001\015\001\016\001\017\001\018\001\019\001\020\001\
\255\255\005\001\006\001\007\001\008\001\255\255\255\255\255\255\
\012\001\013\001\000\000\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\004\001\005\001\
\006\001\007\001\008\001\000\000\255\255\255\255\012\001\013\001\
\255\255\255\255\255\255\255\255\000\000\021\001\022\001\023\001\
\024\001\025\001\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
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
# 85 "src/aorai/ltlparser.mly"
                      ((_1,observed_expressions))
# 333 "src/aorai/ltlparser.ml"
               : (Ltlast.formula * (string, (Cil_types.exp* string*Cil_types.predicate)) Hashtbl.t)))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "src/aorai/ltlparser.mly"
            (Ltlast.LTrue)
# 339 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "src/aorai/ltlparser.mly"
     (Ltlast.LFalse)
# 345 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formula) in
    Obj.repr(
# 95 "src/aorai/ltlparser.mly"
     ( _2 )
# 352 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 98 "src/aorai/ltlparser.mly"
     ( Ltlast.LGlobally(_2) )
# 359 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 100 "src/aorai/ltlparser.mly"
     ( Ltlast.LFatally(_2) )
# 366 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 102 "src/aorai/ltlparser.mly"
     ( Ltlast.LUntil(_1,_3) )
# 374 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 104 "src/aorai/ltlparser.mly"
     ( Ltlast.LRelease(_1,_3) )
# 382 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 106 "src/aorai/ltlparser.mly"
     ( Ltlast.LNext(_2) )
# 389 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 109 "src/aorai/ltlparser.mly"
     ( Ltlast.LOr(_1,_3) )
# 397 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 111 "src/aorai/ltlparser.mly"
     ( Ltlast.LAnd(_1,_3) )
# 405 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 113 "src/aorai/ltlparser.mly"
     ( Ltlast.LNot(_2) )
# 412 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 115 "src/aorai/ltlparser.mly"
     ( Ltlast.LImplies(_1,_3) )
# 420 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formula) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formula) in
    Obj.repr(
# 117 "src/aorai/ltlparser.mly"
     ( Ltlast.LIff(_1,_3) )
# 428 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 120 "src/aorai/ltlparser.mly"
     ( Ltlast.LCall(_3))
# 435 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 122 "src/aorai/ltlparser.mly"
     ( Ltlast.LReturn(_3))
# 442 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 124 "src/aorai/ltlparser.mly"
     ( Ltlast.LCallOrReturn(_3))
# 449 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 128 "src/aorai/ltlparser.mly"
     (
	      let id = get_fresh_ident () in
	      let (pred,exp) = _1 in
	        Hashtbl.add observed_expressions id 
		  (exp, (Pretty_utils.sfprintf "%a" Cil.d_exp exp), pred);
	        Ltlast.LIdent(id)
	    )
# 462 "src/aorai/ltlparser.ml"
               : 'formula))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 141 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Req, Logic_utils.expr_to_term ~cast:true _1 ,Logic_utils.expr_to_term ~cast:true  _3),
		new_exp (BinOp(Cil_types.Eq, _1 , _3 , Cil.intType)) )
	    )
# 472 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 145 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rlt, Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp (BinOp(Cil_types.Lt, _1 , _3 , Cil.intType)) )
	    )
# 482 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 149 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rgt, Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp(BinOp(Cil_types.Gt, _1 , _3 , Cil.intType)) )
	    )
# 492 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 153 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rle, Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp (BinOp(Cil_types.Le, _1 , _3 , Cil.intType) ))
	    )
# 502 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 157 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rge, Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp (BinOp(Cil_types.Ge, _1 , _3 , Cil.intType) ))
	    )
# 512 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 161 "src/aorai/ltlparser.mly"
            ( (	Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true _1 , Logic_utils.expr_to_term ~cast:true _3),
		new_exp (BinOp(Cil_types.Ne , _1 , _3 , Cil.intType) ))
	    )
# 522 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 165 "src/aorai/ltlparser.mly"
     ( (	Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true _1 ,
		     Logic_const.term
		       (TConst( CInt64(Int64.of_int 0,IInt,Some("0"))))
		       (Ctype Cil.intType)),
		_1)
	    )
# 534 "src/aorai/ltlparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 177 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.PlusA, _1 , _3 , Cil.intType)) )
# 542 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 179 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.MinusA, _1 , _3 , Cil.intType)) )
# 550 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 181 "src/aorai/ltlparser.mly"
     ( _1 )
# 557 "src/aorai/ltlparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 187 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.Div, _1 , _3 , Cil.intType)) )
# 565 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 189 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.Mult, _1 , _3 , Cil.intType)) )
# 573 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 191 "src/aorai/ltlparser.mly"
            ( new_exp (BinOp(Cil_types.Mod, _1 , _3 , Cil.intType)) )
# 581 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 193 "src/aorai/ltlparser.mly"
     ( _1 )
# 588 "src/aorai/ltlparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 199 "src/aorai/ltlparser.mly"
            ( new_exp (Const(CInt64(Int64.of_string _1,IInt, Some(_1)))))
# 595 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 201 "src/aorai/ltlparser.mly"
            ( new_exp (Const(CInt64(Int64.of_string ("-"^_2),IInt, Some("-"^_2)))))
# 602 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 203 "src/aorai/ltlparser.mly"
            ( new_exp (Lval(_1)) )
# 609 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 205 "src/aorai/ltlparser.mly"
     ( _2 )
# 616 "src/aorai/ltlparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 212 "src/aorai/ltlparser.mly"
            ( Aorai_option.fatal "NOT YET IMPLEMENTED : A->B pointed structure filed access." )
# 624 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 214 "src/aorai/ltlparser.mly"
            ( let (my_host,my_offset) = (_1) in
              
              let new_offset = Utils_parser.add_offset my_offset (Utils_parser.get_new_offset my_host my_offset _3) in
              (my_host,new_offset))
# 635 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_array) in
    Obj.repr(
# 219 "src/aorai/ltlparser.mly"
     (_1)
# 642 "src/aorai/ltlparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 223 "src/aorai/ltlparser.mly"
     ( Cil.addOffsetLval (Index (_3,NoOffset)) _1)
# 650 "src/aorai/ltlparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 225 "src/aorai/ltlparser.mly"
     (_1)
# 657 "src/aorai/ltlparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 230 "src/aorai/ltlparser.mly"
            ( Aorai_option.fatal "NOT YET IMPLEMENTED : &A 'address of' access." )
# 664 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 232 "src/aorai/ltlparser.mly"
            ( Aorai_option.fatal "NOT YET IMPLEMENTED : *A dereferencement access.")
# 671 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 234 "src/aorai/ltlparser.mly"
     ( Cil.var ( Data_for_aorai.get_varinfo _1) )
# 678 "src/aorai/ltlparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 236 "src/aorai/ltlparser.mly"
     ( _2 )
# 685 "src/aorai/ltlparser.ml"
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
