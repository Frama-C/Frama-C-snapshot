type token =
  | CALL_OF
  | RETURN_OF
  | CALLORRETURN_OF
  | IDENTIFIER of (string)
  | INT of (string)
  | LCURLY
  | RCURLY
  | LPAREN
  | RPAREN
  | LSQUARE
  | RSQUARE
  | RARROW
  | TRUE
  | FALSE
  | NOT
  | DOT
  | AMP
  | COLON
  | SEMI_COLON
  | COMMA
  | PIPE
  | EQ
  | LT
  | GT
  | LE
  | GE
  | NEQ
  | PLUS
  | MINUS
  | SLASH
  | STAR
  | PERCENT
  | OR
  | AND
  | EOF

open Parsing;;
# 28 "src/ltl_to_acsl/yaparser.mly"
open Parsing
open Promelaast
open Bool3
open Format

let observed_states      = Hashtbl.create 1
let observed_vars        = Hashtbl.create 1
let observed_funcs       = Hashtbl.create 1
let observed_expressions = Hashtbl.create 97

let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))
;;



let fetch_and_create_state name = 
  try  
    Hashtbl.find observed_states name
  with
    Not_found -> 
      let s={ name=name;
	      acceptation=False; init=False;
	      nums=(Hashtbl.length observed_states) } in
      Hashtbl.add observed_states name s;
      s
;;


# 72 "src/ltl_to_acsl/yaparser.ml"
let yytransl_const = [|
  257 (* CALL_OF *);
  258 (* RETURN_OF *);
  259 (* CALLORRETURN_OF *);
  262 (* LCURLY *);
  263 (* RCURLY *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* LSQUARE *);
  267 (* RSQUARE *);
  268 (* RARROW *);
  269 (* TRUE *);
  270 (* FALSE *);
  271 (* NOT *);
  272 (* DOT *);
  273 (* AMP *);
  274 (* COLON *);
  275 (* SEMI_COLON *);
  276 (* COMMA *);
  277 (* PIPE *);
  278 (* EQ *);
  279 (* LT *);
  280 (* GT *);
  281 (* LE *);
  282 (* GE *);
  283 (* NEQ *);
  284 (* PLUS *);
  285 (* MINUS *);
  286 (* SLASH *);
  287 (* STAR *);
  288 (* PERCENT *);
  289 (* OR *);
  290 (* AND *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  260 (* IDENTIFIER *);
  261 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\005\000\005\000\003\000\003\000\
\006\000\007\000\007\000\008\000\008\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\011\000\011\000\
\011\000\012\000\012\000\012\000\012\000\013\000\013\000\013\000\
\014\000\014\000\015\000\015\000\016\000\016\000\016\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\005\000\003\000\001\000\002\000\001\000\
\004\000\003\000\001\000\005\000\002\000\004\000\004\000\004\000\
\001\000\001\000\002\000\003\000\003\000\003\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\003\000\003\000\
\001\000\003\000\003\000\003\000\001\000\001\000\001\000\003\000\
\003\000\001\000\004\000\001\000\002\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\048\000\000\000\003\000\000\000\000\000\
\000\000\002\000\008\000\000\000\000\000\007\000\006\000\000\000\
\000\000\000\000\000\000\011\000\004\000\000\000\000\000\000\000\
\000\000\046\000\038\000\000\000\017\000\018\000\000\000\000\000\
\000\000\023\000\000\000\000\000\037\000\000\000\000\000\044\000\
\013\000\009\000\000\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
\000\000\022\000\040\000\047\000\000\000\000\000\000\000\000\000\
\000\000\024\000\025\000\026\000\027\000\028\000\029\000\031\000\
\032\000\034\000\035\000\036\000\041\000\000\000\015\000\016\000\
\014\000\012\000\000\000\043\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\006\000\016\000\011\000\019\000\020\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000"

let yysindex = "\005\000\
\240\254\000\000\014\255\000\000\009\255\000\000\020\255\053\255\
\075\255\000\000\000\000\099\255\069\255\000\000\000\000\028\255\
\006\255\100\255\239\254\000\000\000\000\123\255\130\255\132\255\
\140\255\000\000\000\000\006\255\000\000\000\000\006\255\074\255\
\073\255\000\000\107\255\080\255\000\000\133\255\141\255\000\000\
\000\000\000\000\069\255\000\000\146\255\150\255\151\255\250\254\
\138\255\008\255\000\000\074\255\133\255\145\255\006\255\006\255\
\018\255\018\255\018\255\018\255\018\255\018\255\018\255\018\255\
\018\255\018\255\018\255\152\255\018\255\000\000\149\255\170\255\
\171\255\000\000\000\000\000\000\008\255\155\255\022\255\022\255\
\018\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\172\255\000\000\000\000\
\000\000\000\000\173\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\181\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\255\119\255\000\000\091\255\035\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\050\255\144\255\000\000\000\000\063\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\036\255\092\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\179\000\000\000\176\000\000\000\143\000\
\240\255\000\000\228\255\000\000\070\000\229\255\000\000\000\000"

let yytablesize = 186
let yytable = "\049\000\
\050\000\042\000\074\000\043\000\053\000\001\000\023\000\024\000\
\025\000\026\000\027\000\048\000\008\000\028\000\051\000\003\000\
\076\000\007\000\029\000\030\000\031\000\026\000\027\000\068\000\
\077\000\081\000\055\000\056\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\032\000\012\000\079\000\080\000\
\003\000\042\000\021\000\042\000\021\000\042\000\021\000\022\000\
\032\000\030\000\042\000\030\000\099\000\050\000\055\000\056\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\045\000\013\000\045\000\
\045\000\045\000\017\000\030\000\030\000\026\000\008\000\054\000\
\018\000\052\000\030\000\030\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\039\000\020\000\039\000\020\000\039\000\015\000\041\000\
\032\000\055\000\056\000\063\000\064\000\065\000\066\000\067\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\033\000\044\000\033\000\
\057\000\058\000\059\000\060\000\061\000\062\000\090\000\091\000\
\092\000\045\000\094\000\046\000\033\000\033\000\033\000\033\000\
\033\000\033\000\075\000\047\000\068\000\071\000\069\000\033\000\
\033\000\072\000\073\000\093\000\078\000\095\000\098\000\057\000\
\058\000\059\000\060\000\061\000\062\000\039\000\039\000\039\000\
\039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\096\000\097\000\001\000\075\000\100\000\010\000\
\014\000\070\000"

let yycheck = "\028\000\
\028\000\019\001\009\001\021\001\032\000\001\000\001\001\002\001\
\003\001\004\001\005\001\028\000\004\001\008\001\031\000\032\001\
\009\001\004\001\013\001\014\001\015\001\004\001\005\001\016\001\
\052\000\008\001\033\001\034\001\057\000\058\000\059\000\060\000\
\061\000\062\000\063\000\064\000\031\001\018\001\055\000\056\000\
\032\001\007\001\007\001\009\001\009\001\011\001\019\001\020\001\
\031\001\007\001\016\001\009\001\081\000\081\000\033\001\034\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\007\001\018\001\009\001\
\010\001\011\001\006\001\033\001\034\001\004\001\004\001\007\001\
\012\001\008\001\033\001\034\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\007\001\007\001\009\001\009\001\011\001\004\001\004\001\
\031\001\033\001\034\001\028\001\029\001\030\001\031\001\032\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\007\001\004\001\009\001\
\022\001\023\001\024\001\025\001\026\001\027\001\065\000\066\000\
\067\000\008\001\069\000\008\001\022\001\023\001\024\001\025\001\
\026\001\027\001\009\001\008\001\016\001\004\001\010\001\033\001\
\034\001\004\001\004\001\004\001\012\001\009\001\004\001\022\001\
\023\001\024\001\025\001\026\001\027\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\009\001\009\001\000\000\009\001\011\001\005\000\
\009\000\043\000"

let yynames_const = "\
  CALL_OF\000\
  RETURN_OF\000\
  CALLORRETURN_OF\000\
  LCURLY\000\
  RCURLY\000\
  LPAREN\000\
  RPAREN\000\
  LSQUARE\000\
  RSQUARE\000\
  RARROW\000\
  TRUE\000\
  FALSE\000\
  NOT\000\
  DOT\000\
  AMP\000\
  COLON\000\
  SEMI_COLON\000\
  COMMA\000\
  PIPE\000\
  EQ\000\
  LT\000\
  GT\000\
  LE\000\
  GE\000\
  NEQ\000\
  PLUS\000\
  MINUS\000\
  SLASH\000\
  STAR\000\
  PERCENT\000\
  OR\000\
  AND\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'options) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'states) in
    Obj.repr(
# 85 "src/ltl_to_acsl/yaparser.mly"
                   ( 
  List.iter (fun(key, ids) -> begin
    match key with
      "init"   -> List.iter (fun id -> try 
	(Hashtbl.find observed_states id).init <- True
      with
	Not_found -> begin Ltl_to_acsl_option.error "Error: no state '%s'\n" id; exit 1 end) ids

    | "accept" -> List.iter (fun id -> try
	(Hashtbl.find observed_states id).acceptation <- True
      with Not_found -> begin Ltl_to_acsl_option.error "Error: no state '%s'\n" id; exit 1 end) ids
    | oth      -> begin Ltl_to_acsl_option.error "Error: unknown option '%s'\n" oth; exit 1 end
    ;
  end) _1
;
	   let states=
	     Hashtbl.fold (fun _ st l -> 
	       if st.acceptation=Undefined or st.init=Undefined then
		 begin
		   Ltl_to_acsl_option.fatal "Error: the state '%s' is used but never defined.\n" st.name
		 end;
	       st::l
			  ) observed_states []
	   in 
	   Data_for_ltl.setLtl_expressions observed_expressions;
	   Ltl_logic.setLtl_expressions observed_expressions;
	   let n=ref 0 in
	   let transitions = Ltl_logic.simplifyTrans _2 in
	   List.iter (fun t -> t.numt<-(!n); n:=!n+1) transitions;
	   
	   ((states , transitions),observed_vars,observed_funcs)
	)
# 316 "src/ltl_to_acsl/yaparser.ml"
               : (Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'options) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'option) in
    Obj.repr(
# 121 "src/ltl_to_acsl/yaparser.mly"
                   ( _1@[_2] )
# 324 "src/ltl_to_acsl/yaparser.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'option) in
    Obj.repr(
# 122 "src/ltl_to_acsl/yaparser.mly"
                   ( [_1] )
# 331 "src/ltl_to_acsl/yaparser.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_identifiers) in
    Obj.repr(
# 126 "src/ltl_to_acsl/yaparser.mly"
                                                        ( (_2, _4) )
# 339 "src/ltl_to_acsl/yaparser.ml"
               : 'option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'opt_identifiers) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "src/ltl_to_acsl/yaparser.mly"
                                     ( _1@[_3] )
# 347 "src/ltl_to_acsl/yaparser.ml"
               : 'opt_identifiers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "src/ltl_to_acsl/yaparser.mly"
                                     ( [_1] )
# 354 "src/ltl_to_acsl/yaparser.ml"
               : 'opt_identifiers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'states) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 139 "src/ltl_to_acsl/yaparser.mly"
                 ( _1@_2 )
# 362 "src/ltl_to_acsl/yaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 140 "src/ltl_to_acsl/yaparser.mly"
          ( _1 )
# 369 "src/ltl_to_acsl/yaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    Obj.repr(
# 145 "src/ltl_to_acsl/yaparser.mly"
                                            (
      let start_state = fetch_and_create_state _1 in
      List.map (fun(cross,stop_state) -> { start=start_state; stop=stop_state;
					   cross=cross;       numt=(-1) }) _3 )
# 380 "src/ltl_to_acsl/yaparser.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 153 "src/ltl_to_acsl/yaparser.mly"
                                ( _1@[_3] )
# 388 "src/ltl_to_acsl/yaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 154 "src/ltl_to_acsl/yaparser.mly"
               ( [_1] )
# 395 "src/ltl_to_acsl/yaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'guard) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 159 "src/ltl_to_acsl/yaparser.mly"
                                          ( (_2, fetch_and_create_state _5) )
# 403 "src/ltl_to_acsl/yaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 160 "src/ltl_to_acsl/yaparser.mly"
                      ( (PTrue, fetch_and_create_state _2) )
# 410 "src/ltl_to_acsl/yaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 167 "src/ltl_to_acsl/yaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _3) then Hashtbl.add observed_funcs _3 _3 ; PCallOrReturn _3 )
# 417 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 169 "src/ltl_to_acsl/yaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _3) then Hashtbl.add observed_funcs _3 _3 ; PCall _3 )
# 424 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 171 "src/ltl_to_acsl/yaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _3) then Hashtbl.add observed_funcs _3 _3 ; PReturn _3 )
# 431 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 173 "src/ltl_to_acsl/yaparser.mly"
            ( PTrue )
# 437 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 175 "src/ltl_to_acsl/yaparser.mly"
            ( PFalse )
# 443 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 177 "src/ltl_to_acsl/yaparser.mly"
     ( PNot _2 )
# 450 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 179 "src/ltl_to_acsl/yaparser.mly"
     ( PAnd (_1,_3) )
# 458 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 181 "src/ltl_to_acsl/yaparser.mly"
            ( POr (_1,_3) )
# 466 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'guard) in
    Obj.repr(
# 183 "src/ltl_to_acsl/yaparser.mly"
     ( _2 )
# 473 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 185 "src/ltl_to_acsl/yaparser.mly"
     ( 

	      let id = get_fresh_ident () in
	      let (pred,exp) = _1 in
	      Hashtbl.add observed_expressions id 
		(exp, (Pretty_utils.sfprintf "%a" Cil.d_exp exp), pred);
	      (*Ltlast.LIdent(id)*)

	      Hashtbl.add observed_vars id id ; 
	      PIndexedExp id  
	    )
# 490 "src/ltl_to_acsl/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 203 "src/ltl_to_acsl/yaparser.mly"
                                     (
    ( Cil_types.Prel(Cil_types.Req, Logic_utils.expr_to_term _1,
	  	                    Logic_utils.expr_to_term _3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Eq, _1 , _3 , Cil.intType)) ) )
# 501 "src/ltl_to_acsl/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 207 "src/ltl_to_acsl/yaparser.mly"
                                     (
    ( Cil_types.Prel(Cil_types.Rlt, Logic_utils.expr_to_term _1,
		                    Logic_utils.expr_to_term _3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Lt, _1 , _3 , Cil.intType)) ) )
# 512 "src/ltl_to_acsl/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 211 "src/ltl_to_acsl/yaparser.mly"
                                     ( 
    ( Cil_types.Prel(Cil_types.Rgt, Logic_utils.expr_to_term _1,
		                    Logic_utils.expr_to_term _3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Gt, _1 , _3 , Cil.intType)) ) )
# 523 "src/ltl_to_acsl/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 215 "src/ltl_to_acsl/yaparser.mly"
                                      (
    ( Cil_types.Prel(Cil_types.Rle, Logic_utils.expr_to_term _1,
		                    Logic_utils.expr_to_term _3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Le, _1 , _3 , Cil.intType)) ) )
# 534 "src/ltl_to_acsl/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 219 "src/ltl_to_acsl/yaparser.mly"
                                     (
    ( Cil_types.Prel(Cil_types.Rge, Logic_utils.expr_to_term _1,
		                    Logic_utils.expr_to_term _3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Ge, _1 , _3 , Cil.intType) )) )
# 545 "src/ltl_to_acsl/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 223 "src/ltl_to_acsl/yaparser.mly"
                                      (
    ( Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term _1,
		                    Logic_utils.expr_to_term _3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Ne, _1 , _3 , Cil.intType) )) )
# 556 "src/ltl_to_acsl/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 227 "src/ltl_to_acsl/yaparser.mly"
                   (
    ( Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term _1,
		     Logic_const.term(Cil_types.TConst(Cil_types.CInt64(Int64.of_int 0,Cil_types.IInt,Some("0"))))
		       (Cil_types.Ctype Cil.intType)), _1) )
# 566 "src/ltl_to_acsl/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 235 "src/ltl_to_acsl/yaparser.mly"
                                           (
    Cil.new_exp (Cil_types.BinOp(Cil_types.PlusA, _1 , _3 , Cil.intType)) )
# 575 "src/ltl_to_acsl/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 237 "src/ltl_to_acsl/yaparser.mly"
                                            (
    Cil.new_exp (Cil_types.BinOp(Cil_types.MinusA, _1 , _3 , Cil.intType)) )
# 584 "src/ltl_to_acsl/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 239 "src/ltl_to_acsl/yaparser.mly"
                       ( _1 )
# 591 "src/ltl_to_acsl/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 244 "src/ltl_to_acsl/yaparser.mly"
                                             (
    Cil.new_exp (Cil_types.BinOp(Cil_types.Div, _1 , _3 , Cil.intType)) )
# 600 "src/ltl_to_acsl/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 246 "src/ltl_to_acsl/yaparser.mly"
                                            (
    Cil.new_exp (Cil_types.BinOp(Cil_types.Mult, _1 , _3 , Cil.intType)) )
# 609 "src/ltl_to_acsl/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 248 "src/ltl_to_acsl/yaparser.mly"
                                               (
    Cil.new_exp (Cil_types.BinOp(Cil_types.Mod, _1 , _3 , Cil.intType)) )
# 618 "src/ltl_to_acsl/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 250 "src/ltl_to_acsl/yaparser.mly"
                    ( _1 )
# 625 "src/ltl_to_acsl/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 256 "src/ltl_to_acsl/yaparser.mly"
            ( Cil.new_exp (Cil_types.Const(Cil_types.CInt64(Int64.of_string _1,Cil_types.IInt, Some(_1)))))
# 632 "src/ltl_to_acsl/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 258 "src/ltl_to_acsl/yaparser.mly"
            ( Cil.new_exp (Cil_types.Lval(_1)) )
# 639 "src/ltl_to_acsl/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 260 "src/ltl_to_acsl/yaparser.mly"
     ( _2 )
# 646 "src/ltl_to_acsl/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 268 "src/ltl_to_acsl/yaparser.mly"
            ( 
                            
              let (my_host,my_offset) = (_1) in
              
              let new_offset = Utils_parser.add_offset my_offset (Utils_parser.get_new_offset my_host my_offset _3) in
              (my_host,new_offset)
                         
                            
                             (*Ltl_to_acsl_option.fatal "NOT YET IMPLEMENTED : A.B structure filed access." *))
# 662 "src/ltl_to_acsl/yaparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_array) in
    Obj.repr(
# 278 "src/ltl_to_acsl/yaparser.mly"
     (_1)
# 669 "src/ltl_to_acsl/yaparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 282 "src/ltl_to_acsl/yaparser.mly"
     ( Cil.addOffsetLval (Cil_types.Index (_3,Cil_types.NoOffset)) _1)
# 677 "src/ltl_to_acsl/yaparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 283 "src/ltl_to_acsl/yaparser.mly"
                    (_1)
# 684 "src/ltl_to_acsl/yaparser.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 288 "src/ltl_to_acsl/yaparser.mly"
 ( Ltl_to_acsl_option.fatal "NOT YET IMPLEMENTED : *A dereferencement access." )
# 691 "src/ltl_to_acsl/yaparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 290 "src/ltl_to_acsl/yaparser.mly"
     ( Cil.var ( Data_for_ltl.get_varinfo _1) )
# 698 "src/ltl_to_acsl/yaparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 292 "src/ltl_to_acsl/yaparser.mly"
     ( _2 )
# 705 "src/ltl_to_acsl/yaparser.ml"
               : 'access_leaf))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t))
