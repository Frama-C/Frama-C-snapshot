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
  | FUNC
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
  | OTHERWISE
  | EOF

open Parsing;;
# 28 "src/aorai/yaparser.mly"
open Parsing
open Promelaast
open Bool3
open Format

type trans = Pred of Promelaast.condition | Otherwise

let observed_states      = Hashtbl.create 1
let prefetched_states    = Hashtbl.create 1
let observed_vars        = Hashtbl.create 1
let observed_funcs       = Hashtbl.create 1
let observed_expressions = Hashtbl.create 97

(* Current observed expr contains : *)
type observed_expr = Func_ret of string                      (* func name : a return of the given func *)
		     | Func_param of string * (string list)  (* Func name * param : a call with given param *)
		     | Only_vars                             (* Only constants and variables *)

let observed_expr_is_param = ref Only_vars


let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))
;;



let fetch_and_create_state name =
  Hashtbl.remove prefetched_states name ;
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

let prefetch_and_create_state name =
    if (Hashtbl.mem prefetched_states name) or not (Hashtbl.mem observed_states name) then
      begin
	let s= fetch_and_create_state name in 
	Hashtbl.add prefetched_states name name;
	s
      end 
    else
      (fetch_and_create_state name)
;;

(*TODO: give a proper loc*)
let new_exp =  Cil.new_exp ~loc:(Cil.CurrentLoc.get())

# 99 "src/aorai/yaparser.ml"
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
  271 (* FUNC *);
  272 (* NOT *);
  273 (* DOT *);
  274 (* AMP *);
  275 (* COLON *);
  276 (* SEMI_COLON *);
  277 (* COMMA *);
  278 (* PIPE *);
  279 (* EQ *);
  280 (* LT *);
  281 (* GT *);
  282 (* LE *);
  283 (* GE *);
  284 (* NEQ *);
  285 (* PLUS *);
  286 (* MINUS *);
  287 (* SLASH *);
  288 (* STAR *);
  289 (* PERCENT *);
  290 (* OR *);
  291 (* AND *);
  292 (* OTHERWISE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  260 (* IDENTIFIER *);
  261 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\005\000\005\000\003\000\003\000\
\006\000\007\000\007\000\008\000\008\000\008\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\011\000\
\011\000\011\000\012\000\012\000\012\000\012\000\013\000\013\000\
\013\000\013\000\014\000\014\000\014\000\015\000\015\000\015\000\
\015\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\005\000\003\000\001\000\002\000\001\000\
\004\000\003\000\001\000\005\000\003\000\002\000\004\000\004\000\
\004\000\001\000\001\000\002\000\003\000\003\000\003\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\003\000\001\000\001\000\002\000\
\001\000\003\000\003\000\004\000\001\000\002\000\004\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\050\000\000\000\003\000\000\000\000\000\
\000\000\002\000\008\000\000\000\000\000\007\000\006\000\000\000\
\000\000\000\000\000\000\000\000\011\000\004\000\000\000\000\000\
\000\000\000\000\000\000\039\000\000\000\018\000\019\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\038\000\000\000\
\045\000\014\000\000\000\009\000\000\000\005\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\040\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\010\000\000\000\000\000\000\000\000\000\023\000\
\042\000\049\000\000\000\000\000\000\000\000\000\000\000\025\000\
\026\000\027\000\028\000\029\000\030\000\032\000\033\000\035\000\
\036\000\037\000\000\000\043\000\016\000\017\000\015\000\047\000\
\012\000\000\000\044\000"

let yydgoto = "\002\000\
\004\000\005\000\009\000\006\000\016\000\011\000\020\000\021\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000"

let yysindex = "\003\000\
\248\254\000\000\026\255\000\000\254\254\000\000\025\255\030\255\
\051\255\000\000\000\000\076\255\015\255\000\000\000\000\243\254\
\010\255\078\255\073\255\239\254\000\000\000\000\080\255\084\255\
\085\255\103\255\094\255\000\000\010\255\000\000\000\000\010\255\
\115\255\086\255\022\255\000\000\127\255\141\255\000\000\031\255\
\000\000\000\000\111\255\000\000\015\255\000\000\118\255\142\255\
\143\255\139\255\082\255\127\255\031\255\000\000\000\000\086\255\
\031\255\167\255\010\255\010\255\057\255\057\255\057\255\057\255\
\057\255\057\255\057\255\057\255\057\255\057\255\057\255\057\255\
\176\255\000\000\000\000\172\255\173\255\174\255\180\255\000\000\
\000\000\000\000\000\255\181\255\029\255\029\255\057\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\175\255\000\000\000\000\000\000\000\000\000\000\
\000\000\178\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\188\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\043\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\079\255\114\255\000\000\072\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\110\255\134\255\000\000\000\000\000\000\
\101\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\255\036\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\184\000\000\000\181\000\000\000\146\000\
\243\255\000\000\227\255\000\000\106\000\228\255\000\000"

let yytablesize = 191
let yytable = "\052\000\
\053\000\008\000\044\000\001\000\045\000\057\000\022\000\023\000\
\082\000\072\000\024\000\025\000\026\000\027\000\028\000\051\000\
\073\000\029\000\054\000\022\000\017\000\022\000\030\000\031\000\
\003\000\032\000\018\000\083\000\058\000\007\000\003\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\033\000\
\072\000\034\000\021\000\012\000\021\000\085\000\086\000\073\000\
\013\000\048\000\019\000\048\000\048\000\048\000\008\000\059\000\
\060\000\106\000\053\000\048\000\027\000\028\000\059\000\060\000\
\087\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\041\000\015\000\
\041\000\042\000\041\000\046\000\043\000\031\000\033\000\031\000\
\034\000\027\000\080\000\047\000\048\000\056\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\046\000\050\000\046\000\049\000\046\000\
\031\000\031\000\074\000\059\000\060\000\034\000\031\000\055\000\
\034\000\076\000\034\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
\034\000\034\000\034\000\034\000\034\000\034\000\041\000\031\000\
\031\000\077\000\078\000\034\000\034\000\061\000\062\000\063\000\
\064\000\065\000\066\000\079\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\067\000\068\000\069\000\070\000\071\000\096\000\097\000\
\098\000\099\000\084\000\100\000\101\000\102\000\103\000\104\000\
\105\000\107\000\081\000\001\000\010\000\014\000\075\000"

let yycheck = "\029\000\
\029\000\004\001\020\001\001\000\022\001\034\000\020\001\021\001\
\009\001\010\001\001\001\002\001\003\001\004\001\005\001\029\000\
\017\001\008\001\032\000\007\001\006\001\009\001\013\001\014\001\
\033\001\016\001\012\001\056\000\007\001\004\001\033\001\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\030\001\
\010\001\032\001\007\001\019\001\009\001\059\000\060\000\017\001\
\019\001\007\001\036\001\009\001\010\001\011\001\004\001\034\001\
\035\001\087\000\087\000\017\001\004\001\005\001\034\001\035\001\
\008\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\007\001\004\001\
\009\001\004\001\011\001\004\001\012\001\007\001\030\001\009\001\
\032\001\004\001\009\001\008\001\008\001\008\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\007\001\015\001\009\001\008\001\011\001\
\034\001\035\001\004\001\034\001\035\001\032\001\009\001\005\001\
\007\001\004\001\009\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\023\001\024\001\025\001\026\001\027\001\028\001\009\001\034\001\
\035\001\004\001\004\001\034\001\035\001\023\001\024\001\025\001\
\026\001\027\001\028\001\017\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\029\001\030\001\031\001\032\001\033\001\069\000\070\000\
\071\000\072\000\012\001\004\001\009\001\009\001\009\001\004\001\
\004\001\011\001\009\001\000\000\005\000\009\000\045\000"

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
  FUNC\000\
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
  OTHERWISE\000\
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
# 116 "src/aorai/yaparser.mly"
                   (
  List.iter
    (fun(key, ids) ->
       match key with
           "init"   ->
             List.iter
               (fun id -> try
	          (Hashtbl.find observed_states id).init <- True
                with
	            Not_found ->
                      Aorai_option.abort "Error: no state '%s'\n" id)
               ids
         | "accept" ->
             List.iter
               (fun id -> try
	          (Hashtbl.find observed_states id).acceptation <- True
                with Not_found ->
                  Aorai_option.abort "no state '%s'\n" id) ids
         | oth      ->
             Aorai_option.abort "unknown option '%s'\n" oth
    ) _1
    ;
    let states=
      Hashtbl.fold
        (fun _ st l ->
	   if st.acceptation=Undefined or st.init=Undefined then
	     begin
	       Aorai_option.abort
                 "Error: the state '%s' is used but never defined.\n" st.name
	     end;
	   st::l)
        observed_states []
    in
    if Hashtbl.length prefetched_states >0 then 
      begin
	let r = Hashtbl.fold
	  (fun s n _ -> s^"Error: the state '"^n^"' is used but never defined.\n")
	  prefetched_states 
	  ""
	in
	Aorai_option.abort "%s" r
      end;
  
    Data_for_aorai.setLtl_expressions observed_expressions;
    Logic_simplification.setLtl_expressions observed_expressions;
    let n=ref 0 in
    let (transitions,pcondsl) = Logic_simplification.simplifyTrans _2 in
    let conds = Array.make (List.length transitions) [] in
    List.iter2 (fun t pc -> t.numt<-(!n); conds.(!n)<-pc; n:=!n+1) transitions pcondsl;
    Data_for_aorai.setCondOfParametrizedTransition conds;


    ((states , transitions),observed_vars,observed_funcs)
  )
# 374 "src/aorai/yaparser.ml"
               : (Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'options) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'option) in
    Obj.repr(
# 174 "src/aorai/yaparser.mly"
                   ( _1@[_2] )
# 382 "src/aorai/yaparser.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'option) in
    Obj.repr(
# 175 "src/aorai/yaparser.mly"
                   ( [_1] )
# 389 "src/aorai/yaparser.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_identifiers) in
    Obj.repr(
# 179 "src/aorai/yaparser.mly"
                                                        ( (_2, _4) )
# 397 "src/aorai/yaparser.ml"
               : 'option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'opt_identifiers) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 183 "src/aorai/yaparser.mly"
                                     ( _1@[_3] )
# 405 "src/aorai/yaparser.ml"
               : 'opt_identifiers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 184 "src/aorai/yaparser.mly"
                                     ( [_1] )
# 412 "src/aorai/yaparser.ml"
               : 'opt_identifiers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'states) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 192 "src/aorai/yaparser.mly"
                 ( _1@_2 )
# 420 "src/aorai/yaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 193 "src/aorai/yaparser.mly"
          ( _1 )
# 427 "src/aorai/yaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    Obj.repr(
# 198 "src/aorai/yaparser.mly"
                                            (
      let start_state = fetch_and_create_state _1 in
      let (all_conds, otherwise, transitions) =
        List.fold_left
          (fun (all_conds, otherwise, transitions) (cross,stop_state) ->
             match otherwise, cross with
                 None, Pred cross ->
                   (POr (cross, all_conds), otherwise,
                    { start=start_state; stop=stop_state;
	              cross=cross;       numt=(-1) }::transitions)
               | None, Otherwise ->
                   let trans = { start=start_state; stop=stop_state;
                                 cross = PFalse; numt= (-1) }
                   in
                   (all_conds, Some trans, trans::transitions)
               | Some _, _ ->
                   Aorai_option.abort
                     "'other' directive in definition of %s \
                      transitions is not the last one" start_state.name)
          (PFalse,None,[]) _3
      in
      match otherwise with
          None -> List.rev transitions
        | Some trans ->
            List.rev
            ({trans with cross = PNot all_conds} ::
              (List.filter (fun x -> x != trans) transitions))
  )
# 462 "src/aorai/yaparser.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'transitions) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 230 "src/aorai/yaparser.mly"
                                ( _1@[_3] )
# 470 "src/aorai/yaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 231 "src/aorai/yaparser.mly"
               ( [_1] )
# 477 "src/aorai/yaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'guard) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 236 "src/aorai/yaparser.mly"
                                          ( (Pred _2, prefetch_and_create_state _5) )
# 485 "src/aorai/yaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 237 "src/aorai/yaparser.mly"
                                ((Otherwise, prefetch_and_create_state _3) )
# 492 "src/aorai/yaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 238 "src/aorai/yaparser.mly"
                      ( (Pred PTrue, prefetch_and_create_state _2) )
# 499 "src/aorai/yaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 245 "src/aorai/yaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _3) then Hashtbl.add observed_funcs _3 _3 ; PCallOrReturn _3 )
# 506 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 247 "src/aorai/yaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _3) then Hashtbl.add observed_funcs _3 _3 ; PCall _3 )
# 513 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 249 "src/aorai/yaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _3) then Hashtbl.add observed_funcs _3 _3 ; PReturn _3 )
# 520 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 251 "src/aorai/yaparser.mly"
            ( PTrue )
# 526 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 253 "src/aorai/yaparser.mly"
            ( PFalse )
# 532 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 255 "src/aorai/yaparser.mly"
     ( PNot _2 )
# 539 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 257 "src/aorai/yaparser.mly"
     ( PAnd (_1,_3) )
# 547 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 259 "src/aorai/yaparser.mly"
            ( POr (_1,_3) )
# 555 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'guard) in
    Obj.repr(
# 261 "src/aorai/yaparser.mly"
     ( _2 )
# 562 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 263 "src/aorai/yaparser.mly"
     (

	      let id = get_fresh_ident () in
	      let (pred,exp) = _1 in
	      Hashtbl.add observed_expressions id
		(exp, (Pretty_utils.sfprintf "%a" Cil.d_exp exp), pred);
	      (*Ltlast.LIdent(id)*)

	      Hashtbl.add observed_vars id id ;

	      let res =
		match !observed_expr_is_param with
		  | Only_vars -> PIndexedExp id
		  | Func_param (f,l) -> PFuncParam (id,f,l)
		  | Func_ret f -> PFuncReturn (id,f)
	      in

	      (* On repositionne la variable a son status par defaut pour la prochaine logic_relation *)
	      observed_expr_is_param := Only_vars; (* DEVRAIT ETRE FAIT AVANT LOGIC_RELATION!!!! *)

	      res
	    )
# 590 "src/aorai/yaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 292 "src/aorai/yaparser.mly"
                                     (
    ( Cil_types.Prel(Cil_types.Req, Logic_utils.expr_to_term ~cast:true _1,
	  	                    Logic_utils.expr_to_term ~cast:true _3),
      new_exp(Cil_types.BinOp(Cil_types.Eq, _1 , _3 , Cil.intType)) ) )
# 601 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 296 "src/aorai/yaparser.mly"
                                     (
    ( Cil_types.Prel(Cil_types.Rlt, Logic_utils.expr_to_term ~cast:true _1,
		                    Logic_utils.expr_to_term ~cast:true _3),
      new_exp(Cil_types.BinOp(Cil_types.Lt, _1 , _3 , Cil.intType)) ) )
# 612 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 300 "src/aorai/yaparser.mly"
                                     (
    ( Cil_types.Prel(Cil_types.Rgt, Logic_utils.expr_to_term ~cast:true _1,
		                    Logic_utils.expr_to_term ~cast:true _3),
      new_exp(Cil_types.BinOp(Cil_types.Gt, _1 , _3 , Cil.intType)) ) )
# 623 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 304 "src/aorai/yaparser.mly"
                                      (
    ( Cil_types.Prel(Cil_types.Rle, Logic_utils.expr_to_term ~cast:true _1,
		                    Logic_utils.expr_to_term ~cast:true _3),
      new_exp(Cil_types.BinOp(Cil_types.Le, _1 , _3 , Cil.intType)) ) )
# 634 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 308 "src/aorai/yaparser.mly"
                                     (
    ( Cil_types.Prel(Cil_types.Rge, Logic_utils.expr_to_term ~cast:true _1,
		                    Logic_utils.expr_to_term ~cast:true _3),
      new_exp(Cil_types.BinOp(Cil_types.Ge, _1 , _3 , Cil.intType) )) )
# 645 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 312 "src/aorai/yaparser.mly"
                                      (
    ( Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true _1,
		                    Logic_utils.expr_to_term ~cast:true _3),
      new_exp(Cil_types.BinOp(Cil_types.Ne, _1 , _3 , Cil.intType) )) )
# 656 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 316 "src/aorai/yaparser.mly"
                              (
    ( Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true _1,
		     Logic_const.term(Cil_types.TConst(Cil_types.CInt64(Int64.of_int 0,Cil_types.IInt,Some("0"))))
		       (Cil_types.Ctype Cil.intType)), _1) )
# 666 "src/aorai/yaparser.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 324 "src/aorai/yaparser.mly"
                                           (
    new_exp (Cil_types.BinOp(Cil_types.PlusA, _1 , _3 , Cil.intType)) )
# 675 "src/aorai/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 326 "src/aorai/yaparser.mly"
                                            (
    new_exp (Cil_types.BinOp(Cil_types.MinusA, _1 , _3 , Cil.intType)) )
# 684 "src/aorai/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 328 "src/aorai/yaparser.mly"
                       ( _1 )
# 691 "src/aorai/yaparser.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 333 "src/aorai/yaparser.mly"
                                             (
    new_exp (Cil_types.BinOp(Cil_types.Div, _1 , _3 , Cil.intType)) )
# 700 "src/aorai/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 335 "src/aorai/yaparser.mly"
                                            (
    new_exp (Cil_types.BinOp(Cil_types.Mult, _1 , _3 , Cil.intType)) )
# 709 "src/aorai/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 337 "src/aorai/yaparser.mly"
                                               (
    new_exp (Cil_types.BinOp(Cil_types.Mod, _1 , _3 , Cil.intType)) )
# 718 "src/aorai/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 339 "src/aorai/yaparser.mly"
                    ( _1 )
# 725 "src/aorai/yaparser.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 345 "src/aorai/yaparser.mly"
      ( new_exp (Cil_types.Const(Cil_types.CInt64(Int64.of_string _1,Cil_types.IInt, Some(_1)))))
# 732 "src/aorai/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 347 "src/aorai/yaparser.mly"
      ( new_exp (Cil_types.Const(Cil_types.CInt64(Int64.of_string ("-"^_2),Cil_types.IInt, Some("-"^_2)))))
# 739 "src/aorai/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 349 "src/aorai/yaparser.mly"
      ( new_exp (Cil_types.Lval(_1)) )
# 746 "src/aorai/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 351 "src/aorai/yaparser.mly"
      ( _2 )
# 753 "src/aorai/yaparser.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 359 "src/aorai/yaparser.mly"
            (

              let (my_host,my_offset) = (_1) in

              let new_offset = Utils_parser.add_offset my_offset (Utils_parser.get_new_offset my_host my_offset _3) in
              (my_host,new_offset)
            )
# 767 "src/aorai/yaparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 368 "src/aorai/yaparser.mly"
     ( Cil.addOffsetLval (Cil_types.Index (_3,Cil_types.NoOffset)) _1)
# 775 "src/aorai/yaparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 369 "src/aorai/yaparser.mly"
                    (_1)
# 782 "src/aorai/yaparser.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 374 "src/aorai/yaparser.mly"
            ( Aorai_option.fatal "NOT YET IMPLEMENTED : *A dereferencement access." )
# 789 "src/aorai/yaparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 376 "src/aorai/yaparser.mly"
            (
	      if(String.compare _4 "return")=0 then
		begin
		  if not (!observed_expr_is_param=Only_vars) then
		    Aorai_option.abort "An expression can not contain at same time a reference of a returned value and itself or a reference to a param";

		  observed_expr_is_param := Func_ret _1;
		  Cil.var ( Data_for_aorai.get_returninfo _1)
		end
	      else
		begin
		  match !observed_expr_is_param with
		    | Func_ret _ ->
			Aorai_option.abort "An expression can not contain both a reference of a returned value and another reference to itself or a reference to a param";

		    | Func_param (f,_) when not (f=_1) ->
			Aorai_option.abort "An expression can not contain both references two different called functions.";

		    | Only_vars ->
			observed_expr_is_param:=Func_param (_1,[_4]);
			Cil.var ( Data_for_aorai.get_paraminfo _1 _4)

		    | Func_param (_,l) ->
			observed_expr_is_param:=Func_param (_1,_4::l);
			Cil.var ( Data_for_aorai.get_paraminfo _1 _4)
		end
	    )
# 823 "src/aorai/yaparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 404 "src/aorai/yaparser.mly"
            ( Cil.var ( Data_for_aorai.get_varinfo _1) )
# 830 "src/aorai/yaparser.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 406 "src/aorai/yaparser.mly"
     ( _2 )
# 837 "src/aorai/yaparser.ml"
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
