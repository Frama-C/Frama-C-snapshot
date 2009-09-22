type token =
  | PROMELA_OR
  | PROMELA_AND
  | PROMELA_NOT
  | PROMELA_TRUE
  | PROMELA_FALSE
  | PROMELA_NEVER
  | PROMELA_IF
  | PROMELA_FI
  | PROMELA_GOTO
  | PROMELA_SKIP
  | PROMELA_LABEL of (string)
  | PROMELA_INT of (string)
  | PROMELA_COLON
  | PROMELA_SEMICOLON
  | PROMELA_DOUBLE_COLON
  | PROMELA_LBRACE
  | PROMELA_RBRACE
  | PROMELA_LPAREN
  | PROMELA_RPAREN
  | PROMELA_RIGHT_ARROW
  | PROMELA_EQ
  | PROMELA_LT
  | PROMELA_GT
  | PROMELA_LE
  | PROMELA_GE
  | PROMELA_NEQ
  | PROMELA_PLUS
  | PROMELA_MINUS
  | PROMELA_DIV
  | PROMELA_STAR
  | PROMELA_MODULO
  | PROMELA_DOT
  | PROMELA_LEFT_SQUARE
  | PROMELA_RIGHT_SQUARE
  | PROMELA_CALLOF of (string)
  | PROMELA_RETURNOF of (string)
  | PROMELA_CALLORRETURNOF of (string)
  | EOF

open Parsing;;
# 28 "src/ltl_to_acsl/promelaparser_withexps.mly"
open Parsing
open Promelaast
open Bool3


let observed_states=Hashtbl.create 1
let observed_vars=Hashtbl.create 1
let observed_funcs=Hashtbl.create 1

let observed_expressions=Hashtbl.create 97

let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))

# 60 "src/ltl_to_acsl/promelaparser_withexps.ml"
let yytransl_const = [|
  257 (* PROMELA_OR *);
  258 (* PROMELA_AND *);
  259 (* PROMELA_NOT *);
  260 (* PROMELA_TRUE *);
  261 (* PROMELA_FALSE *);
  262 (* PROMELA_NEVER *);
  263 (* PROMELA_IF *);
  264 (* PROMELA_FI *);
  265 (* PROMELA_GOTO *);
  266 (* PROMELA_SKIP *);
  269 (* PROMELA_COLON *);
  270 (* PROMELA_SEMICOLON *);
  271 (* PROMELA_DOUBLE_COLON *);
  272 (* PROMELA_LBRACE *);
  273 (* PROMELA_RBRACE *);
  274 (* PROMELA_LPAREN *);
  275 (* PROMELA_RPAREN *);
  276 (* PROMELA_RIGHT_ARROW *);
  277 (* PROMELA_EQ *);
  278 (* PROMELA_LT *);
  279 (* PROMELA_GT *);
  280 (* PROMELA_LE *);
  281 (* PROMELA_GE *);
  282 (* PROMELA_NEQ *);
  283 (* PROMELA_PLUS *);
  284 (* PROMELA_MINUS *);
  285 (* PROMELA_DIV *);
  286 (* PROMELA_STAR *);
  287 (* PROMELA_MODULO *);
  288 (* PROMELA_DOT *);
  289 (* PROMELA_LEFT_SQUARE *);
  290 (* PROMELA_RIGHT_SQUARE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  267 (* PROMELA_LABEL *);
  268 (* PROMELA_INT *);
  291 (* PROMELA_CALLOF *);
  292 (* PROMELA_RETURNOF *);
  293 (* PROMELA_CALLORRETURNOF *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\004\000\004\000\006\000\
\005\000\005\000\005\000\005\000\007\000\007\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\013\000\
\013\000\013\000\014\000\014\000\015\000\015\000\016\000\016\000\
\016\000\000\000"

let yylen = "\002\000\
\005\000\006\000\003\000\001\000\002\000\002\000\001\000\002\000\
\003\000\001\000\001\000\004\000\002\000\001\000\005\000\001\000\
\001\000\001\000\001\000\001\000\002\000\003\000\003\000\003\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\003\000\001\000\001\000\
\001\000\003\000\003\000\001\000\004\000\001\000\002\000\001\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\050\000\000\000\000\000\000\000\004\000\
\000\000\000\000\008\000\000\000\000\000\011\000\000\000\010\000\
\005\000\006\000\000\000\003\000\001\000\000\000\000\000\014\000\
\002\000\000\000\019\000\000\000\048\000\040\000\000\000\000\000\
\017\000\018\000\016\000\000\000\025\000\000\000\000\000\039\000\
\000\000\000\000\046\000\009\000\000\000\013\000\020\000\021\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\042\000\
\049\000\000\000\000\000\000\000\000\000\000\000\026\000\027\000\
\028\000\029\000\030\000\031\000\033\000\034\000\036\000\037\000\
\038\000\043\000\000\000\015\000\000\000\045\000"

let yydgoto = "\002\000\
\004\000\007\000\008\000\009\000\017\000\010\000\023\000\024\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000"

let yysindex = "\014\000\
\029\255\000\000\037\255\000\000\046\255\078\255\255\254\000\000\
\140\255\046\255\000\000\045\255\102\000\000\000\088\255\000\000\
\000\000\000\000\144\000\000\000\000\000\006\255\253\254\000\000\
\000\000\034\255\000\000\138\255\000\000\000\000\034\255\033\255\
\000\000\000\000\000\000\066\255\000\000\117\255\155\255\000\000\
\119\255\120\255\000\000\000\000\034\255\000\000\000\000\000\000\
\000\000\173\255\071\255\243\254\033\255\119\255\034\255\034\255\
\143\255\252\254\252\254\252\254\252\254\252\254\252\254\252\254\
\252\254\252\254\252\254\252\254\166\255\252\254\000\000\000\000\
\000\000\243\254\059\255\152\255\168\255\252\254\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\146\255\000\000\174\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\171\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\099\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\255\147\255\000\000\
\103\255\053\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\064\255\134\255\000\000\087\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\079\255\039\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\178\000\184\000\000\000\000\000\000\000\172\000\
\250\255\000\000\225\255\000\000\121\000\226\255\000\000\000\000"

let yytablesize = 195
let yytable = "\051\000\
\052\000\054\000\032\000\032\000\044\000\073\000\029\000\030\000\
\026\000\027\000\028\000\045\000\012\000\078\000\001\000\013\000\
\029\000\030\000\069\000\048\000\032\000\032\000\074\000\031\000\
\050\000\032\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\003\000\032\000\026\000\027\000\047\000\022\000\
\033\000\034\000\035\000\029\000\029\000\030\000\093\000\052\000\
\075\000\076\000\053\000\031\000\005\000\044\000\044\000\006\000\
\006\000\022\000\022\000\055\000\056\000\019\000\032\000\032\000\
\032\000\032\000\055\000\056\000\033\000\034\000\035\000\044\000\
\044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\044\000\044\000\044\000\057\000\044\000\047\000\
\047\000\072\000\011\000\058\000\059\000\060\000\061\000\062\000\
\063\000\023\000\023\000\020\000\020\000\021\000\022\000\041\000\
\041\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
\047\000\047\000\047\000\047\000\047\000\047\000\020\000\047\000\
\047\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\058\000\059\000\060\000\061\000\062\000\063\000\025\000\
\014\000\049\000\015\000\035\000\035\000\016\000\069\000\077\000\
\070\000\056\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\041\000\041\000\041\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\055\000\056\000\007\000\
\090\000\007\000\092\000\094\000\007\000\064\000\065\000\066\000\
\067\000\068\000\087\000\088\000\089\000\020\000\091\000\071\000\
\072\000\018\000\046\000"

let yycheck = "\031\000\
\031\000\032\000\001\001\002\001\008\001\019\001\011\001\012\001\
\003\001\004\001\005\001\015\001\014\001\018\001\001\000\017\001\
\011\001\012\001\032\001\026\000\019\001\020\001\053\000\018\001\
\031\000\030\001\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\006\001\030\001\003\001\004\001\005\001\001\001\
\035\001\036\001\037\001\011\001\011\001\012\001\078\000\078\000\
\055\000\056\000\018\001\018\001\016\001\001\001\002\001\011\001\
\011\001\019\001\020\001\001\001\002\001\017\001\030\001\030\001\
\001\001\002\001\001\001\002\001\035\001\036\001\037\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\020\001\034\001\001\001\
\002\001\019\001\013\001\021\001\022\001\023\001\024\001\025\001\
\026\001\019\001\020\001\001\001\002\001\000\000\015\001\001\001\
\002\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\020\001\033\001\
\034\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\001\001\002\001\
\034\001\021\001\022\001\023\001\024\001\025\001\026\001\000\000\
\005\001\008\001\007\001\001\001\002\001\010\001\032\001\009\001\
\033\001\002\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\001\001\002\001\005\001\
\011\001\007\001\011\001\034\001\010\001\027\001\028\001\029\001\
\030\001\031\001\066\000\067\000\068\000\012\000\070\000\019\001\
\019\001\010\000\023\000"

let yynames_const = "\
  PROMELA_OR\000\
  PROMELA_AND\000\
  PROMELA_NOT\000\
  PROMELA_TRUE\000\
  PROMELA_FALSE\000\
  PROMELA_NEVER\000\
  PROMELA_IF\000\
  PROMELA_FI\000\
  PROMELA_GOTO\000\
  PROMELA_SKIP\000\
  PROMELA_COLON\000\
  PROMELA_SEMICOLON\000\
  PROMELA_DOUBLE_COLON\000\
  PROMELA_LBRACE\000\
  PROMELA_RBRACE\000\
  PROMELA_LPAREN\000\
  PROMELA_RPAREN\000\
  PROMELA_RIGHT_ARROW\000\
  PROMELA_EQ\000\
  PROMELA_LT\000\
  PROMELA_GT\000\
  PROMELA_LE\000\
  PROMELA_GE\000\
  PROMELA_NEQ\000\
  PROMELA_PLUS\000\
  PROMELA_MINUS\000\
  PROMELA_DIV\000\
  PROMELA_STAR\000\
  PROMELA_MODULO\000\
  PROMELA_DOT\000\
  PROMELA_LEFT_SQUARE\000\
  PROMELA_RIGHT_SQUARE\000\
  EOF\000\
  "

let yynames_block = "\
  PROMELA_LABEL\000\
  PROMELA_INT\000\
  PROMELA_CALLOF\000\
  PROMELA_RETURNOF\000\
  PROMELA_CALLORRETURNOF\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'states) in
    Obj.repr(
# 87 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                                                 ( 
	    let states=
	      Hashtbl.fold (fun _ st l -> 
		if st.acceptation=Undefined or st.init=Undefined then
		  begin
		    Format.print_string ("Error: the state '"^(st.name)^"' is used but never defined.\n");
		    exit 1
		  end;
		st::l
	      ) observed_states []
	    in 
	    Data_for_ltl.setLtl_expressions observed_expressions;
	    Ltl_logic.setLtl_expressions observed_expressions;
	    let n=ref 0 in
	    let transitions = Ltl_logic.simplifyTrans _3 in
	    List.iter (fun t -> t.numt<-(!n); n:=!n+1) transitions;

	    ((states , transitions),observed_vars,observed_funcs)
	)
# 297 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : (Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t)))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'states) in
    Obj.repr(
# 106 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                                                                   (
	    let states=
	      Hashtbl.fold (fun _ st l -> 
		if st.acceptation=Undefined or st.init=Undefined then
		  begin
		    Format.print_string ("Error: the state '"^(st.name)^"' is used but never defined.\n");
		    exit 1
		  end;
		st::l
	      ) observed_states []
	    in
	    Data_for_ltl.setLtl_expressions observed_expressions;
	    Ltl_logic.setLtl_expressions observed_expressions;
	    let n=ref 0 in
	    let transitions = Ltl_logic.simplifyTrans _3 in
	    List.iter (fun t -> t.numt<-(!n); n:=!n+1) transitions;


	    ((states , transitions),observed_vars,observed_funcs) )
# 322 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : (Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'states) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 130 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                         ( 
	    _1@_3
	    (*let (s1,t1)=$1 in
	    let (s2,t2)=$3 in
	      (s1@s2,t1@t2)*)
	  )
# 335 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 136 "src/ltl_to_acsl/promelaparser_withexps.mly"
         ( _1 )
# 342 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'state_labels) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_body) in
    Obj.repr(
# 140 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                  (
	  let (stl,trans)=_1 in
	  let (trl,force_final)=_2 in
	    if force_final then
	      begin
		List.iter (fun s -> 
		  try 
		    (Hashtbl.find observed_states s.name).acceptation <- True
		  with
		    | Not_found -> assert false (* This state has to be in the hashtable -- by construction *)
		) stl
	      end;
	    if trl=[] then
	      trans 
	    else
	      let tr_list=
		List.fold_left (fun l1 (cr,stop_st)  -> 
		  List.fold_left (fun l2 st -> 
		    {start=st;stop=stop_st;cross=cr;numt=(-1)}::l2
		  ) l1 stl
		) [] trl 
	      in
	        (List.rev tr_list)@trans
	      



	)
# 377 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_labels) in
    Obj.repr(
# 171 "src/ltl_to_acsl/promelaparser_withexps.mly"
                             ( 
	    let (stl1,trl1)=_1 in
	    let (stl2,trl2)=_2 in
	      (stl1@stl2,trl1@trl2) 
	)
# 389 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'state_labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 176 "src/ltl_to_acsl/promelaparser_withexps.mly"
         ( _1 )
# 396 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'state_labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 180 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                      (
	  begin
    (* Step 0 : trans is the set of new transitions and old is the description of the current state *)
	    let trans = ref [] in
	    (* Promela Label is a state. According to its name, we will try to give him its properties (init / accept) *)
	    (* Firstly, if this state is still referenced, then we get it back. Else, we make a new "empty" state *)
	    let old= 
	      try  
		Hashtbl.find observed_states _1
	      with
		| Not_found -> 
		    let s={name=_1;acceptation=Undefined;init=Undefined;nums=(Hashtbl.length observed_states)} in
		    Hashtbl.add observed_states _1 s;
		    s
	    in
    (* Step 1 : setting up the acceptance status *)
	    (* Default status : Non acceptation state *)
 	    old.acceptation <- False;
	    
	    (* Accept_all state means acceptance state with a reflexive transition without cross condition *)
	    (* This case is not exlusive with the following. Acceptation status is set in this last. *)
	    if (String.length _1>=10) && (String.compare (String.sub _1 0 10) "accept_all")=0 then 
	      trans:={start=old;stop=old;cross=PTrue;numt=(-1)}::!trans;
	    
	    (* If the name includes accept then this state is an acceptation one. *)
	    if (String.length _1>=7) && (String.compare (String.sub _1 0 7) "accept_")=0 then
	      old.acceptation <- True;

    (* Step 2 : setting up the init status *)
	    (* If the state name ended with "_init" then it is an initial state. Else, it is not. *)
	    if (String.length _1>=5) && (String.compare (String.sub _1 ((String.length _1)-5) 5) "_init" ) = 0
	    then  
	      old.init <- True
	    else
	      old.init <- False;
	    
	    ([old],!trans)
	  end
	)
# 441 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    Obj.repr(
# 223 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                            ( (_2,false) )
# 448 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 224 "src/ltl_to_acsl/promelaparser_withexps.mly"
                ( ([],false) )
# 454 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 225 "src/ltl_to_acsl/promelaparser_withexps.mly"
                 ( ([],true) )
# 460 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 226 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                                            ( ([],true) )
# 466 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 231 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                 ( _1@[_2] )
# 474 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 232 "src/ltl_to_acsl/promelaparser_withexps.mly"
              ( [_1] )
# 481 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'guard) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 236 "src/ltl_to_acsl/promelaparser_withexps.mly"
                                                                                    (
	  let s=
	    try
	      Hashtbl.find observed_states _5
	    with
		Not_found -> 
		  let r={name=_5;init=Undefined;acceptation=Undefined;nums=(Hashtbl.length observed_states)}  in
		    Hashtbl.add observed_states _5 r;
		    r
	  in
	    (_2,s)
	)
# 500 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 252 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( if not (Hashtbl.mem observed_funcs _1) then Hashtbl.add observed_funcs _1 _1 ; PCallOrReturn _1 )
# 507 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 254 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( if not (Hashtbl.mem observed_funcs _1) then Hashtbl.add observed_funcs _1 _1 ; PCall _1 )
# 514 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 256 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( if not (Hashtbl.mem observed_funcs _1) then Hashtbl.add observed_funcs _1 _1 ; PReturn _1 )
# 521 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 258 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( PTrue )
# 527 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 260 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( PFalse )
# 533 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 262 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( PNot _2 )
# 540 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 264 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( PAnd (_1,_3) )
# 548 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 266 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( POr (_1,_3) )
# 556 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'guard) in
    Obj.repr(
# 268 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( _2 )
# 563 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 273 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( 
	      let id = get_fresh_ident () in
	      let (pred,exp) = _1 in
	      Hashtbl.add observed_expressions id 
		(exp, (Pretty_utils.sfprintf "%a" Cil.d_exp exp), pred);
	      (*Ltlast.LIdent(id)*)

	      Hashtbl.add observed_vars id id ; 
	      PIndexedExp id  
	    )
# 579 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 291 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( (	Cil_types.Prel(Cil_types.Req, Logic_utils.expr_to_term _1 ,Logic_utils.expr_to_term  _3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Eq, _1 , _3 , Cil.intType)) )
	    )
# 589 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 295 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( (	Cil_types.Prel(Cil_types.Rlt, Logic_utils.expr_to_term _1 , Logic_utils.expr_to_term _3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Lt, _1 , _3 , Cil.intType)) )
	    )
# 599 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 299 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( (	Cil_types.Prel(Cil_types.Rgt, Logic_utils.expr_to_term _1 , Logic_utils.expr_to_term _3),
		Cil.new_exp(Cil_types.BinOp(Cil_types.Gt, _1 , _3 , Cil.intType)) )
	    )
# 609 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 303 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( (	Cil_types.Prel(Cil_types.Rle, Logic_utils.expr_to_term _1 , Logic_utils.expr_to_term _3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Le, _1 , _3 , Cil.intType) ))
	    )
# 619 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 307 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( (	Cil_types.Prel(Cil_types.Rge, Logic_utils.expr_to_term _1 , Logic_utils.expr_to_term _3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Ge, _1 , _3 , Cil.intType) ))
	    )
# 629 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 311 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( (	Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term _1 , Logic_utils.expr_to_term _3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Ne , _1 , _3 , Cil.intType) ))
	    )
# 639 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 315 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( (	Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term _1 ,
		     Logic_const.term
		       (Cil_types.TConst(Cil_types.CInt64(Int64.of_int 0,Cil_types.IInt,Some("0"))))
		       (Cil_types.Ctype Cil.intType)),
		_1)
	    )
# 651 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 327 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( Cil.new_exp (Cil_types.BinOp(Cil_types.PlusA, _1 , _3 , Cil.intType)) )
# 659 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 329 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( Cil.new_exp (Cil_types.BinOp(Cil_types.MinusA, _1 , _3 , Cil.intType)) )
# 667 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 331 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( _1 )
# 674 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 337 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( Cil.new_exp (Cil_types.BinOp(Cil_types.Div, _1 , _3 , Cil.intType)) )
# 682 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 339 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( Cil.new_exp (Cil_types.BinOp(Cil_types.Mult, _1 , _3 , Cil.intType)) )
# 690 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 341 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( Cil.new_exp (Cil_types.BinOp(Cil_types.Mod, _1 , _3 , Cil.intType)) )
# 698 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 343 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( _1 )
# 705 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 349 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( Cil.new_exp (Cil_types.Const(Cil_types.CInt64(Int64.of_string _1,Cil_types.IInt, Some(_1)))))
# 712 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 351 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( Cil.new_exp (Cil_types.Lval(_1)) )
# 719 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 353 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( _2 )
# 726 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 360 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( 
              let (my_host,my_offset) = (_1) in
              
              let new_offset = Utils_parser.add_offset my_offset (Utils_parser.get_new_offset my_host my_offset _3) in
              (my_host,new_offset))
# 738 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_array) in
    Obj.repr(
# 367 "src/ltl_to_acsl/promelaparser_withexps.mly"
     (_1)
# 745 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 371 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( Cil.addOffsetLval (Cil_types.Index (_3,Cil_types.NoOffset)) _1)
# 753 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 373 "src/ltl_to_acsl/promelaparser_withexps.mly"
     (_1)
# 760 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 378 "src/ltl_to_acsl/promelaparser_withexps.mly"
            ( Ltl_to_acsl_option.fatal "NOT YET IMPLEMENTED : *A dereferencement access." )
# 767 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 380 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( Cil.var ( Data_for_ltl.get_varinfo _1) )
# 774 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 382 "src/ltl_to_acsl/promelaparser_withexps.mly"
     ( _2 )
# 781 "src/ltl_to_acsl/promelaparser_withexps.ml"
               : 'access_leaf))
(* Entry promela *)
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
let promela (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t))
