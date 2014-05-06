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
  | PROMELA_FUNC

open Parsing;;
let _ = parse_error;;
# 30 "src/aorai/promelaparser_withexps.mly"
open Logic_ptree
open Promelaast
open Bool3


let observed_states=Hashtbl.create 1

let to_seq c =
  [{ condition = Some c; nested = [];
    min_rep = Some (PCst (IntConstant "1"));
    max_rep = Some (PCst (IntConstant "1"));
   }]
# 58 "src/aorai/promelaparser_withexps.ml"
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
  294 (* PROMELA_FUNC *);
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
\013\000\013\000\013\000\014\000\014\000\015\000\015\000\016\000\
\016\000\016\000\016\000\000\000"

let yylen = "\002\000\
\005\000\006\000\003\000\001\000\002\000\002\000\001\000\002\000\
\003\000\001\000\001\000\004\000\002\000\001\000\005\000\001\000\
\001\000\001\000\001\000\001\000\002\000\003\000\003\000\003\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\003\000\001\000\001\000\
\002\000\001\000\003\000\003\000\001\000\004\000\001\000\002\000\
\004\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\052\000\000\000\000\000\000\000\004\000\
\000\000\000\000\008\000\000\000\000\000\011\000\000\000\010\000\
\005\000\006\000\000\000\003\000\001\000\000\000\000\000\014\000\
\002\000\000\000\019\000\000\000\000\000\040\000\000\000\000\000\
\000\000\017\000\018\000\016\000\000\000\025\000\000\000\000\000\
\039\000\000\000\000\000\047\000\009\000\000\000\013\000\020\000\
\021\000\012\000\000\000\000\000\000\000\000\000\041\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\043\000\051\000\000\000\000\000\000\000\
\000\000\000\000\026\000\027\000\028\000\029\000\030\000\031\000\
\033\000\034\000\036\000\037\000\038\000\044\000\000\000\049\000\
\015\000\000\000\046\000"

let yydgoto = "\002\000\
\004\000\007\000\008\000\009\000\017\000\010\000\023\000\024\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000"

let yysindex = "\003\000\
\014\255\000\000\041\255\000\000\085\255\126\255\002\255\000\000\
\139\255\085\255\000\000\001\255\148\000\000\000\143\255\000\000\
\000\000\000\000\159\000\000\000\000\000\010\255\250\254\000\000\
\000\000\038\255\000\000\182\255\164\255\000\000\038\255\192\255\
\037\255\000\000\000\000\000\000\104\255\000\000\039\255\124\255\
\000\000\173\255\176\255\000\000\000\000\038\255\000\000\000\000\
\000\000\000\000\178\255\006\255\076\255\075\255\000\000\037\255\
\173\255\038\255\038\255\202\255\255\254\255\254\255\254\255\254\
\255\254\255\254\255\254\255\254\255\254\255\254\255\254\201\255\
\255\254\203\255\000\000\000\000\000\000\075\255\052\255\211\255\
\204\255\255\254\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\183\255\000\000\
\000\000\197\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\140\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\186\255\057\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\004\255\172\255\
\000\000\141\255\091\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\068\255\155\255\000\000\000\000\
\107\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\084\255\188\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\206\000\209\000\000\000\000\000\000\000\197\000\
\013\000\000\000\225\255\000\000\130\000\226\255\000\000\000\000"

let yytablesize = 220
let yytable = "\053\000\
\054\000\045\000\057\000\001\000\032\000\032\000\058\000\059\000\
\046\000\029\000\030\000\006\000\026\000\027\000\028\000\012\000\
\082\000\019\000\013\000\003\000\029\000\030\000\032\000\032\000\
\075\000\078\000\032\000\031\000\033\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\032\000\049\000\033\000\
\026\000\027\000\048\000\052\000\034\000\035\000\036\000\029\000\
\029\000\030\000\098\000\054\000\058\000\059\000\056\000\031\000\
\005\000\050\000\050\000\061\000\062\000\063\000\064\000\065\000\
\066\000\032\000\033\000\033\000\032\000\032\000\079\000\080\000\
\034\000\035\000\036\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\045\000\045\000\077\000\076\000\006\000\
\061\000\062\000\063\000\064\000\065\000\066\000\023\000\023\000\
\058\000\059\000\072\000\048\000\048\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\045\000\060\000\045\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\011\000\048\000\048\000\042\000\042\000\014\000\
\007\000\015\000\007\000\021\000\016\000\007\000\067\000\068\000\
\069\000\070\000\071\000\042\000\042\000\022\000\025\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\035\000\035\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\020\000\020\000\022\000\050\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\091\000\092\000\
\093\000\051\000\095\000\055\000\072\000\020\000\022\000\022\000\
\073\000\074\000\081\000\094\000\059\000\096\000\097\000\076\000\
\099\000\020\000\018\000\047\000"

let yycheck = "\031\000\
\031\000\008\001\033\000\001\000\001\001\002\001\001\001\002\001\
\015\001\011\001\012\001\011\001\003\001\004\001\005\001\014\001\
\018\001\017\001\017\001\006\001\011\001\012\001\019\001\020\001\
\019\001\056\000\028\001\018\001\030\001\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\028\001\026\000\030\001\
\003\001\004\001\005\001\031\000\035\001\036\001\037\001\011\001\
\011\001\012\001\082\000\082\000\001\001\002\001\018\001\018\001\
\016\001\001\001\002\001\021\001\022\001\023\001\024\001\025\001\
\026\001\028\001\030\001\030\001\001\001\002\001\058\000\059\000\
\035\001\036\001\037\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\001\001\002\001\019\001\019\001\011\001\
\021\001\022\001\023\001\024\001\025\001\026\001\019\001\020\001\
\001\001\002\001\032\001\001\001\002\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\020\001\034\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\013\001\033\001\034\001\001\001\002\001\005\001\
\005\001\007\001\007\001\000\000\010\001\010\001\027\001\028\001\
\029\001\030\001\031\001\001\001\002\001\015\001\000\000\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\001\001\002\001\034\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\001\001\002\001\001\001\008\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\069\000\070\000\
\071\000\038\001\073\000\012\001\032\001\020\001\019\001\020\001\
\033\001\032\001\009\001\011\001\002\001\011\001\011\001\019\001\
\034\001\012\000\010\000\023\000"

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
  PROMELA_FUNC\000\
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
# 80 "src/aorai/promelaparser_withexps.mly"
                                                                 ( 
	    let states=
	      Hashtbl.fold (fun _ st l -> 
		if st.acceptation=Undefined || st.init=Undefined then
		  begin
		    Format.print_string ("Error: the state '"^(st.name)^"' is used but never defined.\n");
		    exit 1
		  end;
		st::l
	      ) observed_states []
	    in
	    (states , _3)
	)
# 300 "src/aorai/promelaparser_withexps.ml"
               : Promelaast.parsed_automaton))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'states) in
    Obj.repr(
# 94 "src/aorai/promelaparser_withexps.mly"
                                                 (
	    let states=
	      Hashtbl.fold (fun _ st l -> 
		if st.acceptation=Undefined || st.init=Undefined then
		  begin
                    Aorai_option.abort 
                      "Error: state %s is used bug never defined" st.name
		  end;
		st::l
	      ) observed_states []
	    in
	    (states , _3) )
# 318 "src/aorai/promelaparser_withexps.ml"
               : Promelaast.parsed_automaton))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'states) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 109 "src/aorai/promelaparser_withexps.mly"
                                         ( _1@_3 )
# 326 "src/aorai/promelaparser_withexps.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 110 "src/aorai/promelaparser_withexps.mly"
         ( _1 )
# 333 "src/aorai/promelaparser_withexps.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'state_labels) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_body) in
    Obj.repr(
# 114 "src/aorai/promelaparser_withexps.mly"
                                  (
	  let (stl,trans)=_1 in
	  let (trl,force_final)=_2 in
	    if force_final then
	      begin
		List.iter (fun s -> 
		  try 
		    (Hashtbl.find observed_states s.name).acceptation <- True
		  with
		    | Not_found -> assert false 
                (* This state has to be in the hashtable -- by construction *)
		) stl
	      end;
	    if trl=[] then
	      trans 
	    else
	      let tr_list=
		List.fold_left (fun l1 (cr,stop_st)  -> 
		  List.fold_left (fun l2 st -> 
		    {start=st;stop=stop_st;cross=Seq (to_seq cr);numt=(-1)}::l2
		  ) l1 stl
		) [] trl
	      in
	      (List.rev tr_list)@trans
	)
# 365 "src/aorai/promelaparser_withexps.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_labels) in
    Obj.repr(
# 142 "src/aorai/promelaparser_withexps.mly"
                             ( 
	    let (stl1,trl1)=_1 in
	    let (stl2,trl2)=_2 in
	      (stl1@stl2,trl1@trl2) 
	)
# 377 "src/aorai/promelaparser_withexps.ml"
               : 'state_labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 147 "src/aorai/promelaparser_withexps.mly"
         ( _1 )
# 384 "src/aorai/promelaparser_withexps.ml"
               : 'state_labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 151 "src/aorai/promelaparser_withexps.mly"
                                      (
	  begin
            (* Step 0 : trans is the set of new transitions and old 
               is the description of the current state *)
	    let trans = ref [] in
	    (* Promela Label is a state. According to its name, 
               we will try to give him its properties (init / accept) *)
	    (* Firstly, if this state is still referenced, 
               then we get it back. Else, we make a new "empty" state *)
	    let old= 
	      try  
		Hashtbl.find observed_states _1
	      with
		| Not_found -> 
		    let s = Data_for_aorai.new_state _1 in
		    Hashtbl.add observed_states _1 s;
		    s
	    in
            (* Step 1 : setting up the acceptance status *)
	    (* Default status : Non acceptation state *)
 	    old.acceptation <- False;
	    
	    (* Accept_all state means acceptance state with a 
               reflexive transition without cross condition *)
	    (* This case is not exclusive with the following. 
               Acceptation status is set in this last. *)
	    if (String.length _1>=10) && 
              (String.compare (String.sub _1 0 10) "accept_all")=0 
            then 
	      trans:=
                {start=old;stop=old;cross=Seq (to_seq PTrue);numt=(-1)}::!trans;
	    
	    (* If the name includes accept then this state is 
               an acceptation one. *)
	    if (String.length _1>=7) && 
              (String.compare (String.sub _1 0 7) "accept_")=0 
            then
	      old.acceptation <- True;

            (* Step 2 : setting up the init status *)
	    (* If the state name ended with "_init" then 
               it is an initial state. Else, it is not. *)
	    if (String.length _1>=5) && 
              (String.compare 
                 (String.sub _1 ((String.length _1)-5) 5) "_init" ) = 0
	    then
	      old.init <- True
	    else
	      old.init <- False;
	    
	    ([old],!trans)
	  end
	)
# 443 "src/aorai/promelaparser_withexps.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    Obj.repr(
# 208 "src/aorai/promelaparser_withexps.mly"
                                            ( (_2,false) )
# 450 "src/aorai/promelaparser_withexps.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 209 "src/aorai/promelaparser_withexps.mly"
                ( ([],false) )
# 456 "src/aorai/promelaparser_withexps.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 210 "src/aorai/promelaparser_withexps.mly"
                 ( ([],true) )
# 462 "src/aorai/promelaparser_withexps.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 211 "src/aorai/promelaparser_withexps.mly"
                                                            ( ([],true) )
# 468 "src/aorai/promelaparser_withexps.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 216 "src/aorai/promelaparser_withexps.mly"
                                 ( _1@[_2] )
# 476 "src/aorai/promelaparser_withexps.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 217 "src/aorai/promelaparser_withexps.mly"
              ( [_1] )
# 483 "src/aorai/promelaparser_withexps.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'guard) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 222 "src/aorai/promelaparser_withexps.mly"
                                                       (
	  let s=
	    try
	      Hashtbl.find observed_states _5
	    with
		Not_found -> 
		  let r = Data_for_aorai.new_state _5 in
		  Hashtbl.add observed_states _5 r;
		  r
	  in
	  (_2,s)
	)
# 502 "src/aorai/promelaparser_withexps.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 237 "src/aorai/promelaparser_withexps.mly"
                          ( POr(PCall (_1,None), PReturn _1) )
# 509 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 238 "src/aorai/promelaparser_withexps.mly"
                         ( PCall (_1,None) )
# 516 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 239 "src/aorai/promelaparser_withexps.mly"
                           ( PReturn _1 )
# 523 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 240 "src/aorai/promelaparser_withexps.mly"
                ( PTrue )
# 529 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 241 "src/aorai/promelaparser_withexps.mly"
                 ( PFalse )
# 535 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 242 "src/aorai/promelaparser_withexps.mly"
                     ( PNot _2 )
# 542 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 243 "src/aorai/promelaparser_withexps.mly"
                           ( PAnd (_1,_3) )
# 550 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 244 "src/aorai/promelaparser_withexps.mly"
                          ( POr (_1,_3) )
# 558 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'guard) in
    Obj.repr(
# 245 "src/aorai/promelaparser_withexps.mly"
                                       ( _2 )
# 565 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_relation) in
    Obj.repr(
# 246 "src/aorai/promelaparser_withexps.mly"
                         ( _1 )
# 572 "src/aorai/promelaparser_withexps.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 250 "src/aorai/promelaparser_withexps.mly"
                                             ( PRel(Eq, _1, _3) )
# 580 "src/aorai/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 251 "src/aorai/promelaparser_withexps.mly"
                                             ( PRel(Lt, _1, _3) )
# 588 "src/aorai/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 252 "src/aorai/promelaparser_withexps.mly"
                                             ( PRel(Gt, _1, _3) )
# 596 "src/aorai/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 253 "src/aorai/promelaparser_withexps.mly"
                                             ( PRel(Le, _1, _3) )
# 604 "src/aorai/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 254 "src/aorai/promelaparser_withexps.mly"
                                             ( PRel(Ge, _1, _3) )
# 612 "src/aorai/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 255 "src/aorai/promelaparser_withexps.mly"
                                             ( PRel(Neq,_1, _3) )
# 620 "src/aorai/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 256 "src/aorai/promelaparser_withexps.mly"
                  ( PRel(Neq,_1, PCst(IntConstant "0")) )
# 627 "src/aorai/promelaparser_withexps.ml"
               : 'logic_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 262 "src/aorai/promelaparser_withexps.mly"
            ( PBinop(Badd, _1 , _3))
# 635 "src/aorai/promelaparser_withexps.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation) in
    Obj.repr(
# 264 "src/aorai/promelaparser_withexps.mly"
            ( PBinop(Bsub,_1,_3) )
# 643 "src/aorai/promelaparser_withexps.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_relation_mul) in
    Obj.repr(
# 265 "src/aorai/promelaparser_withexps.mly"
                      ( _1 )
# 650 "src/aorai/promelaparser_withexps.ml"
               : 'arith_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 271 "src/aorai/promelaparser_withexps.mly"
            ( PBinop(Bdiv,_1,_3) )
# 658 "src/aorai/promelaparser_withexps.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 273 "src/aorai/promelaparser_withexps.mly"
            ( PBinop(Bmul,_1,_3) )
# 666 "src/aorai/promelaparser_withexps.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_relation_mul) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 275 "src/aorai/promelaparser_withexps.mly"
            ( PBinop(Bmod,_1,_3) )
# 674 "src/aorai/promelaparser_withexps.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_or_const) in
    Obj.repr(
# 276 "src/aorai/promelaparser_withexps.mly"
                   ( _1 )
# 681 "src/aorai/promelaparser_withexps.ml"
               : 'arith_relation_mul))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 280 "src/aorai/promelaparser_withexps.mly"
                      ( PCst(IntConstant _1) )
# 688 "src/aorai/promelaparser_withexps.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 282 "src/aorai/promelaparser_withexps.mly"
            ( PUnop (Uminus, PCst (IntConstant _2)) )
# 695 "src/aorai/promelaparser_withexps.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 283 "src/aorai/promelaparser_withexps.mly"
          ( _1 )
# 702 "src/aorai/promelaparser_withexps.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arith_relation) in
    Obj.repr(
# 284 "src/aorai/promelaparser_withexps.mly"
                                                ( _2 )
# 709 "src/aorai/promelaparser_withexps.ml"
               : 'access_or_const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'access) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 288 "src/aorai/promelaparser_withexps.mly"
                                    ( PField (_1,_3) )
# 717 "src/aorai/promelaparser_withexps.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_array) in
    Obj.repr(
# 289 "src/aorai/promelaparser_withexps.mly"
                (_1)
# 724 "src/aorai/promelaparser_withexps.ml"
               : 'access))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'access_array) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'access_or_const) in
    Obj.repr(
# 293 "src/aorai/promelaparser_withexps.mly"
     ( PArrget(_1,_3) )
# 732 "src/aorai/promelaparser_withexps.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'access_leaf) in
    Obj.repr(
# 294 "src/aorai/promelaparser_withexps.mly"
                   (_1)
# 739 "src/aorai/promelaparser_withexps.ml"
               : 'access_array))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'access) in
    Obj.repr(
# 297 "src/aorai/promelaparser_withexps.mly"
                              ( PUnop(Ustar,_2) )
# 746 "src/aorai/promelaparser_withexps.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 298 "src/aorai/promelaparser_withexps.mly"
                                                        ( PPrm(_1,_4) )
# 754 "src/aorai/promelaparser_withexps.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 299 "src/aorai/promelaparser_withexps.mly"
                 ( PVar _1 )
# 761 "src/aorai/promelaparser_withexps.ml"
               : 'access_leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'access) in
    Obj.repr(
# 300 "src/aorai/promelaparser_withexps.mly"
                                        ( _2 )
# 768 "src/aorai/promelaparser_withexps.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Promelaast.parsed_automaton)
