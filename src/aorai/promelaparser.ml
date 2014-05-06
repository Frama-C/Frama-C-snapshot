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
  | PROMELA_COLON
  | PROMELA_SEMICOLON
  | PROMELA_DOUBLE_COLON
  | PROMELA_LBRACE
  | PROMELA_RBRACE
  | PROMELA_LPAREN
  | PROMELA_RPAREN
  | PROMELA_RIGHT_ARROW
  | PROMELA_CALLOF of (string)
  | PROMELA_RETURNOF of (string)
  | PROMELA_CALLORRETURNOF of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 30 "src/aorai/promelaparser.mly"
open Promelaast
open Bool3

let observed_states=Hashtbl.create 1

let to_seq c = 
  [{ condition = Some c; nested = [];
    min_rep = Some (PCst (Logic_ptree.IntConstant "1"));
    max_rep = Some (PCst (Logic_ptree.IntConstant "1"));
   }]

# 41 "src/aorai/promelaparser.ml"
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
  268 (* PROMELA_COLON *);
  269 (* PROMELA_SEMICOLON *);
  270 (* PROMELA_DOUBLE_COLON *);
  271 (* PROMELA_LBRACE *);
  272 (* PROMELA_RBRACE *);
  273 (* PROMELA_LPAREN *);
  274 (* PROMELA_RPAREN *);
  275 (* PROMELA_RIGHT_ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  267 (* PROMELA_LABEL *);
  276 (* PROMELA_CALLOF *);
  277 (* PROMELA_RETURNOF *);
  278 (* PROMELA_CALLORRETURNOF *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\004\000\004\000\006\000\
\005\000\005\000\005\000\005\000\007\000\007\000\008\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\000\000"

let yylen = "\002\000\
\005\000\006\000\003\000\001\000\002\000\002\000\001\000\002\000\
\003\000\001\000\001\000\004\000\002\000\001\000\005\000\001\000\
\001\000\001\000\001\000\001\000\002\000\003\000\003\000\003\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\004\000\
\000\000\000\000\008\000\000\000\000\000\011\000\000\000\010\000\
\005\000\006\000\000\000\003\000\001\000\000\000\000\000\014\000\
\002\000\000\000\019\000\000\000\025\000\000\000\017\000\018\000\
\016\000\000\000\009\000\000\000\013\000\020\000\021\000\012\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\015\000"

let yydgoto = "\002\000\
\004\000\007\000\008\000\009\000\017\000\010\000\023\000\024\000\
\034\000"

let yysindex = "\012\000\
\029\255\000\000\026\255\000\000\038\255\039\255\027\255\000\000\
\037\255\038\255\000\000\015\255\052\000\000\000\040\255\000\000\
\000\000\000\000\055\000\000\000\000\000\253\254\022\255\000\000\
\000\000\017\255\000\000\048\255\000\000\017\255\000\000\000\000\
\000\000\004\255\000\000\017\255\000\000\000\000\000\000\000\000\
\014\255\017\255\017\255\049\255\000\000\002\255\055\255\050\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\043\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\249\254\006\255\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\047\000\050\000\000\000\000\000\000\000\039\000\
\003\000"

let yytablesize = 62
let yytable = "\026\000\
\027\000\028\000\042\000\043\000\042\000\043\000\022\000\029\000\
\020\000\020\000\023\000\023\000\001\000\030\000\042\000\043\000\
\031\000\032\000\033\000\026\000\027\000\038\000\044\000\022\000\
\022\000\006\000\020\000\029\000\039\000\035\000\019\000\045\000\
\041\000\030\000\003\000\036\000\031\000\032\000\033\000\012\000\
\005\000\014\000\013\000\015\000\046\000\047\000\016\000\007\000\
\006\000\007\000\011\000\021\000\007\000\022\000\025\000\040\000\
\043\000\048\000\020\000\018\000\049\000\037\000"

let yycheck = "\003\001\
\004\001\005\001\001\001\002\001\001\001\002\001\001\001\011\001\
\001\001\002\001\018\001\019\001\001\000\017\001\001\001\002\001\
\020\001\021\001\022\001\003\001\004\001\005\001\019\001\018\001\
\019\001\011\001\019\001\011\001\026\000\008\001\016\001\018\001\
\030\000\017\001\006\001\014\001\020\001\021\001\022\001\013\001\
\015\001\005\001\016\001\007\001\042\000\043\000\010\001\005\001\
\011\001\007\001\012\001\000\000\010\001\014\001\000\000\008\001\
\002\001\009\001\012\000\010\000\011\001\023\000"

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
  EOF\000\
  "

let yynames_block = "\
  PROMELA_LABEL\000\
  PROMELA_CALLOF\000\
  PROMELA_RETURNOF\000\
  PROMELA_CALLORRETURNOF\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'states) in
    Obj.repr(
# 69 "src/aorai/promelaparser.mly"
                                                                 ( 
	    let states=
	      Hashtbl.fold (fun _ st l -> 
		if st.acceptation=Undefined || st.init=Undefined then
		  begin
                    Aorai_option.abort 
                      "Error: the state %s is used but never defined" st.name;
		  end;
		st::l
	      ) observed_states []
            in
            (states , _3)
	)
# 187 "src/aorai/promelaparser.ml"
               : Promelaast.parsed_automaton))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'states) in
    Obj.repr(
# 83 "src/aorai/promelaparser.mly"
                               (
	      let states=
	        Hashtbl.fold (fun _ st l -> 
		  if st.acceptation=Undefined || st.init=Undefined then
		    begin
                      Aorai_option.abort 
                        "Error: the state %s is used but never defined" st.name;
		    end;
		  st::l
	        ) observed_states []
	      in
	      (states , _3) )
# 205 "src/aorai/promelaparser.ml"
               : Promelaast.parsed_automaton))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'states) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 98 "src/aorai/promelaparser.mly"
                                         ( 
	    _1@_3
	  )
# 215 "src/aorai/promelaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 101 "src/aorai/promelaparser.mly"
         ( _1 )
# 222 "src/aorai/promelaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'state_labels) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_body) in
    Obj.repr(
# 105 "src/aorai/promelaparser.mly"
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
# 254 "src/aorai/promelaparser.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_labels) in
    Obj.repr(
# 133 "src/aorai/promelaparser.mly"
                             ( 
	    let (stl1,trl1)=_1 in
	    let (stl2,trl2)=_2 in
	      (stl1@stl2,trl1@trl2) 
	)
# 266 "src/aorai/promelaparser.ml"
               : 'state_labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 138 "src/aorai/promelaparser.mly"
         ( _1 )
# 273 "src/aorai/promelaparser.ml"
               : 'state_labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 142 "src/aorai/promelaparser.mly"
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
                {start=old;stop=old;cross=Seq (to_seq PTrue);numt=(-1)} ::
                !trans;
	    (* If the name includes accept then 
               this state is an acceptation one. *)
	    if (String.length _1>=7) && 
              (String.compare (String.sub _1 0 7) "accept_")=0 
            then
	      old.acceptation <- True;

            (* Step 2 : setting up the init status *)
	    (* If the state name ended with "_init" then 
               it is an initial state. Else, it is not. *)
	    if (String.length _1>=5) && 
              (String.compare (String.sub _1 ((String.length _1)-5) 5) 
                 "_init" ) = 0
	    then
	      old.init <- True
	    else
	      old.init <- False;
	    ([old],!trans)
	  end
	)
# 328 "src/aorai/promelaparser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    Obj.repr(
# 195 "src/aorai/promelaparser.mly"
                                            ( (_2,false) )
# 335 "src/aorai/promelaparser.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "src/aorai/promelaparser.mly"
                ( ([],false) )
# 341 "src/aorai/promelaparser.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "src/aorai/promelaparser.mly"
                 ( ([],true) )
# 347 "src/aorai/promelaparser.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 198 "src/aorai/promelaparser.mly"
                                                            ( ([],true) )
# 353 "src/aorai/promelaparser.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 203 "src/aorai/promelaparser.mly"
                                 ( _1@[_2] )
# 361 "src/aorai/promelaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 204 "src/aorai/promelaparser.mly"
              ( [_1] )
# 368 "src/aorai/promelaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'guard) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 208 "src/aorai/promelaparser.mly"
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
# 387 "src/aorai/promelaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 223 "src/aorai/promelaparser.mly"
                          ( POr(PCall (_1,None), PReturn _1) )
# 394 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 224 "src/aorai/promelaparser.mly"
                         ( PCall (_1,None) )
# 401 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 225 "src/aorai/promelaparser.mly"
                           ( PReturn _1 )
# 408 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 226 "src/aorai/promelaparser.mly"
                ( PTrue )
# 414 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 227 "src/aorai/promelaparser.mly"
                 ( PFalse )
# 420 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 228 "src/aorai/promelaparser.mly"
                     ( PNot _2 )
# 427 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 229 "src/aorai/promelaparser.mly"
                           ( PAnd (_1,_3) )
# 435 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 230 "src/aorai/promelaparser.mly"
                          ( POr (_1,_3) )
# 443 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'guard) in
    Obj.repr(
# 231 "src/aorai/promelaparser.mly"
                                       ( _2 )
# 450 "src/aorai/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 233 "src/aorai/promelaparser.mly"
            ( PRel (Logic_ptree.Neq,PVar _1,PCst(Logic_ptree.IntConstant "0")) )
# 457 "src/aorai/promelaparser.ml"
               : 'guard))
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
