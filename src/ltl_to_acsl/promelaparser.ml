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
# 26 "src/ltl_to_acsl/promelaparser.mly"
open Parsing
open Promelaast
open Bool3


let observed_states=Hashtbl.create 1
let observed_vars=Hashtbl.create 1
let observed_funcs=Hashtbl.create 1


# 39 "src/ltl_to_acsl/promelaparser.ml"
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
# 64 "src/ltl_to_acsl/promelaparser.mly"
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
	    let n=ref 0 in
	    List.iter (fun t -> t.numt<-(!n); n:=!n+1) _3;

	    ((states , _3),observed_vars,observed_funcs)
	)
# 188 "src/ltl_to_acsl/promelaparser.ml"
               : (Promelaast.buchautomata * (string, string) Hashtbl.t  * (string, string) Hashtbl.t)))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'states) in
    Obj.repr(
# 80 "src/ltl_to_acsl/promelaparser.mly"
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
	    let n=ref 0 in
	    List.iter (fun t -> t.numt<-(!n); n:=!n+1) _3;


	    ((states , _3),observed_vars,observed_funcs) )
# 210 "src/ltl_to_acsl/promelaparser.ml"
               : (Promelaast.buchautomata * (string, string) Hashtbl.t  * (string, string) Hashtbl.t)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'states) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 101 "src/ltl_to_acsl/promelaparser.mly"
                                         ( 
	    _1@_3
	    (*let (s1,t1)=$1 in
	    let (s2,t2)=$3 in
	      (s1@s2,t1@t2)*)
	  )
# 223 "src/ltl_to_acsl/promelaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'state) in
    Obj.repr(
# 107 "src/ltl_to_acsl/promelaparser.mly"
         ( _1 )
# 230 "src/ltl_to_acsl/promelaparser.ml"
               : 'states))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'state_labels) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_body) in
    Obj.repr(
# 111 "src/ltl_to_acsl/promelaparser.mly"
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
# 265 "src/ltl_to_acsl/promelaparser.ml"
               : 'state))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'label) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'state_labels) in
    Obj.repr(
# 142 "src/ltl_to_acsl/promelaparser.mly"
                             ( 
	    let (stl1,trl1)=_1 in
	    let (stl2,trl2)=_2 in
	      (stl1@stl2,trl1@trl2) 
	)
# 277 "src/ltl_to_acsl/promelaparser.ml"
               : 'state_labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'label) in
    Obj.repr(
# 147 "src/ltl_to_acsl/promelaparser.mly"
         ( _1 )
# 284 "src/ltl_to_acsl/promelaparser.ml"
               : 'state_labels))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 151 "src/ltl_to_acsl/promelaparser.mly"
                                      (
	  begin
	    let trans = ref [] in
	    let old=
	      try 
		Hashtbl.find observed_states _1
	      with
		| Not_found -> 
		    let s={name=_1;acceptation=Undefined;init=Undefined;nums=(Hashtbl.length observed_states)}
		    in
		      Hashtbl.add observed_states _1 s;
		      s
	    in
	      begin
		try 
		  if (old.acceptation<>False) && (String.sub _1 0 10) = "accept_all" then 
		    begin
		      old.acceptation <- True;
		      trans:={start=old;stop=old;cross=PTrue;numt=(-1)}::!trans
		    end
		  else
		    if (old.acceptation<>False) && (String.sub _1 0 6)  = "accept" then
		      old.acceptation <- True
		    else 
		      old.acceptation <- False
		with Invalid_argument _ -> 
		  if (old.acceptation=Undefined) then 
		    old.acceptation <- False 
	      end;
	      begin
		try 
		  if
		    (old.init<>False) && 
		      let i=(String.index _1 'i') in (String.sub _1 i 4) = "init"  
		  then
		    old.init <- True
		  else
		    old.init <- False
		with  
		    Invalid_argument _ 
		  | Not_found -> 
		      if (old.init=Undefined) then 
			old.init <- False 
	      end;
	      ([old],!trans)
	  end
	)
# 337 "src/ltl_to_acsl/promelaparser.ml"
               : 'label))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    Obj.repr(
# 202 "src/ltl_to_acsl/promelaparser.mly"
                                            ( (_2,false) )
# 344 "src/ltl_to_acsl/promelaparser.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 203 "src/ltl_to_acsl/promelaparser.mly"
                ( ([],false) )
# 350 "src/ltl_to_acsl/promelaparser.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 204 "src/ltl_to_acsl/promelaparser.mly"
                 ( ([],true) )
# 356 "src/ltl_to_acsl/promelaparser.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 205 "src/ltl_to_acsl/promelaparser.mly"
                                                            ( ([],true) )
# 362 "src/ltl_to_acsl/promelaparser.ml"
               : 'state_body))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'transitions) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 210 "src/ltl_to_acsl/promelaparser.mly"
                                 ( _1@[_2] )
# 370 "src/ltl_to_acsl/promelaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'transition) in
    Obj.repr(
# 211 "src/ltl_to_acsl/promelaparser.mly"
              ( [_1] )
# 377 "src/ltl_to_acsl/promelaparser.ml"
               : 'transitions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'guard) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 215 "src/ltl_to_acsl/promelaparser.mly"
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
# 396 "src/ltl_to_acsl/promelaparser.ml"
               : 'transition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 231 "src/ltl_to_acsl/promelaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _1) then Hashtbl.add observed_funcs _1 _1 ; PCallOrReturn _1 )
# 403 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 233 "src/ltl_to_acsl/promelaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _1) then Hashtbl.add observed_funcs _1 _1 ; PCall _1 )
# 410 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 235 "src/ltl_to_acsl/promelaparser.mly"
     ( if not (Hashtbl.mem observed_funcs _1) then Hashtbl.add observed_funcs _1 _1 ; PReturn _1 )
# 417 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 237 "src/ltl_to_acsl/promelaparser.mly"
            ( PTrue )
# 423 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    Obj.repr(
# 239 "src/ltl_to_acsl/promelaparser.mly"
            ( PFalse )
# 429 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 241 "src/ltl_to_acsl/promelaparser.mly"
     ( PNot _2 )
# 436 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 243 "src/ltl_to_acsl/promelaparser.mly"
     ( PAnd (_1,_3) )
# 444 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'guard) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'guard) in
    Obj.repr(
# 245 "src/ltl_to_acsl/promelaparser.mly"
            ( POr (_1,_3) )
# 452 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'guard) in
    Obj.repr(
# 247 "src/ltl_to_acsl/promelaparser.mly"
     ( _2 )
# 459 "src/ltl_to_acsl/promelaparser.ml"
               : 'guard))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 249 "src/ltl_to_acsl/promelaparser.mly"
     ( if not (Hashtbl.mem observed_vars _1) then Hashtbl.add observed_vars _1 _1 ; PIndexedExp _1 )
# 466 "src/ltl_to_acsl/promelaparser.ml"
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : (Promelaast.buchautomata * (string, string) Hashtbl.t  * (string, string) Hashtbl.t))
