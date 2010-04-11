/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2010                                               */
/*    INSA  (Institut National des Sciences Appliquees)                   */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

/* $Id: promelaparser_withexps.mly,v 1.2 2008-10-02 13:33:29 uid588 Exp $ */

/* Originated from http://www.ltl2dstar.de/down/ltl2dstar-0.4.2.zip  */
%{
open Parsing
open Promelaast
open Bool3


let observed_states=Hashtbl.create 1
let observed_vars=Hashtbl.create 1
let observed_funcs=Hashtbl.create 1

let observed_expressions=Hashtbl.create 97


(* Current observed expr contains : *)
type observed_expr = Func_ret of string                      (* func name : a return of the given func *)
		     | Func_param of string * (string list)  (* Func name * param : a call with given param *)
		     | Only_vars                             (* Only constants and variables *)

let observed_expr_is_param = ref Only_vars


let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))

%}



%token PROMELA_OR
%token PROMELA_AND
%token PROMELA_NOT PROMELA_TRUE PROMELA_FALSE

%right PROMELA_OR
%right PROMELA_AND
%nonassoc PROMELA_NOT PROMELA_TRUE PROMELA_FALSE
 
%token PROMELA_NEVER PROMELA_IF PROMELA_FI PROMELA_GOTO PROMELA_SKIP
%token <string> PROMELA_LABEL
%token <string> PROMELA_INT
%token PROMELA_COLON PROMELA_SEMICOLON PROMELA_DOUBLE_COLON 
%token PROMELA_LBRACE PROMELA_RBRACE PROMELA_LPAREN
%token PROMELA_RPAREN PROMELA_RIGHT_ARROW
%token PROMELA_TRUE PROMELA_FALSE


/* Logic relations */
%token PROMELA_EQ PROMELA_LT PROMELA_GT PROMELA_LE PROMELA_GE PROMELA_NEQ
%right PROMELA_EQ PROMELA_LT PROMELA_GT PROMELA_LE PROMELA_GE PROMELA_NEQ


/* Arithmetic relations */
%token PROMELA_PLUS PROMELA_MINUS
%token PROMELA_DIV PROMELA_STAR PROMELA_MODULO
%right PROMELA_PLUS PROMELA_MINUS PROMELA_DIV PROMELA_STAR PROMELA_MODULO


/* Access */
%token PROMELA_DOT PROMELA_LEFT_SQUARE PROMELA_RIGHT_SQUARE
%token <string> PROMELA_CALLOF  PROMELA_RETURNOF  PROMELA_CALLORRETURNOF
%token EOF
%token PROMELA_FUNC


%type <(Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t)> promela
%start promela
%%

promela
        : PROMELA_NEVER PROMELA_LBRACE states PROMELA_RBRACE EOF { 
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
	    Data_for_aorai.setLtl_expressions observed_expressions;
	    Logic_simplification.setLtl_expressions observed_expressions;
	    let n=ref 0 in
	    let transitions = Logic_simplification.simplifyTrans $3 in
	    List.iter (fun t -> t.numt<-(!n); n:=!n+1) transitions;

	    ((states , transitions),observed_vars,observed_funcs)
	}
        | PROMELA_NEVER PROMELA_LBRACE states PROMELA_SEMICOLON PROMELA_RBRACE EOF {
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
	    Data_for_aorai.setLtl_expressions observed_expressions;
	    Logic_simplification.setLtl_expressions observed_expressions;
	    let n=ref 0 in
	    let transitions = Logic_simplification.simplifyTrans $3 in
	    List.iter (fun t -> t.numt<-(!n); n:=!n+1) transitions;


	    ((states , transitions),observed_vars,observed_funcs) }
  ;



states   
        : states PROMELA_SEMICOLON state { 
	    $1@$3
	    (*let (s1,t1)=$1 in
	    let (s2,t2)=$3 in
	      (s1@s2,t1@t2)*)
	  }
	| state { $1 }
        ;

state 
        : state_labels state_body {
	  let (stl,trans)=$1 in
	  let (trl,force_final)=$2 in
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
	      



	}
        ;

state_labels
        : label state_labels { 
	    let (stl1,trl1)=$1 in
	    let (stl2,trl2)=$2 in
	      (stl1@stl2,trl1@trl2) 
	}
	| label { $1 }
        ;

label   
        : PROMELA_LABEL PROMELA_COLON {
	  begin
    (* Step 0 : trans is the set of new transitions and old is the description of the current state *)
	    let trans = ref [] in
	    (* Promela Label is a state. According to its name, we will try to give him its properties (init / accept) *)
	    (* Firstly, if this state is still referenced, then we get it back. Else, we make a new "empty" state *)
	    let old= 
	      try  
		Hashtbl.find observed_states $1
	      with
		| Not_found -> 
		    let s={name=$1;acceptation=Undefined;init=Undefined;nums=(Hashtbl.length observed_states)} in
		    Hashtbl.add observed_states $1 s;
		    s
	    in
    (* Step 1 : setting up the acceptance status *)
	    (* Default status : Non acceptation state *)
 	    old.acceptation <- False;
	    
	    (* Accept_all state means acceptance state with a reflexive transition without cross condition *)
	    (* This case is not exlusive with the following. Acceptation status is set in this last. *)
	    if (String.length $1>=10) && (String.compare (String.sub $1 0 10) "accept_all")=0 then 
	      trans:={start=old;stop=old;cross=PTrue;numt=(-1)}::!trans;
	    
	    (* If the name includes accept then this state is an acceptation one. *)
	    if (String.length $1>=7) && (String.compare (String.sub $1 0 7) "accept_")=0 then
	      old.acceptation <- True;

    (* Step 2 : setting up the init status *)
	    (* If the state name ended with "_init" then it is an initial state. Else, it is not. *)
	    if (String.length $1>=5) && (String.compare (String.sub $1 ((String.length $1)-5) 5) "_init" ) = 0
	    then  
	      old.init <- True
	    else
	      old.init <- False;
	    
	    ([old],!trans)
	  end
	}
        ;


state_body
        : PROMELA_IF transitions PROMELA_FI { ($2,false) }
	| PROMELA_SKIP { ([],false) }
	| PROMELA_FALSE { ([],true) }
	| PROMELA_IF PROMELA_DOUBLE_COLON PROMELA_FALSE PROMELA_FI { ([],true) }
        ; 


transitions
        : transitions transition { $1@[$2] }
	| transition { [$1] }
        ;

transition
        : PROMELA_DOUBLE_COLON guard PROMELA_RIGHT_ARROW PROMELA_GOTO PROMELA_LABEL {
	  let s=
	    try
	      Hashtbl.find observed_states $5
	    with
		Not_found -> 
		  let r={name=$5;init=Undefined;acceptation=Undefined;nums=(Hashtbl.length observed_states)}  in
		    Hashtbl.add observed_states $5 r;
		    r
	  in
	    ($2,s)
	}
        ;

guard
	: PROMELA_CALLORRETURNOF 
	    { if not (Hashtbl.mem observed_funcs $1) then Hashtbl.add observed_funcs $1 $1 ; PCallOrReturn $1 } 
        | PROMELA_CALLOF 
	    { if not (Hashtbl.mem observed_funcs $1) then Hashtbl.add observed_funcs $1 $1 ; PCall $1 }
        | PROMELA_RETURNOF 
	    { if not (Hashtbl.mem observed_funcs $1) then Hashtbl.add observed_funcs $1 $1 ; PReturn $1 }
	| PROMELA_TRUE
            { PTrue }
	| PROMELA_FALSE
            { PFalse }
	| PROMELA_NOT guard
	    { PNot $2 }
	| guard PROMELA_AND guard
	    { PAnd ($1,$3) }
	| guard PROMELA_OR guard
            { POr ($1,$3) }
	| PROMELA_LPAREN guard PROMELA_RPAREN
	    { $2 }


/* returns a string identifer associated, through observed_expressions table, to the represented expression */
        | logic_relation
	    {

	      let id = get_fresh_ident () in
	      let (pred,exp) = $1 in
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
	    }
   ;




/* returns a (Cil_types.predicate,Cil_types.exp) couple of expressions */
logic_relation
	: arith_relation PROMELA_EQ  arith_relation
            { (	Cil_types.Prel(Cil_types.Req, Logic_utils.expr_to_term ~cast:true $1 ,Logic_utils.expr_to_term  ~cast:true $3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Eq, $1 , $3 , Cil.intType)) )
	    }
	| arith_relation PROMELA_LT  arith_relation
            { (	Cil_types.Prel(Cil_types.Rlt, Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Lt, $1 , $3 , Cil.intType)) )
	    }
	| arith_relation PROMELA_GT  arith_relation
            { (	Cil_types.Prel(Cil_types.Rgt, Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		Cil.new_exp(Cil_types.BinOp(Cil_types.Gt, $1 , $3 , Cil.intType)) )
	    }
	| arith_relation PROMELA_LE  arith_relation
            { (	Cil_types.Prel(Cil_types.Rle, Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Le, $1 , $3 , Cil.intType) ))
	    }
	| arith_relation PROMELA_GE  arith_relation
            { (	Cil_types.Prel(Cil_types.Rge, Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Ge, $1 , $3 , Cil.intType) ))
	    }
	| arith_relation PROMELA_NEQ arith_relation
            { (	Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		Cil.new_exp (Cil_types.BinOp(Cil_types.Ne , $1 , $3 , Cil.intType) ))
	    }
	| arith_relation
	    { (	Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true $1 ,
		     Logic_const.term
		       (Cil_types.TConst(Cil_types.CInt64(Int64.of_int 0,Cil_types.IInt,Some("0"))))
		       (Cil_types.Ctype Cil.intType)),
		$1)
	    }

  ;

/* returns a Cil_types.exp expression */
arith_relation
        : arith_relation_mul PROMELA_PLUS arith_relation
            { Cil.new_exp (Cil_types.BinOp(Cil_types.PlusA, $1 , $3 , Cil.intType)) }
	| arith_relation_mul PROMELA_MINUS arith_relation
            { Cil.new_exp (Cil_types.BinOp(Cil_types.MinusA, $1 , $3 , Cil.intType)) }
	| arith_relation_mul
	    { $1 }
  ;


arith_relation_mul
	: arith_relation_mul PROMELA_DIV access_or_const
            { Cil.new_exp (Cil_types.BinOp(Cil_types.Div, $1 , $3 , Cil.intType)) }
	| arith_relation_mul PROMELA_STAR access_or_const
            { Cil.new_exp (Cil_types.BinOp(Cil_types.Mult, $1 , $3 , Cil.intType)) }
	| arith_relation_mul PROMELA_MODULO access_or_const
            { Cil.new_exp (Cil_types.BinOp(Cil_types.Mod, $1 , $3 , Cil.intType)) }
	| access_or_const
	    { $1 }
  ;

/* returns a Lval exp or a Const exp*/
access_or_const
        : PROMELA_INT
            { Cil.new_exp (Cil_types.Const(Cil_types.CInt64(Int64.of_string $1,Cil_types.IInt, Some($1))))}
	| access
            { Cil.new_exp (Cil_types.Lval($1)) }
	| PROMELA_LPAREN arith_relation PROMELA_RPAREN
	    { $2 }
  ;


/* returns a lval */
access
	: access PROMELA_DOT PROMELA_LABEL
            { 
              let (my_host,my_offset) = ($1) in
              
              let new_offset = Utils_parser.add_offset my_offset (Utils_parser.get_new_offset my_host my_offset $3) in
              (my_host,new_offset)}
						 
	| access_array
	    {$1}

access_array
	: access_array PROMELA_LEFT_SQUARE access_or_const PROMELA_RIGHT_SQUARE
	    { Cil.addOffsetLval (Cil_types.Index ($3,Cil_types.NoOffset)) $1}
    	| access_leaf
	    {$1}


access_leaf
        : PROMELA_STAR access
            { Aorai_option.fatal "NOT YET IMPLEMENTED : *A dereferencement access." }



	| PROMELA_LABEL PROMELA_FUNC PROMELA_DOT PROMELA_LABEL
            { 
	      if(String.compare $4 "return")=0 then
		begin
		  if not (!observed_expr_is_param=Only_vars) then 
		    Aorai_option.abort "An expression can not contain at same time a reference of a returned value and itself or a reference to a param";
		  
		  observed_expr_is_param := Func_ret $1;
		  Cil.var ( Data_for_aorai.get_returninfo $1)
		end
	      else
		begin
		  match !observed_expr_is_param with
		    | Func_ret _ -> 
			Aorai_option.abort "An expression can not contain both a reference of a returned value and another reference to itself or a reference to a param";
			
		    | Func_param (f,_) when not (f=$1) -> 
			Aorai_option.abort "An expression can not contain both references two different called functions.";
			
		    | Only_vars -> 
			observed_expr_is_param:=Func_param ($1,[$4]);
			Cil.var ( Data_for_aorai.get_paraminfo $1 $4)
			  
		    | Func_param (_,l) -> 
			observed_expr_is_param:=Func_param ($1,$4::l);
			Cil.var ( Data_for_aorai.get_paraminfo $1 $4)
		end
	    }
	    



	| PROMELA_LABEL
	    { Cil.var ( Data_for_aorai.get_varinfo $1) }
	| PROMELA_LPAREN access PROMELA_RPAREN
	    { $2 }

  ;
