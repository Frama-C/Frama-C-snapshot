/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2009                                               */
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


%}


 
%token CALL_OF  RETURN_OF  CALLORRETURN_OF
%token <string> IDENTIFIER
%token <string> INT
%token LCURLY RCURLY LPAREN RPAREN LSQUARE RSQUARE
%token RARROW
%token TRUE FALSE
%token NOT DOT AMP 
%token COLON SEMI_COLON COMMA PIPE
%token EQ LT GT LE GE NEQ PLUS MINUS SLASH STAR PERCENT OR AND
%token EOF


%right EQ LT GT LE GE NEQ PLUS MINUS SLASH STAR PERCENT OR AND
%nonassoc NOT TRUE FALSE


%type <(Promelaast.buchautomata * (string, string) Hashtbl.t * (string, string) Hashtbl.t)> main
%start main
%%


main
  : options states { 
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
  end) $1
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
	   let transitions = Ltl_logic.simplifyTrans $2 in
	   List.iter (fun t -> t.numt<-(!n); n:=!n+1) transitions;
	   
	   ((states , transitions),observed_vars,observed_funcs)
	}
  ;


options
  : options option { $1@[$2] }
  | option         { [$1] }
  ;

option
  : PERCENT IDENTIFIER COLON opt_identifiers SEMI_COLON { ($2, $4) }
  ;

opt_identifiers
  : opt_identifiers COMMA IDENTIFIER { $1@[$3] }
  | IDENTIFIER                       { [$1] }
  ;





states   
  : states state { $1@$2 }
  | state { $1 }
  ;


state 
  : IDENTIFIER COLON transitions SEMI_COLON {
      let start_state = fetch_and_create_state $1 in
      List.map (fun(cross,stop_state) -> { start=start_state; stop=stop_state;
					   cross=cross;       numt=(-1) }) $3 }
  ;


transitions  /*=>  [transition; ...] */
  : transitions PIPE transition { $1@[$3] }
  | transition { [$1] }
  ;


transition  /*=>  (guard, state) */
  : LCURLY guard RCURLY RARROW IDENTIFIER { ($2, fetch_and_create_state $5) }
  | RARROW IDENTIFIER { (PTrue, fetch_and_create_state $2) }
  ;



guard
	: CALLORRETURN_OF  LPAREN IDENTIFIER RPAREN
	    { if not (Hashtbl.mem observed_funcs $3) then Hashtbl.add observed_funcs $3 $3 ; PCallOrReturn $3 } 
        | CALL_OF  LPAREN IDENTIFIER RPAREN
	    { if not (Hashtbl.mem observed_funcs $3) then Hashtbl.add observed_funcs $3 $3 ; PCall $3 }
        | RETURN_OF  LPAREN IDENTIFIER RPAREN
	    { if not (Hashtbl.mem observed_funcs $3) then Hashtbl.add observed_funcs $3 $3 ; PReturn $3 }
	| TRUE
            { PTrue }
	| FALSE
            { PFalse }
	| NOT guard
	    { PNot $2 }
	| guard AND guard
	    { PAnd ($1,$3) }
	| guard OR guard
            { POr ($1,$3) }
	| LPAREN guard RPAREN
	    { $2 }
        | logic_relation
	    { 

	      let id = get_fresh_ident () in
	      let (pred,exp) = $1 in
	      Hashtbl.add observed_expressions id 
		(exp, (Pretty_utils.sfprintf "%a" Cil.d_exp exp), pred);
	      (*Ltlast.LIdent(id)*)

	      Hashtbl.add observed_vars id id ; 
	      PIndexedExp id  
	    } 
   ;




/* returns a (Cil_types.predicate,Cil_types.exp) couple of expressions */
logic_relation
  : arith_relation EQ arith_relation {
    ( Cil_types.Prel(Cil_types.Req, Logic_utils.expr_to_term $1,
	  	                    Logic_utils.expr_to_term $3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Eq, $1 , $3 , Cil.intType)) ) }
  | arith_relation LT arith_relation {
    ( Cil_types.Prel(Cil_types.Rlt, Logic_utils.expr_to_term $1,
		                    Logic_utils.expr_to_term $3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Lt, $1 , $3 , Cil.intType)) ) }
  | arith_relation GT arith_relation { 
    ( Cil_types.Prel(Cil_types.Rgt, Logic_utils.expr_to_term $1,
		                    Logic_utils.expr_to_term $3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Gt, $1 , $3 , Cil.intType)) ) }
  | arith_relation LE  arith_relation {
    ( Cil_types.Prel(Cil_types.Rle, Logic_utils.expr_to_term $1,
		                    Logic_utils.expr_to_term $3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Le, $1 , $3 , Cil.intType)) ) }
  | arith_relation GE arith_relation {
    ( Cil_types.Prel(Cil_types.Rge, Logic_utils.expr_to_term $1,
		                    Logic_utils.expr_to_term $3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Ge, $1 , $3 , Cil.intType) )) }
  | arith_relation NEQ arith_relation {
    ( Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term $1,
		                    Logic_utils.expr_to_term $3),
      Cil.new_exp(Cil_types.BinOp(Cil_types.Ne, $1 , $3 , Cil.intType) )) }
  | arith_relation {
    ( Cil_types.Prel(Cil_types.Rneq,Logic_utils.expr_to_term $1,
		     Logic_const.term(Cil_types.TConst(Cil_types.CInt64(Int64.of_int 0,Cil_types.IInt,Some("0"))))
		       (Cil_types.Ctype Cil.intType)), $1) }
  ;

/* returns a Cil_types.exp expression */
arith_relation
  : arith_relation_mul PLUS arith_relation {
    Cil.new_exp (Cil_types.BinOp(Cil_types.PlusA, $1 , $3 , Cil.intType)) }
  | arith_relation_mul MINUS arith_relation {
    Cil.new_exp (Cil_types.BinOp(Cil_types.MinusA, $1 , $3 , Cil.intType)) }
  | arith_relation_mul { $1 }
  ;


arith_relation_mul
  : arith_relation_mul SLASH access_or_const {
    Cil.new_exp (Cil_types.BinOp(Cil_types.Div, $1 , $3 , Cil.intType)) }
  | arith_relation_mul STAR access_or_const {
    Cil.new_exp (Cil_types.BinOp(Cil_types.Mult, $1 , $3 , Cil.intType)) }
  | arith_relation_mul PERCENT access_or_const {
    Cil.new_exp (Cil_types.BinOp(Cil_types.Mod, $1 , $3 , Cil.intType)) }
  | access_or_const { $1 }
  ;

/* returns a Lval exp or a Const exp*/
access_or_const
  : INT
            { Cil.new_exp (Cil_types.Const(Cil_types.CInt64(Int64.of_string $1,Cil_types.IInt, Some($1))))}
  | access
            { Cil.new_exp (Cil_types.Lval($1)) }
  | LPAREN arith_relation RPAREN
	    { $2 }
  ;



/* returns a lval */
access
  : access DOT IDENTIFIER
            { 
                            
              let (my_host,my_offset) = ($1) in
              
              let new_offset = Utils_parser.add_offset my_offset (Utils_parser.get_new_offset my_host my_offset $3) in
              (my_host,new_offset)
                         
                            
                             (*Ltl_to_acsl_option.fatal "NOT YET IMPLEMENTED : A.B structure filed access." *)}
  | access_array
	    {$1}

access_array
  : access_array LSQUARE access_or_const RSQUARE
	    { Cil.addOffsetLval (Cil_types.Index ($3,Cil_types.NoOffset)) $1}
  | access_leaf	    {$1}
  ;

access_leaf
  : STAR access
 { Ltl_to_acsl_option.fatal "NOT YET IMPLEMENTED : *A dereferencement access." }
  | IDENTIFIER
	    { Cil.var ( Data_for_ltl.get_varinfo $1) }
  | LPAREN access RPAREN
	    { $2 }

  ;
