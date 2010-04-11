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

/* $Id: ltlparser.mly,v 1.3 2009-02-13 07:59:29 uid562 Exp $ */

/* Originated from http://www.ltl2dstar.de/down/ltl2dstar-0.4.2.zip  */
%{
open Parsing
open Cil_types
open Cil

let observed_expressions=Hashtbl.create 97

let ident_count=ref 0
let get_fresh_ident () =
  ident_count:=!ident_count+1;
  ("buchfreshident"^(string_of_int !ident_count))

%}


%token LTL_TRUE LTL_FALSE LTL_LPAREN LTL_RPAREN

/* Logic operators */
%token LTL_OR LTL_IMPLIES LTL_LEFT_RIGHT_ARROW
%token LTL_AND
%token LTL_NOT
%token LTL_GLOBALLY LTL_FATALLY LTL_UNTIL LTL_RELEASE LTL_NEXT

%right LTL_OR LTL_IMPLIES LTL_LEFT_RIGHT_ARROW
%right LTL_AND
%nonassoc LTL_NOT
%right LTL_GLOBALLY LTL_FATALLY LTL_UNTIL LTL_RELEASE LTL_NEXT


/* Logic relations */
%token LTL_EQ LTL_LT LTL_GT LTL_LE LTL_GE LTL_NEQ
%right LTL_EQ LTL_LT LTL_GT LTL_LE LTL_GE LTL_NEQ


/* Arithmetic relations */
%token LTL_PLUS LTL_MINUS
%token LTL_DIV LTL_STAR LTL_MODULO
%right LTL_PLUS LTL_MINUS LTL_DIV LTL_STAR LTL_MODULO


/* Access */
%token LTL_RIGHT_ARROW LTL_DOT LTL_LEFT_SQUARE LTL_RIGHT_SQUARE LTL_ADRESSE
%token LTL_CALL LTL_RETURN LTL_CALL_OR_RETURN

/* Variables and constants */
%token <string> LTL_INT
%token <string> LTL_LABEL

/* Others */
%token EOF


%type <(Ltlast.formula * (string, (Cil_types.exp* string*Cil_types.predicate)) Hashtbl.t)> ltl
%start ltl
%%

ltl
        : formula EOF {($1,observed_expressions)}
  ;


formula
        : LTL_TRUE
            {Ltlast.LTrue}
        | LTL_FALSE
	    {Ltlast.LFalse}
	| LTL_LPAREN formula LTL_RPAREN
	    { $2 }

	| LTL_GLOBALLY formula
	    { Ltlast.LGlobally($2) }
	| LTL_FATALLY  formula
	    { Ltlast.LFatally($2) }
	| formula LTL_UNTIL formula
	    { Ltlast.LUntil($1,$3) }
	| formula LTL_RELEASE formula
	    { Ltlast.LRelease($1,$3) }
	| LTL_NEXT formula
	    { Ltlast.LNext($2) }

	| formula LTL_OR formula
	    { Ltlast.LOr($1,$3) }
	| formula LTL_AND formula
	    { Ltlast.LAnd($1,$3) }
	| LTL_NOT formula
	    { Ltlast.LNot($2) }
	| formula LTL_IMPLIES formula
	    { Ltlast.LImplies($1,$3) }
	| formula LTL_LEFT_RIGHT_ARROW formula
	    { Ltlast.LIff($1,$3) }

	| LTL_CALL LTL_LPAREN LTL_LABEL LTL_RPAREN
	    { Ltlast.LCall($3)}
	| LTL_RETURN LTL_LPAREN LTL_LABEL LTL_RPAREN
	    { Ltlast.LReturn($3)}
	| LTL_CALL_OR_RETURN LTL_LPAREN LTL_LABEL LTL_RPAREN
	    { Ltlast.LCallOrReturn($3)}

/* returns a string identifer associated, through observed_expressions table, to the represented expression */
	| logic_relation
	    {
	      let id = get_fresh_ident () in
	      let (pred,exp) = $1 in
	        Hashtbl.add observed_expressions id 
		  (exp, (Pretty_utils.sfprintf "%a" Cil.d_exp exp), pred);
	        Ltlast.LIdent(id)
	    }
  ;


/* returns a (Cil_types.predicate,Cil_types.exp) couple of expressions */
logic_relation
	: arith_relation LTL_EQ  arith_relation
            { (	Prel(Cil_types.Req, Logic_utils.expr_to_term ~cast:true $1 ,Logic_utils.expr_to_term ~cast:true  $3),
		new_exp (BinOp(Cil_types.Eq, $1 , $3 , Cil.intType)) )
	    }
	| arith_relation LTL_LT  arith_relation
            { (	Prel(Cil_types.Rlt, Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		new_exp (BinOp(Cil_types.Lt, $1 , $3 , Cil.intType)) )
	    }
	| arith_relation LTL_GT  arith_relation
            { (	Prel(Cil_types.Rgt, Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		new_exp(BinOp(Cil_types.Gt, $1 , $3 , Cil.intType)) )
	    }
	| arith_relation LTL_LE  arith_relation
            { (	Prel(Cil_types.Rle, Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		new_exp (BinOp(Cil_types.Le, $1 , $3 , Cil.intType) ))
	    }
	| arith_relation LTL_GE  arith_relation
            { (	Prel(Cil_types.Rge, Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		new_exp (BinOp(Cil_types.Ge, $1 , $3 , Cil.intType) ))
	    }
	| arith_relation LTL_NEQ arith_relation
            { (	Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true $1 , Logic_utils.expr_to_term ~cast:true $3),
		new_exp (BinOp(Cil_types.Ne , $1 , $3 , Cil.intType) ))
	    }
	| arith_relation
	    { (	Prel(Cil_types.Rneq,Logic_utils.expr_to_term ~cast:true $1 ,
		     Logic_const.term
		       (TConst( CInt64(Int64.of_int 0,IInt,Some("0"))))
		       (Ctype Cil.intType)),
		$1)
	    }

  ;

/* returns a Cil_types.exp expression */
arith_relation
        : arith_relation_mul LTL_PLUS arith_relation
            { new_exp (BinOp(Cil_types.PlusA, $1 , $3 , Cil.intType)) }
	| arith_relation_mul LTL_MINUS arith_relation
            { new_exp (BinOp(Cil_types.MinusA, $1 , $3 , Cil.intType)) }
	| arith_relation_mul
	    { $1 }
  ;


arith_relation_mul
	: arith_relation_mul LTL_DIV access_or_const
            { new_exp (BinOp(Cil_types.Div, $1 , $3 , Cil.intType)) }
	| arith_relation_mul LTL_STAR access_or_const
            { new_exp (BinOp(Cil_types.Mult, $1 , $3 , Cil.intType)) }
	| arith_relation_mul LTL_MODULO access_or_const
            { new_exp (BinOp(Cil_types.Mod, $1 , $3 , Cil.intType)) }
	| access_or_const
	    { $1 }
  ;

/* returns a Lval exp or a Const exp*/
access_or_const
        : LTL_INT
            { new_exp (Const(CInt64(Int64.of_string $1,IInt, Some($1))))}
	| access
            { new_exp (Lval($1)) }
	| LTL_LPAREN arith_relation LTL_RPAREN
	    { $2 }
  ;


/* returns a lval */
access
	: access_array LTL_RIGHT_ARROW  access
            { Aorai_option.fatal "NOT YET IMPLEMENTED : A->B pointed structure filed access." }
	| access LTL_DOT LTL_LABEL
            { let (my_host,my_offset) = ($1) in
              
              let new_offset = Utils_parser.add_offset my_offset (Utils_parser.get_new_offset my_host my_offset $3) in
              (my_host,new_offset)}
	| access_array
	    {$1}

access_array
	: access_array LTL_LEFT_SQUARE access_or_const LTL_RIGHT_SQUARE
	    { Cil.addOffsetLval (Index ($3,NoOffset)) $1}
    	| access_leaf
	    {$1}


access_leaf
        : LTL_ADRESSE access
            { Aorai_option.fatal "NOT YET IMPLEMENTED : &A 'address of' access." }
	| LTL_STAR access
            { Aorai_option.fatal "NOT YET IMPLEMENTED : *A dereferencement access."}
	| LTL_LABEL
	    { Cil.var ( Data_for_aorai.get_varinfo $1) }
	| LTL_LPAREN access LTL_RPAREN
	    { $2 }

  ;
