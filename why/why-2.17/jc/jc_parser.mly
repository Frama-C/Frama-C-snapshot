/**************************************************************************/
/*                                                                        */
/*  The Why platform for program certification                            */
/*  Copyright (C) 2002-2008                                               */
/*    Romain BARDOU                                                       */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
/*    Christine PAULIN                                                    */
/*    Yann RÉGIS-GIANAS                                                   */
/*    Nicolas ROUSSET                                                     */
/*    Xavier URBAIN                                                       */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2, with the special exception on linking              */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* $Id: jc_parser.mly,v 1.125 2008/12/09 09:14:18 marche Exp $ */

%{

  open Format
  open Jc_env
  open Jc_ast
  open Jc_pervasives
  open Parsing
  open Error
  open Jc_constructors

  let pos () = (symbol_start_pos (), symbol_end_pos ())
  let pos_i i = (rhs_start_pos i, rhs_end_pos i)

  let locate n = new node_positioned ~pos:(pos ()) n
  let locate_identifier n = new identifier ~pos:(pos ()) n

  let skip = locate (JCPEconst JCCvoid)

  let label s = match s with
    | "Pre" -> LabelPre
    | "Old" -> LabelOld
    | "Here" -> LabelHere
    | _ -> 
	LabelName { 
	  label_info_name = s; 
	  label_info_final_name = s;
	  times_used = 0;
	}


%}

%token <string> IDENTIFIER
%token <Jc_ast.const> CONSTANT
%token <string> STRING_LITERAL 
%token NULL

/* ( ) () { } [ ] .. ... */
%token LPAR RPAR LPARRPAR LBRACE RBRACE LSQUARE RSQUARE DOTDOT DOTDOTDOT

/* ; ;; , : . ? */
%token SEMICOLON SEMISEMI COMMA COLON DOT QUESTION

/* - -- + ++ * / % */
%token MINUS MINUSMINUS PLUS PLUSPLUS STAR SLASH PERCENT
 
/* = <= >= > < == != <: :> */
%token EQ LTEQ GTEQ GT LT EQEQ BANGEQ LTCOLON COLONGT

/* += -= *= /= %= */
%token PLUSEQ MINUSEQ STAREQ SLASHEQ PERCENTEQ

/* && || => <=> ! */
%token AMPAMP BARBAR EQEQGT LTEQEQGT EXCLAM

/* if then else return while break for fo break continue case switch default goto */
%token IF THEN ELSE RETURN WHILE FOR DO BREAK CONTINUE CASE SWITCH DEFAULT GOTO

/* exception of throw try catch finally new free let in var */
%token EXCEPTION OF THROW TRY CATCH FINALLY NEW FREE LET IN VAR

/* pack unpack assert */
%token PACK UNPACK ASSERT ASSUME HINT

/* type invariant logic axiomatic with variant and axiom tag match */
%token TYPE INVARIANT LOGIC AXIOMATIC WITH VARIANT AND AXIOM LEMMA TAG MATCH

/* integer boolean real double unit void rep */
%token INTEGER BOOLEAN REAL DOUBLE FLOAT UNIT REP

/* assigns assumes behavior ensures requires throws reads */
%token ASSIGNS ASSUMES BEHAVIOR ENSURES REQUIRES THROWS READS

/* \forall \exists \offset_max \offset_min \address \old \result \mutable \typeof \bottom \typeeq \absolute_address */
%token BSFORALL BSEXISTS BSOFFSET_MAX BSOFFSET_MIN BSADDRESS BSOLD BSAT 
%token BSRESULT BSMUTABLE BSTYPEOF BSBOTTOM BSTYPEEQ BSABSOLUTE_ADDRESS
%token BSBASE_BLOCK

/* \nothing */
%token BSNOTHING

/* as _ match -> end */
%token AS UNDERSCORE MATCH MINUSGT END

/* & ~ ^ | << >> >>> */
%token AMP TILDE HAT PIPE LSHIFT LRSHIFT ARSHIFT

/* |= &= ^= */
%token BAREQ AMPEQ CARETEQ

/* @ (string concat) */
%token AT

%token EOF
%type <Jc_ast.pexpr Jc_ast.decl list> file
%start file

/* precedences on expressions (from lowest to highest) */

%nonassoc PRECLOOPANNOT
%nonassoc FOR

%nonassoc PRECIF THEN
%nonassoc ELSE

%nonassoc PRECTRY
%nonassoc FINALLY

%nonassoc PRECLOGIC
%nonassoc EQ

%nonassoc PRECTYPE
/* and */
%right AND

/* precedences on expressions  */

%nonassoc RETURN ASSERT ASSUME THROW precwhile
%nonassoc COLON
%nonassoc PRECFORALL 
/* <=> */
%right LTEQEQGT
/* => */
%right EQEQGT 
%left QUESTION ASSIGNOP
%left BARBAR
%left AMPAMP
%left PIPE
%left AS
%left HAT
%left AMP
%left LT GT LTEQ GTEQ EQEQ BANGEQ COLONGT LTCOLON
%nonassoc DOTDOT
/* unary -, unary +, ++, --, ! ~ */
%nonassoc UMINUS UPLUS PLUSPLUS MINUSMINUS EXCLAM TILDE
/* . */
%nonassoc DOT



%%

/****************************************/
/* a file is a sequence of declarations */
/****************************************/

file: 
| decl file 
    { $1::$2 }
| tag_and_type_decl file
    { let tag, ty = $1 in tag::ty::$2 }
| EOF 
    { [] }
;

decl: 
| variable_definition
    { $1 }
| function_definition
    { $1 }
| tag_definition
    { $1 }
| type_definition
    { $1 }
/*
| axiom_definition
    { $1 }
*/
| global_invariant_definition
    { $1 }
| exception_definition
    { $1 }
| logic_definition
    { $1 }
/*
| error
    { Jc_options.parsing_error (pos ()) "'type' or type expression expected" }
*/
;


/*******************/
/* type definition */	      
/*******************/

type_definition:
| TYPE IDENTIFIER EQ int_constant DOTDOT int_constant
    { locate (JCDenum_type($2,$4,$6)) }
/*
| LOGIC TYPE IDENTIFIER
    { locate (JCDlogic_type($3)) }
*/
| TYPE IDENTIFIER EQ LSQUARE variant_tag_list RSQUARE
    { locate (JCDvariant_type($2, $5)) }
| TYPE IDENTIFIER EQ LSQUARE union_tag_list RSQUARE
    { locate (JCDunion_type($2,false,$5)) }
| TYPE IDENTIFIER EQ LSQUARE discr_union_tag_list RSQUARE
    { locate (JCDunion_type($2,true,$5)) }
;

int_constant:
| CONSTANT 
    { num_of_constant (pos_i 1)$1 }
| MINUS CONSTANT
    { Num.minus_num (num_of_constant (pos_i 2) $2) }
;

variant_tag_list:
| identifier PIPE variant_tag_list
    { $1::$3 }
| identifier
    { [ $1 ] }
;

union_tag_list:
| identifier AMP union_tag_list
    { $1::$3 }
| identifier AMP identifier
    { [ $1; $3 ] }
;

discr_union_tag_list:
| identifier HAT discr_union_tag_list
    { $1::$3 }
| identifier HAT identifier
    { [ $1; $3 ] }
;

/******************/
/* tag definition */
/******************/

tag_definition:
| TAG IDENTIFIER LT type_parameter_names GT EQ
    extends LBRACE field_declaration_list RBRACE
    { let (f,i) = $9 in locate (JCDtag($2,$4,$7,f,i)) }
| TAG IDENTIFIER EQ
    extends LBRACE field_declaration_list RBRACE
    { let (f,i) = $6 in locate (JCDtag($2,[],$4,f,i)) }
;

type_parameter_names:
| IDENTIFIER COMMA type_parameter_names
    { $1::$3 }
| IDENTIFIER
    { [$1] }
;

extends:
| /* epsilon */
    { None }
| IDENTIFIER WITH
    { Some($1, []) }
| IDENTIFIER LT type_parameters GT WITH
    { Some($1, $3) }
;

field_declaration_list:
| /* epsilon */
    { ([],[]) }
| field_declaration field_declaration_list
    { let (f,i) = $2 in ($1::f,i) }
| invariant field_declaration_list
    { let (f,i) = $2 in (f,$1::i) }
;

field_declaration:
| type_expr IDENTIFIER SEMICOLON
    { (false, $1, $2, None) }
| type_expr IDENTIFIER COLON int_constant SEMICOLON
    { (false, $1, $2, Some (Num.int_of_num $4)) }
| REP type_expr IDENTIFIER SEMICOLON
    { (true, $2, $3, None) }
| REP type_expr IDENTIFIER COLON int_constant SEMICOLON
    { (true, $2, $3, Some (Num.int_of_num $5)) }
;

invariant:
| INVARIANT identifier LPAR IDENTIFIER RPAR EQ expression SEMICOLON
    { ($2,$4,$7) }
;

/********************************/
/* tag and type syntactic suger */
/********************************/

tag_and_type_decl:
| TYPE IDENTIFIER EQ extends LBRACE field_declaration_list RBRACE
    { let (f,i) = $6 in
      let id = locate_identifier $2 in
      locate (JCDtag($2, [], $4, f, i)),
      locate (JCDvariant_type($2, [id])) }
| TYPE IDENTIFIER LT type_parameter_names GT EQ
    extends LBRACE field_declaration_list RBRACE
    { let (f,i) = $9 in
      let id = locate_identifier $2 in
      locate (JCDtag($2, $4, $7, f, i)),
      locate (JCDvariant_type($2, [id])) }
;

/***********************/
/* Function definition */
/***********************/


parameters:
| LPARRPAR
    { [] }
| LPAR RPAR
    { [] }
| LPAR parameter_comma_list RPAR
    { $2 } 
;

parameter_comma_list: 
| parameter_declaration 
    { [$1] }
| parameter_declaration COMMA parameter_comma_list 
    { $1 :: $3 }
;

parameter_declaration:
| type_expr IDENTIFIER
    { ($1,$2) }
;


type_expr:
| INTEGER
    { locate (JCPTnative Tinteger) }
| BOOLEAN
    { locate (JCPTnative Tboolean) }
| REAL
    { locate (JCPTnative Treal) }
| DOUBLE
    { locate (JCPTnative Tdouble) }
| FLOAT
    { locate (JCPTnative Tfloat) }
| UNIT
    { locate (JCPTnative Tunit) }
| IDENTIFIER
    { locate (JCPTidentifier $1) }

| IDENTIFIER LSQUARE DOTDOT RSQUARE
    { locate (JCPTpointer($1,[],None,None)) }
| IDENTIFIER LSQUARE int_constant RSQUARE
    { let n = $3 in
      locate (JCPTpointer($1,[],Some n,Some n)) }
| IDENTIFIER LSQUARE int_constant DOTDOT RSQUARE
    { let n = $3 in
      locate (JCPTpointer($1,[],Some n,None)) }
| IDENTIFIER LSQUARE int_constant DOTDOT int_constant RSQUARE
    { let n, m = $3, $5 in
      locate (JCPTpointer($1,[],Some n,Some m)) }
| IDENTIFIER LSQUARE DOTDOT int_constant RSQUARE
    { let m = $4 in
      locate (JCPTpointer($1,[],None,Some m)) }

| IDENTIFIER LT type_parameters GT LSQUARE DOTDOT RSQUARE
    { locate (JCPTpointer($1,$3,None,None)) }
| IDENTIFIER LT type_parameters GT LSQUARE int_constant RSQUARE
    { let n = $6 in
      locate (JCPTpointer($1,$3,Some n,Some n)) }
| IDENTIFIER LT type_parameters GT LSQUARE int_constant DOTDOT RSQUARE
    { let n = $6 in
      locate (JCPTpointer($1,$3,Some n,None)) }
| IDENTIFIER LT type_parameters GT
    LSQUARE int_constant DOTDOT int_constant RSQUARE
    { let n, m = $6, $8 in
      locate (JCPTpointer($1,$3,Some n,Some m)) }
| IDENTIFIER LT type_parameters GT LSQUARE DOTDOT int_constant RSQUARE
    { let m = $7 in
      locate (JCPTpointer($1,$3,None,Some m)) }
;

type_parameters:
| type_expr COMMA type_parameters
    { $1::$3 }
| type_expr
    { [$1] }
;

function_specification:
| /* epsilon */ 
    { [] }
| spec_clause function_specification 
    { $1::$2 }
;

spec_clause:
| REQUIRES expression SEMICOLON
    { JCCrequires($2) }
| behavior
  { JCCbehavior $1 }
;

behavior:
| BEHAVIOR ident_or_default COLON throws assumes requires assigns 
  ENSURES expression SEMICOLON
    { (pos_i 2,$2,$4,$5,$6,$7,$9) }
;

ident_or_default:
| IDENTIFIER { $1 }
| DEFAULT { "default" }

throws:
| /* epsilon */
    { None }
| THROWS identifier SEMICOLON
    { Some $2 }
;

assumes:
| /* epsilon */
    { None }
| ASSUMES expression SEMICOLON
    { Some $2 }
;

requires:
| /* epsilon */
    { None }
| REQUIRES expression SEMICOLON
    { Some $2 }
;

assigns:
| /* epsilon */
    { None }
| ASSIGNS argument_expression_list SEMICOLON
    { Some(pos_i 2,$2) }
| ASSIGNS BSNOTHING SEMICOLON
    { Some (pos_i 2,[]) }
;

reads:
| /* epsilon */
    { [] }
| READS argument_expression_list SEMICOLON
    { $2 }
| READS BSNOTHING SEMICOLON
    { [] }
;


function_definition: 
| type_expr identifier parameters function_specification compound_expr
    { locate (JCDfun($1, $2, $3, $4, Some $5)) }
| type_expr identifier parameters function_specification SEMICOLON
    { locate (JCDfun($1, $2, $3, $4, None)) }
;


/******************************/
/* Global variable definition */
/******************************/

variable_definition:
| type_expr IDENTIFIER EQ expression SEMICOLON
    { locate (JCDvar($1,$2,Some $4)) }
| type_expr IDENTIFIER SEMICOLON
    { locate (JCDvar($1,$2,None)) }
;

/********************/
/* global invariant */
/********************/

global_invariant_definition:
| INVARIANT IDENTIFIER COLON expression
    { locate( JCDglobal_inv($2,$4)) }
;


/*************************/
/* exception definitions */
/*************************/

exception_definition:
| EXCEPTION IDENTIFIER OF type_expr
    { locate (JCDexception($2,Some $4)) }
;


/***************/
/* expressions */
/***************/

primary_expression: 
| IDENTIFIER 
    { locate (JCPEvar $1) }
| BSRESULT
    { locate (JCPEvar "\\result") }
| CONSTANT 
    { locate (JCPEconst $1) }
| LPARRPAR 
    { locate (JCPEconst JCCvoid) }
| NULL 
    { locate (JCPEconst JCCnull) }
| STRING_LITERAL 
    { locate (JCPEconst (JCCstring $1)) }
| LPAR expression RPAR 
    { $2 }
| LPAR IDENTIFIER COLON expression RPAR
    { locate (JCPElabel($2,$4)) }
;

postfix_expression: 
| primary_expression 
    { $1 }
| IDENTIFIER label_binders LPAR argument_expression_list_opt RPAR 
    { locate (JCPEapp($1, $2, $4)) }
| IDENTIFIER label_binders LPARRPAR 
    { locate (JCPEapp($1, $2, [])) }
| BSOLD LPAR expression RPAR 
    { locate (JCPEold($3)) }
| BSAT LPAR expression COMMA IDENTIFIER RPAR 
    { locate (JCPEat($3,label $5)) }
| BSOFFSET_MAX LPAR expression RPAR 
    { locate (JCPEoffset(Offset_max,$3)) }
| BSOFFSET_MIN LPAR expression RPAR 
    { locate (JCPEoffset(Offset_min,$3)) }
| BSADDRESS LPAR expression RPAR 
    { locate (JCPEaddress(Addr_pointer,$3)) }
| BSABSOLUTE_ADDRESS LPAR expression RPAR 
    { locate (JCPEaddress(Addr_absolute,$3)) }
| BSBASE_BLOCK LPAR expression RPAR 
    { locate (JCPEbase_block($3)) }
| postfix_expression DOT IDENTIFIER
    { locate (JCPEderef ($1, $3)) }
| postfix_expression PLUSPLUS 
    { locate (JCPEunary (`Upostfix_inc, $1)) }
| postfix_expression MINUSMINUS
    { locate (JCPEunary (`Upostfix_dec, $1)) }
| PLUSPLUS postfix_expression 
    { locate (JCPEunary (`Uprefix_inc, $2)) }
| MINUSMINUS postfix_expression 
    { locate (JCPEunary (`Uprefix_dec, $2)) }
| PLUS postfix_expression %prec UPLUS
    { locate (JCPEunary (`Uplus, $2)) }
| MINUS postfix_expression %prec UMINUS
    { locate (JCPEunary (`Uminus, $2)) }
| TILDE postfix_expression 
    { locate (JCPEunary (`Ubw_not, $2)) }
| EXCLAM postfix_expression 
    { locate (JCPEunary (`Unot, $2)) }
| LSQUARE expression DOTDOT expression RSQUARE
    { locate (JCPErange(Some $2,Some $4)) }
| LSQUARE DOTDOT expression RSQUARE 
    { locate (JCPErange(None,Some $3)) }
| LSQUARE expression DOTDOT RSQUARE 
    { locate (JCPErange(Some $2,None)) }
| LSQUARE DOTDOT RSQUARE 
    { locate (JCPErange(None,None)) }
;

label_binders:
| /* epsilon */ { [] }
| LBRACE IDENTIFIER label_list_end RBRACE { (label $2)::$3 }
;

label_list_end:
| /* epsilon */ { [] }
| COMMA IDENTIFIER label_list_end { (label $2)::$3 }
;

argument_expression_list: 
| expression 
    { [$1] }
| expression COMMA argument_expression_list 
    { $1::$3 }
;

argument_expression_list_opt: 
| /* $\varepsilon$ */
    { [] }
| argument_expression_list 
    { $1 }
;


multiplicative_expression: 
| postfix_expression 
    { $1 }
| multiplicative_expression STAR postfix_expression 
    { locate (JCPEbinary ($1, `Bmul, $3)) }
| multiplicative_expression SLASH postfix_expression 
    { locate (JCPEbinary ($1, `Bdiv, $3)) }
| multiplicative_expression PERCENT postfix_expression 
    { locate (JCPEbinary ($1, `Bmod, $3)) }
;

additive_expression: 
| multiplicative_expression 
    { $1 }
| additive_expression PLUS multiplicative_expression 
    { locate (JCPEbinary ($1, `Badd, $3)) }
| additive_expression MINUS multiplicative_expression 
    { locate (JCPEbinary ($1, `Bsub, $3)) }
| additive_expression AT multiplicative_expression 
    { locate (JCPEbinary ($1, `Bconcat, $3)) }
;

shift_expression: 
| additive_expression 
    { $1 }
| shift_expression LSHIFT additive_expression 
    { locate (JCPEbinary ($1, `Bshift_left, $3)) }
| shift_expression LRSHIFT additive_expression 
    { locate (JCPEbinary ($1, `Blogical_shift_right, $3)) }
| shift_expression ARSHIFT additive_expression 
    { locate (JCPEbinary ($1, `Barith_shift_right, $3)) }
;

assignment_operator: 
| EQ { `Aeq }
| PLUSEQ { `Aadd }
| MINUSEQ { `Asub }
| STAREQ { `Amul }
| SLASHEQ { `Adiv }
| PERCENTEQ { `Amod }
/*
| LEFT_ASSIGN { Aleft }
| RIGHT_ASSIGN { Aright }
*/
| AMPEQ { `Aand }
| CARETEQ { `Axor }
| BAREQ { `Aor }
;


expression: 
| compound_expr
    { $1 }
| ASSERT FOR identifier_list COLON expression %prec FOR
    { locate (JCPEassert($3,Aassert,$5)) }
| ASSERT expression 
    { locate (JCPEassert([],Aassert,$2)) }
| HINT FOR identifier_list COLON expression %prec FOR
    { locate (JCPEassert($3,Ahint,$5)) }
| HINT expression 
    { locate (JCPEassert([],Ahint,$2)) }
| ASSUME FOR identifier_list COLON expression %prec FOR
    { locate (JCPEassert($3,Aassume,$5)) }
| ASSUME expression 
    { locate (JCPEassert([],Aassume,$2)) }
| behavior compound_expr
    { locate (JCPEcontract(None,None,[$1],$2)) } 
| iteration_expression 
    { $1 }
| jump_expression 
    { $1 }
| declaration
    { $1 }
/*
| SPEC expression { locate (CSspec ($1,$2)) }
*/
| pack_expression { $1 }
| exception_expression { $1 }
| shift_expression 
    { $1 }
| SWITCH LPAR expression RPAR LBRACE switch_block RBRACE
    { locate (JCPEswitch ($3, $6)) }

| NEW IDENTIFIER LSQUARE expression RSQUARE
    { locate (JCPEalloc ($4, $2)) }
| FREE LPAR expression RPAR
    { locate (JCPEfree $3) }
| expression LT expression 
    { locate (JCPEbinary ($1, `Blt, $3)) }
| expression GT expression
    { locate (JCPEbinary ($1, `Bgt, $3)) }
| expression LTEQ expression
    { locate (JCPEbinary ($1, `Ble, $3)) }
| expression GTEQ expression
    { locate (JCPEbinary ($1, `Bge, $3)) }
| expression LTCOLON IDENTIFIER
    { locate (JCPEinstanceof($1, $3)) }
/*
| expression COLONGT IDENTIFIER
    { locate (JCPEcast($1, $3)) }
| expression COLONGT REAL
    { locate (JCPEcast($1, "real")) }
| expression COLONGT INTEGER
    { locate (JCPEcast($1, "integer")) }
*/
| expression COLONGT type_expr
    { locate (JCPEcast($1, $3)) }
| expression EQEQ expression 
    { locate (JCPEbinary ($1, `Beq, $3)) }
| expression BANGEQ expression 
    { locate (JCPEbinary ($1, `Bneq, $3)) }
| expression AMP expression 
    { locate (JCPEbinary ($1, `Bbw_and, $3)) }
| expression HAT expression 
    { locate (JCPEbinary ($1, `Bbw_xor, $3)) }
| expression PIPE expression 
    { locate (JCPEbinary ($1, `Bbw_or, $3)) }
| expression AMPAMP expression 
    { locate (JCPEbinary($1, `Bland, $3)) }
| expression BARBAR expression 
    { locate (JCPEbinary($1, `Blor, $3)) }
| IF expression THEN expression ELSE expression
    { locate (JCPEif ($2, $4, $6)) }
| IF expression THEN expression
    { locate (JCPEif ($2, $4, skip)) }
| LET IDENTIFIER EQ expression IN expression %prec PRECFORALL
    { locate (JCPElet (None, $2, Some $4, $6)) }
| LET type_expr IDENTIFIER EQ expression IN expression %prec PRECFORALL
    { locate (JCPElet (Some $2, $3, Some $5, $7)) }
| postfix_expression assignment_operator expression %prec ASSIGNOP
    { let a  =
	match $2 with
		| `Aeq -> JCPEassign ($1, $3)
		| `Aadd -> JCPEassign_op ($1, `Badd, $3)
		| `Asub -> JCPEassign_op ($1, `Bsub, $3)
		| `Amul -> JCPEassign_op ($1, `Bmul, $3)
		| `Adiv -> JCPEassign_op ($1, `Bdiv, $3)
		| `Amod -> JCPEassign_op ($1, `Bmod, $3)
		| `Aand -> JCPEassign_op ($1, `Bbw_and, $3)
		| `Axor -> JCPEassign_op ($1, `Bbw_xor, $3)
		| `Aor -> JCPEassign_op ($1, `Bbw_or, $3)
(*
		| Aleft -> CEassign_op ($1, `Bshift_left, $3)
		| Aright -> CEassign_op ($1, `Bshift_right, $3)
*)
      in locate a }

| BSFORALL type_expr identifier_list SEMICOLON expression 
    %prec PRECFORALL
    { locate (JCPEquantifier(Forall,$2,$3,$5)) }
| BSEXISTS type_expr identifier_list SEMICOLON expression 
    %prec PRECFORALL
    { locate (JCPEquantifier(Exists,$2,$3,$5)) }
| expression EQEQGT expression
    { locate (JCPEbinary($1,`Bimplies,$3)) }
| expression LTEQEQGT expression
    { locate (JCPEbinary($1,`Biff,$3)) }
/*
| expression COMMA assignment_expression { locate (CEseq ($1, $3)) }
*/
| BSMUTABLE LPAR expression COMMA tag RPAR
    { locate (JCPEmutable($3, $5)) }
| BSMUTABLE LPAR expression RPAR
    { locate (JCPEmutable($3, locate JCPTbottom)) }
| BSTYPEEQ LPAR tag COMMA tag RPAR
    { locate (JCPEeqtype($3, $5)) }
| MATCH expression WITH pattern_expression_list END
    { locate (JCPEmatch($2, $4)) }
;

tag:
| identifier
    { locate (JCPTtag $1) }
| BSBOTTOM
    { locate JCPTbottom }
| BSTYPEOF LPAR expression RPAR
    { locate (JCPTtypeof $3) }
;

identifier_list: 
| identifier 
    { [$1#name] }
| identifier COMMA identifier_list 
    { $1#name :: $3 }
;

identifier:
| DEFAULT
    { locate_identifier "default" }
| IDENTIFIER
    { locate_identifier $1 }
;

/****************/
/* declarations */
/****************/


declaration: 
| VAR type_expr IDENTIFIER
    { locate (JCPEdecl($2, $3, None)) }
| VAR type_expr IDENTIFIER EQ expression
    { locate (JCPEdecl($2, $3, Some $5)) }
;


/**************/
/* expressions */
/**************/

/*
case_expression:
| CASE CONSTANT COLON expression_list 
    { Case $2, $4 }
;

default_expression:
| DEFAULT COLON expression_list
    { Default, $3 }
;

case_expression_list: 
|  
    { [] }
| case_expression case_expression_list 
    { $1 :: $2 }
;

default_case_expression_list:
| case_expression_list default_expression
    { $1 @ [$2] }
| case_expression_list
    { $1 }
;
*/

compound_expr:
| LBRACE expression_list RBRACE
    { locate (JCPEblock $2) }
;

expression_list: 
| expression SEMICOLON
    { [$1] }
| expression
    { [$1] }
| expression SEMICOLON expression_list 
    { $1 :: $3 }
;

switch_block: 
| /* $\varepsilon$ */
    { [] }
| switch_labels 
    { [($1, locate (JCPEblock []))] }
| switch_labels expression_list switch_block
    { ($1, locate (JCPEblock $2))::$3 }
;

switch_labels:
| switch_label
    { [$1] }
| switch_label switch_labels
    { $1::$2 }
;

switch_label:
| CASE expression COLON
    { Some($2) }
| DEFAULT COLON
    { None }
;

iteration_expression: 
| loop_annot WHILE LPAR expression RPAR expression %prec precwhile
    { let (i,v) = $1 in 
      locate (JCPEwhile ($4, i, v, $6)) }
| loop_annot DO expression WHILE LPAR expression RPAR
    { assert false (* TODO locate (JCPEdowhile ($1, $3, $6)) *) }
| loop_annot FOR LPAR argument_expression_list_opt SEMICOLON expression SEMICOLON 
    argument_expression_list_opt RPAR expression %prec precwhile
    { let (i,v) = $1 in 
      locate (JCPEfor($4, $6, $8, i, v, $10)) }
;

loop_invariant:
| INVARIANT FOR identifier_list COLON expression SEMICOLON %prec FOR
    { ($3, $5) }
| INVARIANT expression SEMICOLON
    { ([], $2) }
;

loop_invariant_list:
| loop_invariant loop_invariant_list
    { $1 :: $2 }
| loop_invariant
    { [$1] }
;

loop_annot:
| loop_invariant_list VARIANT expression SEMICOLON
    { ($1, Some $3) }
| loop_invariant_list
    { ($1, None) }
| VARIANT expression SEMICOLON
    { ([], Some $2) }
| %prec PRECLOOPANNOT
    { ([], None) }
;

jump_expression: 
| GOTO identifier
    { locate (JCPEgoto $2#name) }
/*
| CONTINUE SEMICOLON { locate CScontinue }
*/
| BREAK
    { locate (JCPEbreak "") }
| RETURN expression
    { locate (JCPEreturn $2) }
;

pack_expression:
| PACK LPAR expression COMMA identifier RPAR
    { locate (JCPEpack ($3, Some $5)) }
| PACK LPAR expression RPAR
    { locate (JCPEpack ($3, None)) }
| UNPACK LPAR expression COMMA identifier RPAR
    { locate (JCPEunpack ($3, Some $5)) }
| UNPACK LPAR expression RPAR
    { locate (JCPEunpack ($3, None)) }
;

catch_expression: 
| CATCH identifier IDENTIFIER expression
    { ($2, $3, $4) }
;

catch_expression_list:
| catch_expression
    { [$1] }
| catch_expression catch_expression_list 
    { $1 :: $2 }
;

exception_expression:
| THROW identifier expression
   { locate (JCPEthrow($2,$3)) }
| TRY expression catch_expression_list END
   { locate (JCPEtry($2, $3, skip)) }
| TRY expression catch_expression_list FINALLY expression END
   { locate (JCPEtry($2, $3, $5)) }
;

/**********************************/
/* Logic functions and predicates */
/**********************************/

logic_definition:
/* constants def */
| LOGIC type_expr IDENTIFIER EQ expression
    { locate (JCDlogic(Some $2, $3, [], [], JCexpr $5)) }
/* constants no def */
| LOGIC type_expr IDENTIFIER 
    { locate (JCDlogic(Some $2, $3, [], [], JCreads [])) }
/* logic fun def */
| LOGIC type_expr IDENTIFIER label_binders parameters EQ expression
    { locate (JCDlogic(Some $2, $3, $4, $5, JCexpr $7)) }
/* logic pred def */
| LOGIC IDENTIFIER label_binders parameters EQ expression
    { locate (JCDlogic(None, $2, $3, $4, JCexpr $6)) }
/* logic fun reads */
/*
| LOGIC type_expr IDENTIFIER label_binders parameters reads %prec PRECLOGIC
    { locate (JCDlogic(Some $2, $3, $4, $5, JCreads $6)) }
*/
/* logic pred reads */
/*
| LOGIC IDENTIFIER label_binders parameters reads %prec PRECLOGIC
    { locate (JCDlogic(None, $2, $3, $4, JCreads $5)) }
*/
/* logic fun axiomatic def */
/*
| LOGIC type_expr IDENTIFIER label_binders parameters LBRACE axioms RBRACE
    { locate (JCDlogic(Some $2, $3, $4, $5, JCaxiomatic $7)) }
*/
/* logic pred axiomatic def */
/*
| LOGIC IDENTIFIER label_binders parameters LBRACE axioms RBRACE
    { locate (JCDlogic(None, $2, $3, $4, JCaxiomatic $6)) }
*/
/* logic pred inductive def */
| LOGIC IDENTIFIER label_binders parameters LBRACE indcases RBRACE
    { locate (JCDlogic(None, $2, $3, $4, JCinductive $6)) }
| AXIOMATIC IDENTIFIER LBRACE logic_declarations RBRACE
    { locate (JCDaxiomatic($2,$4)) } 
| LEMMA IDENTIFIER label_binders COLON expression
    { locate( JCDlemma($2,false,$3,$5)) }
;



logic_declarations:
| logic_declaration
    { [$1] }
| logic_declaration logic_declarations
    { $1::$2 }
;

logic_declaration:
| logic_definition
    { $1 }
| LOGIC TYPE IDENTIFIER
    { locate (JCDlogic_type($3)) }
/* remove this comment if removed from logic_definition
| LOGIC type_expr IDENTIFIER 
    { locate (JCDlogic(Some $2, $3, [], [], JCreads [])) }
*/
| LOGIC IDENTIFIER label_binders parameters 
    { locate (JCDlogic(None, $2, $3, $4, JCreads [])) }
| LOGIC type_expr IDENTIFIER label_binders parameters 
    { locate (JCDlogic(Some $2, $3, $4, $5, JCreads [])) }
| AXIOM identifier label_binders COLON expression
    { locate( JCDlemma($2#name,true,$3,$5)) }
;

indcases:
| /* epsilon */
    { [] }
| CASE identifier label_binders COLON expression SEMICOLON indcases
    { ($2,$3,$5)::$7 }
;

/*
logic_rec_definitions:
| logic_definition AND logic_rec_definitions %prec PRECTYPE
    { $1::$3 }
| logic_definition AND logic_definition %prec PRECTYPE
    { $1::[$3] }
*/

/************/
/* patterns */
/************/

pattern:
| identifier LBRACE field_patterns RBRACE
    { locate (JCPPstruct($1, $3)) }
| identifier
    { locate (JCPPvar $1) }
| LPAR pattern RPAR
    { $2 }
| pattern PIPE pattern
    { locate (JCPPor($1, $3)) }
| pattern AS identifier
    { locate (JCPPas($1, $3)) }
| UNDERSCORE
    { locate JCPPany }
| CONSTANT 
    { locate (JCPPconst $1) }
| LPARRPAR 
    { locate (JCPPconst JCCvoid) }
| NULL 
    { locate (JCPPconst JCCnull) }
;

field_patterns:
| identifier EQ pattern SEMICOLON field_patterns
    { ($1, $3)::$5 }
|
    { [] }
;

pattern_expression_list:
| pattern MINUSGT expression SEMICOLON pattern_expression_list
    { ($1, $3)::$5 }
| pattern MINUSGT expression SEMICOLON
    { [$1, $3] }
;

pattern_expression_list:
| pattern MINUSGT compound_expr pattern_expression_list
    { ($1, $3)::$4 }
| pattern MINUSGT compound_expr
    { [$1, $3] }
;

/*
Local Variables: 
compile-command: "LC_ALL=C make -C .. bin/jessie.byte"
End: 
*/
