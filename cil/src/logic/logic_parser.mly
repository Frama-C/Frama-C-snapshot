/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2009                                               */
/*    CEA   (Commissariat à l'Énergie Atomique)                           */
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
/*  See the GNU Lesser General Public License version v2.1                */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

/* Grammar for C annotations */

%{

  open Cil
  open Cil_types
  open Logic_ptree
  open Logic_const
  open Logic_utils

  let loc () = (symbol_start_pos (), symbol_end_pos ())
  let loc_i i = (rhs_start i, rhs_end i)
  let info x = { lexpr_node = x; lexpr_loc = loc () }
  let loc_info loc x = { lexpr_node = x; lexpr_loc = loc }
  let loc_start x = fst x.lexpr_loc
  let loc_end x = snd x.lexpr_loc

  type sense_of_relation = Unknown | Equal | Disequal | Less | Greater

  let relation_sense rel sense =
    match rel, sense with
        Eq, _ -> sense, true
      | Neq, Unknown -> Disequal, true (* No chain of disequality for now*)
      | (Gt|Ge), (Unknown|Equal|Greater) -> Greater, true
      | (Lt|Le), (Unknown|Equal|Less) -> Less, true
      | _ -> sense, false

  let type_variables_stack = Stack.create ()

  let enter_type_variables_scope l =
    List.iter Logic_env.add_typename l;
    Stack.push l type_variables_stack

  let exit_type_variables_scope () =
    let l = Stack.pop type_variables_stack in
    List.iter Logic_env.remove_typename l

  let rt_type = ref false

  let set_rt_type () = rt_type:= true

  let reset_rt_type () = rt_type:=false

  let is_rt_type () = !rt_type

%}

%token <string> IDENTIFIER TYPENAME
%token <bool*string> STRING_LITERAL
%token <Logic_ptree.constant> CONSTANT
%token LPAR RPAR IF ELSE COLON COLON2 COLONCOLON DOT DOTDOT DOTDOTDOT
%token INT INTEGER REAL FLOAT LT GT LE GE EQ NE COMMA ARROW EQUAL
%token FORALL EXISTS IFF IMPLIES AND OR NOT SEPARATED
%token TRUE FALSE OLD AT RESULT BLOCK_LENGTH BASE_ADDR
%token VALID VALID_INDEX VALID_RANGE FRESH DOLLAR
%token QUESTION MINUS PLUS STAR AMP SLASH PERCENT LSQUARE RSQUARE EOF
%token GLOBAL INVARIANT VARIANT DECREASES FOR LABEL ASSERT SEMICOLON NULL EMPTY
%token REQUIRES ENSURES ASSIGNS LOOP NOTHING SLICE IMPACT PRAGMA FROM
%token READS LOGIC PREDICATE INDUCTIVE AXIOMATIC AXIOM LEMMA LBRACE RBRACE GHOST CASE
%token VOID CHAR SIGNED UNSIGNED SHORT LONG DOUBLE STRUCT ENUM UNION
%token BSUNION INTER
%token LTCOLON COLONGT TYPE BEHAVIOR BEHAVIORS ASSUMES COMPLETE DISJOINT
%token TERMINATES
%token HAT HATHAT PIPE TILDE GTGT LTLT
%token SIZEOF LAMBDA
%token TYPEOF BSTYPE

%right prec_named
%nonassoc IDENTIFIER TYPENAME SEPARATED
%nonassoc prec_forall prec_exists prec_lambda
%right QUESTION prec_question
%right IMPLIES IFF
%left OR
%left HATHAT
%left AND
%left PIPE
%left HAT
%nonassoc prec_no_rel
%left prec_rel_list /* for list of relations (LT GT LE GE EQ NE) */
%left LT
%left LTLT GTGT
%left PLUS MINUS
%left STAR SLASH PERCENT AMP
%right prec_uminus
%right prec_cast
%nonassoc TILDE NOT
%nonassoc LTCOLON COLONGT
%left DOT ARROW LSQUARE
%right prec_par

%type <Logic_ptree.lexpr> lexpr
%start lexpr

%type <Logic_ptree.annot> annot
%start annot

%type <Logic_ptree.spec * Cabs.cabsloc> spec
%start spec

%%

enter_kw_c_mode:
/* empty */ { enter_kw_c_mode () }

exit_kw_c_mode:
/* empty */ { exit_kw_c_mode () }

enter_rt_type:
/* empty */ { if is_rt_type () then enter_rt_type_mode () }

exit_rt_type:
/* empty */ { if is_rt_type () then exit_rt_type_mode () }

begin_rt_type:
/* empty */ { set_rt_type () }

end_rt_type:
/* empty */ { reset_rt_type () }

/*** predicates and terms ***/

lexpr_list:
| /* epsilon */ { [] }
| ne_lexpr_list  { $1 }
;

ne_lexpr_list:
| lexpr                    { [$1] }
| lexpr COMMA ne_lexpr_list { $1 :: $3 }
;

lexpr_rel:
| lexpr_inner %prec prec_no_rel { $1 }
| lexpr_inner rel_list %prec prec_rel_list
      { let rel, rhs, _, oth_rel = $2 in
        let loc = loc_start $1, loc_end rhs in
        let relation = loc_info loc (PLrel($1,rel,rhs)) in
        match oth_rel with
            None -> relation
          | Some oth_relation -> info (PLand(relation,oth_relation))
      }

lexpr_inner:
  | string {
      let (is_wide,content) = $1 in
      let cst = if is_wide then
        WStringConstant content
      else
        StringConstant content
      in
      info (PLconstant cst)
    }
  | NOT lexpr_inner { info (PLnot $2) }
  | TRUE { info PLtrue }
  | FALSE { info PLfalse }
  | VALID LPAR lexpr RPAR { info (PLvalid ($3)) }
  | VALID_INDEX LPAR lexpr COMMA lexpr RPAR { info (PLvalid_index ($3,$5)) }
  | VALID_RANGE LPAR lexpr COMMA lexpr COMMA lexpr RPAR
      { info (PLvalid_range ($3,$5,$7)) }
  | FRESH LPAR lexpr RPAR { info (PLfresh ($3)) }
  | NULL { info PLnull }
  | CONSTANT { info (PLconstant $1) }
  | lexpr_inner PLUS lexpr_inner { info (PLbinop ($1, Badd, $3)) }
  | lexpr_inner MINUS lexpr_inner { info (PLbinop ($1, Bsub, $3)) }
  | lexpr_inner STAR lexpr_inner { info (PLbinop ($1, Bmul, $3)) }
  | lexpr_inner SLASH lexpr_inner { info (PLbinop ($1, Bdiv, $3)) }
  | lexpr_inner PERCENT lexpr_inner { info (PLbinop ($1, Bmod, $3)) }
  | lexpr_inner ARROW identifier { info (PLarrow ($1, $3)) }
  | lexpr_inner DOT identifier { info (PLdot ($1, $3)) }
  | lexpr_inner LSQUARE range RSQUARE { info (PLarrget ($1, $3)) }
  | lexpr_inner LSQUARE lexpr RSQUARE { info (PLarrget ($1, $3)) }
  | MINUS lexpr_inner %prec prec_uminus { info (PLunop (Uminus, $2)) }
  | PLUS lexpr_inner %prec prec_uminus { $2 }
  | TILDE lexpr_inner { info (PLunop (Ubw_not, $2)) }
  | STAR lexpr_inner { info (PLunop (Ustar, $2)) }
  | AMP lexpr_inner { info (PLunop (Uamp, $2)) }
  | SIZEOF LPAR lexpr RPAR { info (PLsizeofE $3) }
  | SIZEOF LPAR logic_type RPAR { info (PLsizeof $3) }
  | OLD LPAR lexpr RPAR { info (PLold $3) }
  | AT LPAR lexpr COMMA label RPAR { info (PLat ($3, $5)) }
  | BASE_ADDR LPAR lexpr RPAR { info (PLbase_addr $3) }
  | BLOCK_LENGTH LPAR lexpr RPAR { info (PLblock_length $3) }
  | RESULT { info PLresult }
  | identifier LPAR ne_lexpr_list RPAR
      { info (PLapp ($1, [], $3)) }
  | SEPARATED LPAR ne_lexpr_list RPAR
      { info (PLseparated $3) }
  | identifier LBRACE ne_tvar_list RBRACE LPAR ne_lexpr_list RPAR
      { info (PLapp ($1, $3, $6)) }
  | identifier LBRACE ne_tvar_list RBRACE
      { info (PLapp ($1, $3, [])) }
  | identifier %prec IDENTIFIER { info (PLvar $1) }
  | lexpr_inner GTGT lexpr_inner { info (PLbinop ($1, Brshift, $3))}
  | lexpr_inner LTLT lexpr_inner { info (PLbinop ($1, Blshift, $3))}
  | LPAR lexpr RPAR %prec prec_par { info $2.lexpr_node }
  | LPAR range RPAR { info $2.lexpr_node }
  | LPAR logic_type RPAR lexpr_inner %prec prec_cast
      { info (PLcast ($2, $4)) }
  | lexpr_inner LTCOLON lexpr_inner %prec prec_cast
      { info (PLsubtype ($1, $3)) }
  | lexpr_inner COLONGT logic_type %prec prec_cast
      { info (PLcoercion ($1, $3)) }
  | lexpr_inner COLONGT lexpr_inner %prec prec_cast
      { info (PLcoercionE ($1, $3)) }
  | TYPEOF LPAR lexpr RPAR { info (PLtypeof $3) }
  | BSTYPE LPAR type_spec STAR RPAR { info (PLtype $3) }
    /* tsets */
  | EMPTY { info PLempty }
  | BSUNION LPAR lexpr_list RPAR { info (PLunion $3) }
  | INTER LPAR lexpr_list RPAR { info (PLinter $3) }
  | LBRACE lexpr PIPE binders RBRACE
      {info (PLcomprehension ($2,$4,None)) }
  | LBRACE lexpr PIPE binders SEMICOLON lexpr RBRACE
      { info (PLcomprehension ($2,$4,Some $6)) }
    /* Functional update */
  | LBRACE lexpr FOR identifier EQUAL lexpr RBRACE { info (PLupdate($2,$4,$6)) }
;

string:
    STRING_LITERAL { $1 }
  | string STRING_LITERAL {
      let (is_wide,prefix) = $1 in
      let (is_wide2,suffix) = $2 in
      (is_wide || is_wide2, prefix ^ suffix)
    }

relation:
  | LT    { Lt }
  | GT    { Gt }
  | LE    { Le }
  | GE    { Ge }
  | EQ    { Eq }
  | NE    { Neq }
;

range:
| lexpr_option DOTDOT lexpr_option { info (PLrange($1,$3)) }

lexpr:
  /* predicates */
  lexpr IMPLIES lexpr { info (PLimplies ($1, $3)) }
| lexpr IFF lexpr { info (PLiff ($1, $3)) }
| lexpr OR lexpr     { info (PLor ($1, $3)) }
| lexpr AND lexpr    { info (PLand ($1, $3)) }
| lexpr HATHAT lexpr    { info (PLxor ($1, $3)) }
| FORALL binders SEMICOLON lexpr  %prec prec_forall
      { info (PLforall ($2, $4)) }
| EXISTS binders SEMICOLON lexpr  %prec prec_exists
      { info (PLexists ($2, $4)) }
| LAMBDA binders SEMICOLON lexpr  %prec prec_lambda
      { info (PLlambda ($2,$4)) }
/* terms */
| lexpr AMP lexpr { info (PLbinop ($1, Bbw_and, $3)) }
| lexpr PIPE lexpr { info (PLbinop ($1, Bbw_or, $3)) }
| lexpr HAT lexpr { info (PLbinop ($1, Bbw_xor, $3)) }
| lexpr QUESTION lexpr COLON2 lexpr %prec prec_question
    { info (PLif ($1, $3, $5)) }
/* both terms and predicates */
| identifier COLON lexpr %prec prec_named { info (PLnamed ($1, $3)) }
| lexpr_rel %prec prec_rel_list { $1 }
;

rel_list:
  relation lexpr_inner %prec prec_rel_list
  { $1, $2, fst(relation_sense $1 Unknown), None }
| relation lexpr_inner rel_list %prec prec_rel_list
  {
    let next_rel, rhs, sense, oth_rel = $3 in
    let (sense, correct) = relation_sense $1 sense
    in
    if correct then
      let loc = loc_start $2, loc_end rhs in
      let my_rel = loc_info loc (PLrel($2,next_rel,rhs)) in
      let oth_rel = match oth_rel with
          None -> my_rel
        | Some rel ->
	    let loc = loc_start $2, loc_end rel in
	    loc_info loc (PLand(my_rel,rel))
      in
      $1,$2,sense,Some oth_rel
    else begin
      let loc = Parsing.rhs_start_pos 1, Parsing.rhs_end_pos 3 in
      raise (Not_well_formed(loc,"Inconsistent relation chain."));
    end
  }
;

lexpr_option:
| /* epsilon */ { None }
| lexpr         { Some $1 }
;

/*** binders ***/

binders: binders_reentrance { let (_lt, vars) = $1 in vars };

binders_reentrance:
| decl_spec { let (lt, var) = $1 in (lt, [var]) }
| binders_reentrance COMMA decl_spec
    { let _, vars = $1 in
      let (lt, var) = $3 in
        (lt, vars @ [ var ])
    }
| binders_reentrance COMMA var_spec
    { let last_type_spec, vars = $1 in
        (last_type_spec, vars @ [ let (modif, name) = $3 in (modif last_type_spec, name)])
    }

decl_spec:
| type_spec var_spec { ($1, let (modif, name) = $2 in (modif $1, name))  }
;

var_spec:
|       var_spec_bis { $1 }
| stars var_spec_bis
  { let (modif, name) = $2 in
      ((fun x -> $1 (modif x)), name) }
;

var_spec_bis:
| identifier     { ((fun x -> x), $1) }
| var_spec_bis LSQUARE lexpr_option RSQUARE
      { (* TODO: use size information for LTarray - $3 *)
        let (modif, name) = $1 in
          ((fun x -> LTarray (modif x)), name)
      }
| LPAR var_spec RPAR { $2 }
| var_spec_bis LPAR abs_param_type_list RPAR { (* TODO *) raise Parse_error }
;

abs_param_type_list:
| /* empty */ { [ ] }
| abs_param_list { $1 }
| abs_param_list COMMA DOTDOTDOT { (* TODO *) raise Parse_error }
;

abs_param_list:
| abs_param { [ $1 ] }
| abs_param_list COMMA abs_param { $1 @ [ $3 ] }
;

/* TODO: abs_param should be less restrictive than parameter
since its name can be omitted
*/
abs_param:
| parameter { $1 }
;

/*** restricted type expressions ***/

id_as_typename:
  identifier { LTnamed($1, []) }
;

ne_parameters:
| parameter { [$1] }
| parameter COMMA ne_parameters { $1 :: $3 }
;

parameter:
| type_spec var_spec { let (modif, name) = $2 in (modif $1, name)}
| id_as_typename var_spec { let (modif, name) = $2 in (modif $1, name) }
;


/*** type expressions ***/

logic_type:
| type_spec abs_spec_option { $2 $1 }
;

logic_rt_type:
  IDENTIFIER { LTnamed($1,[]) }
| begin_rt_type logic_type end_rt_type { $2 }

abs_spec_option:
| /* empty */ %prec TYPENAME  { fun t -> t }
| abs_spec { $1 }
;

abs_spec:
|                    tabs { $1 }
| stars                   %prec TYPENAME { $1 }
| stars              tabs                { fun t -> $2 ($1 t) }
| stars abs_spec_bis      %prec TYPENAME { fun t -> $2 ($1 t) }
| stars abs_spec_bis tabs                { fun t -> $2 ($3 ($1 t)) }
|       abs_spec_bis tabs                { fun t -> $1 ($2 t) }
;

abs_spec_bis:
| LPAR abs_spec RPAR { $2 }
| abs_spec_bis LPAR abs_param_type_list RPAR { (* TODO *) raise Parse_error };
;

stars:
| STAR       { fun t -> LTpointer t }
| stars STAR { fun t -> $1 (LTpointer t) }
;

tabs:
| LSQUARE lexpr_option RSQUARE %prec TYPENAME
    {  (* TODO: use size information for LTarray - $2 *)
      fun t -> LTarray t
    }
| LSQUARE lexpr_option RSQUARE tabs
    {  (* TODO: use size information for LTarray - $2 *)
      fun t -> LTarray ($4 t)
    }
;

type_spec:
| INTEGER        { LTinteger }
| REAL           { LTreal }
| VOID           { LTvoid }
| CHAR           { LTint IChar }       /** [char] */
| SIGNED CHAR    { LTint ISChar }      /** [signed char] */
| UNSIGNED CHAR  { LTint IUChar }      /** [unsigned char] */
| INT            { LTint IInt }        /** [int] */
| SIGNED INT     { LTint IInt }        /** [int] */
| UNSIGNED INT   { LTint IUInt }       /** [unsigned int] */
| UNSIGNED       { LTint IUInt }
| SHORT          { LTint IShort }      /** [short] */
| SIGNED SHORT   { LTint IShort }      /** [short] */
| UNSIGNED SHORT { LTint IUShort }     /** [unsigned short] */
| LONG           { LTint ILong }       /** [long] */
| SIGNED LONG    { LTint ILong }       /** [long] */
| UNSIGNED LONG  { LTint IULong }      /** [unsigned long] */
| SIGNED LONG INT{ LTint ILong }       /** [long] */
| LONG  INT      { LTint ILong }       /** [long] */
| UNSIGNED LONG INT { LTint IULong }      /** [unsigned long] */
| LONG LONG      { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| SIGNED LONG LONG   { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| UNSIGNED LONG LONG { LTint IULongLong }  /** [unsigned long long]
                                (or [unsigned _int64] on Microsoft Visual C) */
| LONG LONG INT     { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| SIGNED LONG LONG INT  { LTint ILongLong }   /** [long long] (or [_int64] on
					   Microsoft Visual C) */
| UNSIGNED LONG LONG INT { LTint IULongLong }  /** [unsigned long long]
                                (or [unsigned _int64] on Microsoft Visual C) */
| FLOAT             { LTfloat FFloat }
| DOUBLE            { LTfloat FDouble }
| LONG DOUBLE       { LTfloat FLongDouble }
| STRUCT exit_rt_type identifier { LTstruct $3 }
| ENUM   exit_rt_type identifier { LTenum $3 }
| UNION  exit_rt_type identifier  { LTunion $3 }
| TYPENAME          { LTnamed ($1,[]) }
| TYPENAME LT enter_rt_type  ne_logic_type_list GT exit_rt_type
      { LTnamed($1,$4) }
;

ne_logic_type_list:
| logic_type                          { [$1] }
| logic_type COMMA enter_rt_type ne_logic_type_list { $1 :: $4 }
;

/*** annotations ***/

full_lexpr:
enter_kw_c_mode lexpr exit_kw_c_mode { $2 }
;

full_identifier:
enter_kw_c_mode identifier exit_kw_c_mode { $2 }
;

full_parameters:
enter_kw_c_mode ne_parameters exit_kw_c_mode { $2 }
;

full_parameter:
enter_kw_c_mode parameter exit_kw_c_mode { $2 }
;

/*
full_tsets:
enter_kw_c_mode tsets exit_kw_c_mode  { $2 }
;
*/

full_ne_lexpr_list:
enter_kw_c_mode ne_lexpr_list exit_kw_c_mode { $2 }
;

full_logic_type:
enter_kw_c_mode logic_type exit_kw_c_mode { $2 }
;

full_logic_rt_type:
  enter_kw_c_mode logic_rt_type exit_kw_c_mode { $2 }

full_assigns:
enter_kw_c_mode assigns exit_kw_c_mode { $2 }
;

assigns:
  location_dep { $1 }
| NOTHING { [Nothing,[]] }
;

requires:
  /* epsilon */ { [] }
| REQUIRES full_lexpr SEMICOLON requires { $2::$4 }
;


behavior_body:
  /* epsilon */ { [],[],[] }
| ne_behavior_body { $1 }

ne_behavior_body:
| ASSUMES full_lexpr SEMICOLON behavior_body
    { let a,b,c = $4 in $2::a,b,c }
| ENSURES full_lexpr SEMICOLON behavior_body
    { let a,b,c = $4 in a,$2::b,c }
| ASSIGNS full_assigns SEMICOLON behavior_body
    { let a,b,c = $4 in
      let assigns =
        match c,$2 with
            ([] | [(Nothing,_) ]), ([] | [(Nothing,_) ]) -> [(Nothing,[])]
          | [(Nothing,_) ], _ | _, [(Nothing,_)] ->
              raise (
                Not_well_formed (loc(),"Mixing \\nothing and a real location"))
          | _,_ -> $2 @ c
      in a,b,assigns
    }
/* Grammar Extensibility for plugins
| identifier ENSURES string SEMICOLON behavior_body
	{ 
	  let custom_parser, typing_function = H.find $1 in
	   {custom_name=$1;
	    custom_type = typing_function;
	    custom_state = ENSURES;
	    custom_ast = custom_parser $3
	    ::$5
	}
*/
;

behaviors:
  /* epsilon */ { [] }
| BEHAVIOR full_identifier COLON behavior_body behaviors
      { let (assumes,ensures,assigns) = $4 in
        Logic_utils.check_assigns ~loc:(loc()) assigns;
        {b_name=$2; b_assumes = assumes;
         b_ensures = ensures;
         b_assigns = assigns}::$5 }
;

simple_behavior_body:
  behavior_body terminates behavior_body
      { let (assumes1, ensures1, assigns1) = $1 in
        let (assumes2, ensures2,assigns2) = $3 in
        (Some $2, (assumes1 @ assumes2,
                      ensures1 @ ensures2, assigns1 @ assigns2))
      }
| behavior_body { (None, $1) }
;

behaviors_or_default:
| simple_behavior_body behaviors
    { let (terminates,(assumes,ensures,assigns)) = $1 in
      let behaviors =
        if (*TODO: enforce to have a default behavior true || *)
           assumes <> [] || ensures <> [] || assigns <> [] then
          { b_name = "default";b_assumes = assumes;
            b_ensures = ensures;
            b_assigns = assigns} :: $2
        else $2
      in
      terminates, behaviors
    }
;

terminates: TERMINATES lexpr SEMICOLON { $2 }
;

ne_full_identifier_list:
  full_identifier { [$1] }
| full_identifier COMMA ne_full_identifier_list {$1 :: $3}
;

complete_or_disjoint:
  /* epsilon */ { [],[] }
| COMPLETE BEHAVIORS ne_full_identifier_list SEMICOLON complete_or_disjoint
      { let (complete,disjoint) = $5 in ($3::complete, disjoint) }
| DISJOINT BEHAVIORS ne_full_identifier_list SEMICOLON complete_or_disjoint
      { let (complete,disjoint) = $5 in (complete, $3::disjoint) }

spec:
  requires behaviors_or_default complete_or_disjoint decreases
      {
        { spec_requires = $1;
          spec_behavior = snd $2;
          spec_variant = $4;
          spec_terminates = fst $2;
          spec_complete_behaviors = fst $3;
          spec_disjoint_behaviors = snd $3;
        },loc() }
;

/* Spec are parsed after the function prototype itself. This rule distinguishes
   between spec and other annotations by the first key word of the annotation.
   in order to return the appropriate token in clexer.mll
*/
is_spec:
  REQUIRES { () }
| BEHAVIOR { () }
| ENSURES { () }
| ASSIGNS { () }
| DECREASES { () }
| TERMINATES { () }
| COMPLETE { () } /* not sure it can be found alone */
;


loop_annotations:
| loop_annot_stack
    { let (i,a,b,v,p) = $1 in
      let invs = List.map (fun i -> AInvariant([],true,i)) i in
      let assigns = List.map (fun i -> AAssigns([],i)) a in
      (invs@assigns@b,v,p)
(*
      match i,a with
	| [],[] -> (b,v,p)
	| _ -> (ALoopBehavior([],i,a)::b,v,p)
*)
    }
;

loop_annot_stack:
| loop_invariant loop_annot_opt
    { let (i,a,b,v,p) = $2 in ($1::i,a,b,v,p) }
| loop_effects loop_annot_opt
    { let (i,a,b,v,p) = $2 in (i,$1 @ a,b,v,p) }
| FOR ne_full_identifier_list COLON loop_annot_opt
    { let (i,a,b,v,p) = $4 in
      let behav = $2 in
      let invs = List.map (fun i -> AInvariant(behav,true,i)) i in
      let assigns = List.map (fun i -> AAssigns(behav,i)) a in
      ([],[],invs@assigns@b,v,p) }
| loop_variant loop_annot_opt
    { let (i,a,b,v,p) = $2 in (i,a,b,AVariant $1::v,p) }
| loop_pragma loop_annot_opt
    { let (i,a,b,v,p) = $2 in (i,a,b,v,APragma (Loop_pragma $1)::p) }
;

loop_annot_opt:
| /* epsilon */
    { ([],[],[],[],[]) }
| loop_annot_stack
    { $1 }
;




type_annot:
| TYPE INVARIANT full_identifier LPAR full_parameter RPAR EQUAL
    full_lexpr SEMICOLON
  { let typ,name = $5 in{ inv_name = $3; this_name = name; this_type = typ; inv = $8; } }
;

variant:
  full_lexpr FOR full_identifier { ($1, Some $3) }
| full_lexpr                { ($1, None) }
;

loop_invariant:
| LOOP INVARIANT full_lexpr SEMICOLON { $3 }
;

loop_variant:
  LOOP VARIANT variant SEMICOLON { $3 }
;

decreases:
  /* epsilon */   { None }
| DECREASES variant SEMICOLON { Some $2 }
;

code_annotation:
  slice_pragma     { APragma (Slice_pragma $1) }
| impact_pragma    { APragma (Impact_pragma $1) }
| FOR ne_full_identifier_list COLON ASSERT full_lexpr SEMICOLON
      { AAssert ($2,$5,{status=Cil_types.Unknown}) }
| ASSERT full_lexpr SEMICOLON    { AAssert ([],$2,{status=Cil_types.Unknown}) }
| INVARIANT full_lexpr SEMICOLON { AInvariant ([],false,$2) }
;

loop_pragma_tk:
  LOOP PRAGMA { }
;

loop_pragma:
  loop_pragma_tk IDENTIFIER full_ne_lexpr_list SEMICOLON
  { if $2 = "UNROLL_LOOP" || $2 = "UNROLL" then
      match $3 with
        | [level] -> Unroll_level level
        | _ -> raise(
            Not_well_formed(loc(),"usage: loop pragma UNROLL n;"))
    else if $2 = "WIDEN_VARIABLES" then
      Widen_variables $3
    else if $2 = "WIDEN_HINTS" then
      Widen_hints $3
    else raise (Not_well_formed (loc(),"unknown loop pragma")) }

slice_pragma_tk:
  SLICE PRAGMA { }
;

slice_pragma:
  slice_pragma_tk IDENTIFIER full_lexpr SEMICOLON
    { if $2 = "expr" then SPexpr $3
      else raise (Not_well_formed (loc(), "unknown slice pragma")) }
| slice_pragma_tk IDENTIFIER SEMICOLON
    { if $2 = "ctrl" then SPctrl
      else if $2 = "stmt" then SPstmt
      else raise (Not_well_formed (loc(), "unknown slice pragma")) }

impact_pragma:
  IMPACT PRAGMA IDENTIFIER full_lexpr SEMICOLON
    { if $3 = "expr" then IPexpr $4
      else raise (Not_well_formed (loc(), "unknown impact pragma")) }
| IMPACT PRAGMA IDENTIFIER SEMICOLON
    { if $3 = "stmt" then IPstmt
      else raise (Not_well_formed (loc(), "unknown impact pragma")) }

loop_effects:
  LOOP ASSIGNS full_assigns SEMICOLON { $3 }
;

location_dep:
| zones { List.map (fun x -> (x,[])) $1 }
| zones FROM zones {List.map (fun x -> (x, $3)) $1}
| zones FROM NOTHING  {List.map (fun x -> (x, [Nothing])) $1}

zones : tsets { List.map (fun x -> Location x) $1 }

tsets:
| location { [$1] }
| location COMMA tsets { $1 :: $3 }
;

location:
| lexpr { $1 }
;

poly_id_type:
| full_identifier
    { enter_type_variables_scope []; ($1,[]) }
| full_identifier LT ne_tvar_list GT
        { enter_type_variables_scope $3; ($1,$3) }

/* we need to recognize the typename as soon as it has been declared, so
  so that it can be used in data constructors in the type definition itself
*/
poly_id_type_add_typename:
| poly_id_type { let (id,_) = $1 in Logic_env.add_typename id; $1 }

poly_id:
| poly_id_type { let (name,ty_vars) = $1 in
                 (name,[],ty_vars)
               }
| full_identifier LBRACE ne_label_list RBRACE {
    enter_type_variables_scope [];
    ($1,$3,[]) }
| full_identifier LBRACE ne_label_list RBRACE LT ne_tvar_list GT {
    enter_type_variables_scope $6;
    $1,$3,$6
  }

identifier:
| IDENTIFIER { $1 }
;

opt_parameters:
| /*epsilon*/ { [] }
| parameters { $1 }
;

parameters:
| LPAR full_parameters RPAR { $2 }
;

decl:
| logic_def  { $1 }
| deprecated_logic_decl
    { $1 }
| GLOBAL INVARIANT full_identifier COLON full_lexpr SEMICOLON
    { LDinvariant ($3, $5) }
| type_annot {LDtype_annot $1}
;

logic_def:
/* logic function definition */
| LOGIC full_logic_rt_type poly_id opt_parameters EQUAL full_lexpr SEMICOLON
    { let (id, labels, tvars) = $3 in
      exit_type_variables_scope ();
      LDlogic_def (id, labels, tvars, $2, $4, $6) }
/* predicate definition */
| PREDICATE poly_id opt_parameters EQUAL full_lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDpredicate_def (id, labels, tvars, $3, $5) }
/* inductive predicate definition */
| INDUCTIVE poly_id parameters LBRACE indcases RBRACE
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDinductive_def(id, labels, tvars, $3, $5) }
| LEMMA poly_id COLON full_lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDlemma (id, false, labels, tvars, $4) }
| AXIOMATIC identifier LBRACE logic_decls RBRACE
    { LDaxiomatic($2,$4) }
| TYPE poly_id_type_add_typename EQUAL datacons_list SEMICOLON
        { let (id,tvars) = $2 in
          exit_type_variables_scope ();
          LDtype(id,tvars,Some $4)
        }
;

deprecated_logic_decl:
/* OBSOLETE: logic function declaration */
| LOGIC full_logic_rt_type poly_id opt_parameters SEMICOLON
    { let (id, labels, tvars) = $3 in
      exit_type_variables_scope ();
      Format.eprintf "Warning: deprecated logic declaration '%s', should be declared inside an axiomatic block@." id;
      LDlogic_reads (id, labels, tvars, $2, $4, []) }
/* OBSOLETE: predicate declaration */
| PREDICATE poly_id opt_parameters SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      Format.eprintf "Warning: deprecated logic declaration `%s', should be declared inside an axiomatic block@." id;
      LDpredicate_reads (id, labels, tvars, $3, []) }
/* OBSOLETE: type declaration */
| TYPE poly_id_type SEMICOLON
    { let (id,tvars) = $2 in
      Logic_env.add_typename id;
      exit_type_variables_scope ();
      Format.eprintf "Warning: deprecated logic type declaration `%s', should be declared inside an axiomatic block@." id;
      LDtype(id,tvars,None) }
;


logic_decls:
| /* epsilon */
    { [] }
| logic_decl logic_decls
    { $1::$2 }
;

logic_decl:
| logic_def  { $1 }
/* logic function declaration */
| LOGIC full_logic_rt_type poly_id opt_parameters reads_clause SEMICOLON
    { let (id, labels, tvars) = $3 in
      exit_type_variables_scope ();
      LDlogic_reads (id, labels, tvars, $2, $4, $5) }
/* predicate declaration */
| PREDICATE poly_id opt_parameters reads_clause SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDpredicate_reads (id, labels, tvars, $3, $4) }
/* type declaration */
| TYPE poly_id_type SEMICOLON
    { let (id,tvars) = $2 in
      Logic_env.add_typename id;
      exit_type_variables_scope ();
      LDtype(id,tvars,None) }
/* axiom */
| AXIOM poly_id COLON full_lexpr SEMICOLON
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      LDlemma (id, true, labels, tvars, $4) }
;

reads_clause:
| /* epsilon */
    { [] }
| READS tsets
    { $2 }
;

datacons_list:
  /* epsilon */ { [] }
| ne_datacons_list { $1 }
| datacons { [$1] }
| datacons ne_datacons_list { $1 :: $2}
;

ne_datacons_list:
  PIPE datacons { [$2] }
| PIPE datacons ne_datacons_list { $2 :: $3 }
;

datacons:
  full_identifier { ($1,[]) }
| full_identifier LPAR ne_type_list RPAR { ($1,$3) }
;

ne_type_list:
  full_logic_type { [$1] }
| full_logic_type COMMA ne_type_list { $1::$3 }

indcases:
| /* epsilon */
    { [] }
| CASE poly_id COLON lexpr SEMICOLON indcases
    { let (id,labels,tvars) = $2 in
      exit_type_variables_scope ();
      (id,labels,tvars,$4)::$6 }
;


ne_tvar_list:
| full_identifier { [$1] }
| full_identifier COMMA ne_tvar_list { $1 :: $3 }
;

ne_label_list:
| label { [$1] }
| label COMMA ne_label_list { $1 :: $3 }
;

label:
| full_identifier { $1 }
;

annot:
  annotation EOF   { $1 }
;

decl_list:
  decl { [(loc(), $1)] }
| decl decl_list { (loc(),$1) :: $2 }

annotation:
  decl_list             { Adecl ($1) }
| is_spec any      { Aspec }
| loop_annotations
      { let (b,v,p) = $1 in
	(* TODO: do better, do not lose the structure ! *)
	let l = b@v@p in
	Logic_utils.check_loop_annotation ~loc:(loc()) l;
        Aloop_annot (loc (), l) }
| code_annotation { Acode_annot (loc(),$1) }
| IDENTIFIER      { Aattribute_annot (loc (), $1) }

;

any:
  wildcard { () }
| wildcard any { () }
;

wildcard:
  IDENTIFIER { () }
| STRING_LITERAL { () }
| TYPENAME { () }
| CONSTANT { () }
| LPAR { () }
| RPAR { () }
| IF { () }
| ELSE { () }
| COLON { () }
| COLON2 { () }
| COLONCOLON { () }
| DOT { () }
| DOTDOT { () }
| DOTDOTDOT { () }
| INT { () }
| INTEGER { () }
| REAL { () }
| FLOAT { () }
| LT { () }
| GT { () }
| LE { () }
| GE { () }
| EQ { () }
| NE { () }
| COMMA { () }
| ARROW { () }
| EQUAL { () }
| FORALL { () }
| EXISTS { () }
| IFF { () }
| IMPLIES { () }
| AND { () }
| OR { () }
| NOT { () }
| TRUE { () }
| FALSE { () }
| OLD { () }
| AT { () }
| RESULT { () }
| BLOCK_LENGTH { () }
| BASE_ADDR { () }
| VALID { () }
| VALID_INDEX { () }
| VALID_RANGE { () }
| FRESH { () }
| DOLLAR { () }
| QUESTION { () }
| MINUS { () }
| PLUS { () }
| STAR { () }
| AMP { () }
| SLASH { () }
| PERCENT { () }
| LSQUARE { () }
| RSQUARE { () }
| GLOBAL { () }
| INVARIANT { () }
| VARIANT { () }
| DECREASES { () }
| FOR { () }
| LABEL { () }
| ASSERT { () }
| SEMICOLON { () }
| NULL { () }
| EMPTY { () }
| REQUIRES { () }
| ENSURES { () }
| ASSIGNS { () }
| LOOP { () }
| NOTHING { () }
| SLICE { () }
| PRAGMA { () }
| FROM { () }
| READS { () }
| LOGIC { () }
| PREDICATE { () }
| AXIOM { () }
| LEMMA { () }
| LBRACE { () }
| RBRACE { () }
| GHOST { () }
| VOID { () }
| CHAR { () }
| SIGNED { () }
| UNSIGNED { () }
| SHORT { () }
| LONG { () }
| DOUBLE { () }
| STRUCT { () }
| ENUM { () }
| UNION { () }
| BSUNION { () }
| INTER { () }
| LTCOLON { () }
| COLONGT { () }
| TYPE { () }
| BEHAVIOR { () }
| ASSUMES { () }
| HAT { () }
| HATHAT { () }
| PIPE { () }
| TILDE { () }
| GTGT { () }
| LTLT { () }
| SIZEOF { () }
| COMPLETE { () }
| BEHAVIORS { () }
| DISJOINT { () }
| TERMINATES { () }
| LAMBDA { () }
| TYPEOF { () }
| BSTYPE { () }
| SEPARATED { () }
;

%%

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
