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

/*

Parser for Java source files

$Id: java_parser.mly,v 1.58 2008/12/09 09:14:18 marche Exp $

*/


%{
  
  open Loc
  open Java_env
  open Java_ast

  let loc () = (symbol_start_pos (), symbol_end_pos ())
  let loc_i i = (rhs_start_pos i, rhs_end_pos i)

  let locate_expr e =
    { java_pexpr_node = e ; java_pexpr_loc = loc () }

  let locate_lit e = locate_expr (JPElit e)

  let locate_statement s =
    { java_pstatement_node = s ; java_pstatement_loc = loc () }

  let rec build_array_type t n =
    if n=0 then t else Array_type_expr(build_array_type t (pred n))

(*
  let rec build_array_creation_expr t (l,n) =
    match l with
      | [] -> 
	  Implicit_array_creation(build_array_type t n)
      | a::b ->
	  Explicit_array_creation(a,(build_array_creation_expr t (b,n)))
*)

  let default_behavior_name = "default"

  let default_behavior assigns ensures behs =
    match assigns,ensures with
      | None, None -> behs
      | a, None ->
	  ((Loc.dummy_position,default_behavior_name),
	   { java_pbehavior_assumes = None;
	     java_pbehavior_assigns = a;
	     java_pbehavior_throws = None;
	     java_pbehavior_ensures = Java_pervasives.expr_true }) :: behs
      | a, Some e -> 
	  ((Loc.dummy_position,default_behavior_name),
	   { java_pbehavior_assumes = None;
	     java_pbehavior_assigns = a;
	     java_pbehavior_throws = None;
	     java_pbehavior_ensures = e }) :: behs

(*
  let label (loc,s) = match s with
    | "Pre" -> LabelPre
    | "Here" -> LabelHere
    | "Old" -> LabelOld
    | _ -> LabelName s
*)

  let float_of_suf = function
    | None -> `Real
    | Some('f' | 'F') -> `Single
    | Some('d' | 'D') -> `Double
    | _ -> assert false
%}

/*s Start symbols */

%start compilation_unit
%type  <Java_ast.compilation_unit> compilation_unit

%start kml_global_def_eof
%type  <Java_ast.type_declaration> kml_global_def_eof

%start kml_field_decl
%type  <Java_ast.field_declaration> kml_field_decl

%start kml_statement_annot
%type  <Java_ast.pstatement_node> kml_statement_annot

%start kml_modifier
%type  <Java_ast.modifier> kml_modifier

%type <Java_ast.qualified_ident> name

/*s Tokens */

/* Literals */

%token <string> ID 
%token <string> INTCONSTANT
%token <string * char option> REALCONSTANT
%token <string> STRING
%token <string> CHARACTER
%token TRUE FALSE NULL THIS 

/* Keywords */

%token NEW SUPER
%token ABSTRACT BOOLEAN BYTE CASE CATCH
%token CHAR CLASS CONST DEFAULT DOUBLE ELSE EXTENDS
%token FALSE FINAL FINALLY FLOAT GHOST GOTO 
/* ??? %token FUTURE BYVALUE GENERIC INNER OPERATOR OUTER REST VAR */
%token IMPLEMENTS IMPORT INSTANCEOF INT INTEGER INTERFACE LONG
%token MODEL NATIVE PACKAGE PRIVATE PROTECTED
%token PUBLIC REAL SHORT STATIC STRICTFP
%token THROWS TRANSIENT TRUE VOID VOLATILE
%token WHILE DO FOR IF SWITCH BREAK CONTINUE RETURN TRY SYNCHRONIZED THROW 

%token REQUIRES DECREASES ENSURES SIGNALS ASSUMES ASSIGNS BEHAVIOR ASSERT
%token INVARIANT LOOP_INVARIANT LOOP_VARIANT 
%token AXIOM LEMMA LOGIC TYPE PREDICATE INDUCTIVE AXIOMATIC READS
%token BSFORALL BSEXISTS BSOLD BSAT BSRESULT BSNOTHING
%token NON_NULL NULLABLE

/* Others symbols */

%token LEFTPAR 
%token RIGHTPAR LEFTBRACE RIGHTBRACE LEFTBRACKET RIGHTBRACKET
%token SEMICOLON COLON COMMA QUESTIONMARK DOT DOTDOT
%token <string> DOC_COMMENT
%token <Lexing.position*string> ANNOT
%token EOF

/* Operators (see precedences below for details) */

%token <Java_ast.bin_op> ASSIGNOP
%token EQ EQEQGT LTEQEQGT
%token VERTICALBARVERTICALBAR
%token AMPERSANDAMPERSAND  
%token VERTICALBAR         
%token CARET               
%token AMPERSAND           
%token <Java_ast.bin_op> EQOP
%token <Java_ast.bin_op> COMP 
%token <Java_ast.bin_op> SHIFT
%token PLUS MINUS 
%token STAR SLASH PERCENT
%token PLUSPLUS MINUSMINUS TILDA BANG

/*s Operator precedences */

%nonassoc THEN
%nonassoc ELSE
%nonassoc BSFORALL
%right LTEQEQGT 
%right EQEQGT 
x%right EQ ASSIGNOP 
  /*r ["="], ["*="],  ["/="], ["%="], ["+="], ["-="], ["<<="], [">>="], 
  [">>>="], ["&="], ["^="] and ["|="], and ["==>"] ["<==>"] */ 
%right IFEXPR QUESTIONMARK     /*r [" ? : "] */
%left VERTICALBARVERTICALBAR  /*r conditional OR ["||"] */
%left AMPERSANDAMPERSAND  /*r conditional AND ["&&"] */
%left VERTICALBAR         /*r bitwise or boolean OR ["|"] */
%left CARET               /*r bitwise or boolean XOR ["^"] */
%left AMPERSAND           /*r bitwise or boolean AND ["&"] */
%left EQOP                /*r ["=="] and ["!="] */
%left COMP INSTANCEOF  /*r ["<"], ["<="], [">"], [">="] and ["instanceof"] */
%left SHIFT                 /*r ["<<"], [">>"] and [">>>"] */
%left PLUS MINUS             /*r ["+"] and ["-"] */
%left STAR SLASH PERCENT     /*r ["*"], ["/"] and ["%"] */
%right UMINUS UPLUS PLUSPLUS MINUSMINUS TILDA BANG CAST
           /*r unary ["+"], ["-"], ["++"], ["--"], ["~"], ["!"] and cast */


%%

/*s Compilation units */

compilation_unit:
| package_declaration import_declarations type_declarations EOF
    { { cu_package = $1 ;
	cu_imports = $2 ;
	cu_type_decls = $3} }
;

package_declaration:
| /* $\varepsilon$ */
    { [] }
| PACKAGE name SEMICOLON
    { $2 }
;

import_declarations:
| /* $\varepsilon$ */
    { [] }
| IMPORT import_declaration SEMICOLON import_declarations 
    { $2::$4 }
;


import_declaration:
| name DOT STAR 
    { Import_package($1) }
| name 
    { Import_class_or_interface($1) }
;

/*s type declarations */

type_declarations:
| /* $\varepsilon$ */
    { [] }
| type_declaration type_declarations
    { $1::$2 }
|  SEMICOLON type_declarations
    { $2 }
;

type_declaration:
| class_declaration 
    { JPTclass($1) }
| interface_declaration
    { JPTinterface($1) }
| ANNOT
    { let (loc,s) = $1 in JPTannot(loc,s) } 
;

/*s Class declarations */

class_declaration:
| modifiers_class ident extends_decl implements_decl class_body
    { { class_modifiers = $1;
	class_name = $2;
	class_extends = $3;
	class_implements = $4;
	class_fields = $5 }}
;

class_body:
  LEFTBRACE field_declarations RIGHTBRACE
    { $2 }

extends_decl:
| /* $\varepsilon$ */
    { None }
| EXTENDS name 
    { Some $2 }
;

implements_decl:
| /* $\varepsilon$ */
    { [] }
| IMPLEMENTS name_comma_list 
    { $2 }
;

field_declarations:
| /* $\varepsilon$ */
    { [] }
| field_declaration field_declarations
    { $1::$2 }
;

field_declaration:
| method_declaration 
    { $1 }
| constructor_declaration 
    { $1 }
| variable_declaration 
    { JPFvariable($1) }
| static_initializer 
    { JPFstatic_initializer($1) }
| ANNOT
    { let (loc,s)=$1 in JPFannot(loc,s) }
/* Java 1.4 */
| class_declaration
    { JPFclass $1 }
| interface_declaration
    { JPFinterface $1 }
;

/*s variable declarations */

variable_declaration:
| modifiers_type_expr variable_declarators SEMICOLON
    { let a, b = $1 in
	match b with
	  | Some b ->
	      { variable_modifiers = a ;
		variable_type = b ;
		variable_decls = $2 } 
	  | None -> raise Parse_error } 
| modifiers_type_expr ANNOT variable_declarators SEMICOLON
    { let a, b = $1 in
	match b with
	  | Some b ->
	      let loc, s = $2 in 
	      { variable_modifiers = Annot_modifier (loc, s) :: a ;
		variable_type = b ;
		variable_decls = $3 } 
	  | None -> raise Parse_error } 
;

variable_declarators:
| variable_declarator
    { [$1] }
| variable_declarator COMMA variable_declarators
    { $1::$3 }
;

variable_declarator:
| variable_declarator_id 
    { { variable_id = $1 ;
	variable_initializer = None } }
| variable_declarator_id EQ variable_initializer
    { { variable_id = $1 ;
	variable_initializer = Some $3 } }
;

variable_declarator_id:
| ident
    { let (loc,id)=$1 in Simple_id(loc,id) }
| variable_declarator_id LEFTBRACKET RIGHTBRACKET
    { Array_id($1) } 

variable_initializer:
| expr
    { Simple_initializer($1) }
| array_initializer
    { Array_initializer($1) }
;

array_initializer:
| LEFTBRACE variable_initializers RIGHTBRACE
    { $2 }
;

variable_initializers:
| /* $\varepsilon$ */
    { [] }
| variable_initializer 
    { [$1] }
| variable_initializer COMMA variable_initializers
    { $1::$3 }
; 


/*s static initializers */

static_initializer:
| STATIC block 
    { $2 }
;


/*s method declarations */

method_declaration:
| method_header method_body 
    { JPFmethod($1,$2) }
;

method_header:
| modifiers_type_expr method_declarator throws_decl
    { let (a, b) = $1 in
      { 
	method_modifiers = a ;
	method_return_type = b ;
	method_declarator = $2 ;
	method_throws = $3 
      } 
    }
| modifiers_type_expr ANNOT method_declarator throws_decl
    { let (a, b) = $1 in
      let loc, s = $2 in
	{ 
	  method_modifiers = Annot_modifier (loc, s) :: a ;
	  method_return_type = b ;
	  method_declarator = $3 ;
	  method_throws = $4 
	} 
    }
;

method_declarator:
| ident method_parameters 
    { Simple_method_declarator($1,$2) }
| method_declarator LEFTBRACKET RIGHTBRACKET
    { Array_method_declarator($1) }
;


method_parameters:
| LEFTPAR RIGHTPAR 
    { [] }
| LEFTPAR parameter_comma_list RIGHTPAR
    { $2 }
;

parameter_comma_list:
| parameter
    { [$1] }
| parameter COMMA parameter_comma_list
    { $1::$3 }
;

parameter:
/* final modifier allowed since 1.4 (JLS 3.0) */
| final_opt type_expr ident 
    { Simple_parameter (None, $2, $3) }
| final_opt type_expr ANNOT ident
    { let loc, s = $3 in 
      Simple_parameter (Some (Annot_modifier (loc, s)), $2, $4) } 
| parameter LEFTBRACKET RIGHTBRACKET
    { Array_parameter($1) }
;

final_opt:
| /* \epsilon */ { false}
| FINAL { true }
;


throws_decl:
| /* $\varepsilon$ */
    { [] }
| THROWS name_comma_list
    { $2 }
;

method_body:
| block 
    { Some($1) } 
| SEMICOLON
    { None }
;

/*s constructor declarations */

constructor_declaration:
| modifiers_type_expr method_parameters throws_decl constructor_body
    { let (a,b)=$1 in
      match b with
	| Some (Type_name [id]) ->
	    let c =
	      { 
	      constr_modifiers = a ;
	      constr_name = id ;
	      constr_parameters = $2 ;
	      constr_throws = $3 }
	    in
	    let eci,b = $4 in
	    JPFconstructor(c,eci,b) 
	| _ -> raise Parse_error}
;

constructor_body:
| LEFTBRACE explicit_constructor_invocation statements RIGHTBRACE   
    { ($2,$3) }
| LEFTBRACE statements RIGHTBRACE   
    { (Invoke_none,$2) }
| SEMICOLON
    { (Invoke_none,[]) }
;

explicit_constructor_invocation:
| THIS LEFTPAR argument_list RIGHTPAR SEMICOLON
    { Invoke_this($3) }
| SUPER LEFTPAR argument_list RIGHTPAR SEMICOLON
    { Invoke_super($3) }
;

argument_list:
| /* $\varepsilon$ */
    { [] }
| expr_comma_list
    { $1 }
;

/*s interface declarations */

interface_declaration:
| modifiers_interface ident extends_interfaces_decl 
    LEFTBRACE interface_member_declarations RIGHTBRACE
    { { interface_modifiers = $1;
	interface_name = $2;
	interface_extends = $3;
	interface_members = $5 }}
;

extends_interfaces_decl:
| /* $\varepsilon$ */
    { [] }
| EXTENDS name_comma_list 
    { $2 }
;

interface_member_declarations:
| /* $\varepsilon$ */
    { [] }
| interface_member_declaration interface_member_declarations
    { $1::$2 }
;

interface_member_declaration:
| variable_declaration
    { JPFvariable($1) }
| method_header SEMICOLON
    { JPFmethod($1,None) }
| ANNOT
    { let (loc,s)=$1 in JPFannot(loc,s) }
/* Java 1.4 */
| interface_declaration
    { JPFinterface $1 }
;




/*s type expressions */

base_type:
| SHORT 
    { Tshort }
| BOOLEAN 
    { Tboolean }
| BYTE 
    { Tbyte }
| CHAR 
    { Tchar }
| INT 
    { Tint }
| FLOAT 
    { Tfloat }
| LONG 
    { Tlong }
| DOUBLE 
    { Tdouble }
| INTEGER
    { Tinteger }
| REAL
    { Treal }
;

type_expr:
| name 
    { Type_name($1) }
| base_type  
    { Base_type($1) }
| array_type_expr
    { Array_type_expr($1) }
;

array_type_expr:
| base_type LEFTBRACKET RIGHTBRACKET
    { Base_type($1) }
| name LEFTBRACKET RIGHTBRACKET
    { Type_name($1) }
| array_type_expr LEFTBRACKET RIGHTBRACKET
    { Array_type_expr($1) }
;


/*s modifiers */

modifiers_class:
| CLASS 
    { [] }
| modifier modifiers_class
    { $1::$2 }
;

modifiers_interface:
| INTERFACE
    { [] }
| modifier modifiers_interface
    { $1::$2 }
;


modifier:
| STATIC
    { Static }
| FINAL
    { Final }
| PUBLIC
    { Public }
| PRIVATE 
    { Private }
| PROTECTED
    { Protected }
| NATIVE
    { Native }
| SYNCHRONIZED
    { Synchronized }
| ABSTRACT
    { Abstract }
/* "threadsafe" ? */
| TRANSIENT
    { Transient }
| VOLATILE
    { Volatile }
| STRICTFP
    { Strictfp }
;

modifiers_type_expr:
| type_expr
    { ([],Some $1) }
| VOID
    { ([],None) }
| modifier modifiers_type_expr
    { let (a,b)=$2 in ($1::a,b) }
;

/*s Statements */

block:
| LEFTBRACE statements RIGHTBRACE   
    { $2 }

statements:
| statement statements
    { $1::$2 } 
| /* $\varepsilon$ */
    { [] } 
;

/*s Statements */

local_variable_declaration:
| modifiers_type_expr variable_declarators
    { let (a,b)=$1 in
      match b with
	| Some b ->
	    { variable_modifiers = a ;
	      variable_type = b ;
	      variable_decls = $2 }
	| None -> raise Parse_error } 
;

statement:
| ANNOT
    { let (loc, s) = $1 in 
	locate_statement (JPSannot (loc, s)) }
| WHILE LEFTPAR expr RIGHTPAR statement %prec WHILE
    { locate_statement (JPSwhile($3,$5)) }
| DO statement WHILE LEFTPAR expr RIGHTPAR 
    { locate_statement (JPSdo($2,$5)) } 
| FOR LEFTPAR argument_list SEMICOLON for_cond SEMICOLON 
  argument_list RIGHTPAR statement 
    { locate_statement (JPSfor($3,$5,$7,$9)) }
| FOR LEFTPAR local_variable_declaration SEMICOLON for_cond SEMICOLON 
  argument_list RIGHTPAR statement 
    { locate_statement (JPSfor_decl($3,$5,$7,$9)) }
| block
    { locate_statement (JPSblock $1) }
| expr SEMICOLON
    { locate_statement (JPSexpr($1)) }
| SEMICOLON
    { locate_statement JPSskip }
| local_variable_declaration SEMICOLON
    { locate_statement (JPSvar_decl($1)) }
| IF LEFTPAR expr RIGHTPAR statement %prec THEN
    { locate_statement (JPSif($3,$5,locate_statement JPSskip)) }
| IF LEFTPAR expr RIGHTPAR statement ELSE statement %prec ELSE
    { locate_statement (JPSif($3,$5,$7)) }
| SWITCH LEFTPAR expr RIGHTPAR LEFTBRACE switch_block RIGHTBRACE
    { locate_statement (JPSswitch($3,$6)) }
| ident COLON statement
    { locate_statement (JPSlabel($1,$3)) } 
| BREAK SEMICOLON
    { locate_statement (JPSbreak None) }
| BREAK ident SEMICOLON
    { locate_statement (JPSbreak(Some $2)) }
| CONTINUE SEMICOLON
    { locate_statement (JPScontinue(None)) }
| CONTINUE ident SEMICOLON
    { locate_statement (JPScontinue(Some $2)) }
| RETURN SEMICOLON
    { locate_statement (JPSreturn None) }
| RETURN expr SEMICOLON
    { locate_statement (JPSreturn(Some $2)) }
| THROW expr SEMICOLON
    { locate_statement (JPSthrow($2)) }
| TRY block catch_clauses
    { locate_statement (JPStry($2,$3,None)) }
| TRY block catch_clauses FINALLY block
    { locate_statement (JPStry($2,$3,Some($5))) }
| TRY block FINALLY block
    { locate_statement (JPStry($2,[],Some($4))) }
| SYNCHRONIZED LEFTPAR expr RIGHTPAR block
    { locate_statement (JPSsynchronized($3,$5)) }

for_cond:
| expr
    { $1 }
| /* $\varepsilon$ */
    { locate_expr Java_pervasives.expr_node_true }
;

switch_block: 
| /* $\varepsilon$ */
    { [] }
| switch_labels 
    { [($1,[])] }
| switch_labels statement statements switch_block
    { ($1,$2::$3)::$4 }
;

switch_labels:
| switch_label
    { [$1] }
| switch_label switch_labels
    { $1::$2 }
;

switch_label:
| CASE expr COLON
    { Case($2) }
| DEFAULT COLON
    { Default }
;

catch_clauses:
| catch_clause
    { [$1] }
| catch_clause catch_clauses
    { $1::$2 }
;

catch_clause:
| CATCH LEFTPAR parameter RIGHTPAR block
    { ($3,$5) }
;

/*s Expressions */

field_access:
| SUPER DOT ident
    { Super_access $3 }
| primary_expr DOT ident
    { Primary_access($1,$3) }
;

primary_expr:
| primary_no_new_array
    { $1 }
| array_creation_expression
    { $1 }
;

primary_no_new_array:
| INTCONSTANT                    
    { locate_lit (Integer $1) }
| REALCONSTANT                    
    { let x,suf = $1 in locate_lit (Float(x,float_of_suf suf)) }
| TRUE                       
    { locate_lit (Bool true) }
| FALSE                      
    { locate_lit (Bool false) } 
| STRING                     
    { locate_lit (String $1) }
| NULL                     
    { locate_lit Null }
| CHARACTER                     
    { locate_lit (Char $1) }
| THIS
    { locate_expr JPEthis }
| BSRESULT
    { locate_expr JPEresult }
| LEFTPAR expr_no_name RIGHTPAR      
    { $2 }
| LEFTPAR name RIGHTPAR
    { locate_expr (JPEname $2) }
| field_access
    { locate_expr (JPEfield_access $1) }
| name label_binders LEFTPAR argument_list RIGHTPAR
    { locate_expr (JPEcall_name($1,$2,$4)) } 
| SUPER DOT ident LEFTPAR argument_list RIGHTPAR
    { locate_expr (JPEsuper_call($3, $5)) }
| primary_expr DOT ident LEFTPAR argument_list RIGHTPAR
    { locate_expr (JPEcall_expr($1,$3,$5)) } 
| NEW name LEFTPAR argument_list RIGHTPAR
    { locate_expr (JPEnew($2,$4)) }
/* new in java 1.4 (see JLS 3.0) */
| NEW name LEFTPAR argument_list RIGHTPAR class_body
    { (* TODO: incorporate class body *)
      locate_expr (JPEnew($2,$4)) }
| array_access
    { let (a,b)=$1 in locate_expr (JPEarray_access(a,b)) }
| array_range
    { let (a,b,c)=$1 in locate_expr (JPEarray_range(a,b,c)) }
| BSOLD LEFTPAR expr RIGHTPAR
    { locate_expr (JPEold $3) }
| BSAT LEFTPAR expr COMMA ident RIGHTPAR
    { locate_expr (JPEat($3,$5)) }
;

array_access:
| primary_no_new_array LEFTBRACKET expr RIGHTBRACKET
    { ($1,$3) }
| name LEFTBRACKET expr RIGHTBRACKET
    { (locate_expr (JPEname $1),$3) }
;

array_range:
| primary_no_new_array LEFTBRACKET expr_opt DOTDOT expr_opt RIGHTBRACKET
    { ($1,$3,$5) }
| name LEFTBRACKET expr_opt DOTDOT expr_opt RIGHTBRACKET
    { (locate_expr (JPEname $1),$3,$5) }
;

expr_opt:
| /* epsilon */
    { None }
| expr
    { Some $1 }
;

array_creation_expression:
| NEW base_type array_dims 
    { let (a,b) = $3 in 
      let t = build_array_type (Base_type($2)) b in
      locate_expr (JPEnew_array(t,a)) }
| NEW name array_dims 
    { let (a,b) = $3 in 
      let t = build_array_type (Type_name($2)) b in
      locate_expr (JPEnew_array(t,a)) }
/* array_initializer allowed in 1.4 (JLS 3.0) */
| NEW base_type implicit_dims array_initializer
    { let b = $3 in 
      let t = build_array_type (Base_type($2)) b in
      locate_expr (JPEnew_array(t,[])) }
| NEW name implicit_dims array_initializer
    { let b = $3 in 
      let t = build_array_type (Type_name($2)) b in
      locate_expr (JPEnew_array(t,[])) }
;

array_dims:
| LEFTBRACKET expr RIGHTBRACKET implicit_dims
    { ([$2],$4) }
| LEFTBRACKET expr RIGHTBRACKET array_dims
    { let (a,b) = $4 in ($2::a,b) }
;

implicit_dims:
| /* $\varepsilon$$ */
    { 0 }
| LEFTBRACKET RIGHTBRACKET implicit_dims
    { succ $3 }
;

castable_expr:
| primary_expr
    { $1 }
| name
    { locate_expr (JPEname $1) }
| non_basic_cast
    { $1 }

non_basic_cast:
| LEFTPAR array_type_expr RIGHTPAR castable_expr %prec CAST
    { locate_expr (JPEcast(Array_type_expr($2),$4)) }
| LEFTPAR name RIGHTPAR castable_expr %prec CAST
    { locate_expr (JPEcast(Type_name($2),$4)) }
;

expr:
| name
    { locate_expr (JPEname $1) }
| expr_no_name
    { $1 }
;

expr_no_name:
| primary_expr
    { $1 }
| name assign_op expr %prec ASSIGNOP
    { locate_expr (JPEassign_name($1,$2,$3)) }
| field_access assign_op expr %prec ASSIGNOP
    { locate_expr (JPEassign_field($1,$2,$3)) }
| array_access assign_op expr %prec ASSIGNOP
    { let (a,b)=$1 in 
      locate_expr (JPEassign_array(a,b,$2,$3)) }
| PLUSPLUS expr 
    { locate_expr (JPEincr(Preincr,$2)) }
| MINUSMINUS expr 
    { locate_expr (JPEincr(Predecr,$2)) }
| expr PLUSPLUS 
    { locate_expr (JPEincr(Postincr,$1)) }
| expr MINUSMINUS 
    { locate_expr (JPEincr(Postdecr,$1)) }
| expr QUESTIONMARK expr COLON expr %prec IFEXPR
    { locate_expr (JPEif($1,$3,$5)) }
| expr VERTICALBARVERTICALBAR expr          
    { locate_expr (JPEbin($1,Bor,$3)) }
| expr AMPERSANDAMPERSAND expr          
    { locate_expr (JPEbin($1,Band,$3)) }
| expr VERTICALBAR expr          
    { locate_expr (JPEbin($1,Bbwor,$3)) }
| expr CARET expr          
    { locate_expr (JPEbin($1,Bbwxor,$3)) } 
| expr AMPERSAND expr          
    { locate_expr (JPEbin($1,Bbwand,$3)) } 
| expr EQOP expr          
    { locate_expr (JPEbin($1,$2,$3)) } 
| expr COMP expr         
    { locate_expr (JPEbin($1,$2,$3)) } 
| expr SHIFT expr         
    { locate_expr (JPEbin($1,$2,$3)) } 
| expr PLUS expr          
    { locate_expr (JPEbin($1,Badd,$3)) }  
| expr MINUS expr         
    { locate_expr (JPEbin($1,Bsub,$3)) }  
| expr STAR expr        
    { locate_expr (JPEbin($1,Bmul,$3)) } 
| expr SLASH expr       
    { locate_expr (JPEbin($1,Bdiv,$3)) }  
| expr PERCENT expr        
    { locate_expr (JPEbin($1,Bmod,$3)) }  
| PLUS expr %prec UPLUS 
    { locate_expr (JPEun(Uplus,$2)) }  
| MINUS expr %prec UMINUS 
    { locate_expr (JPEun(Uminus,$2)) }  
| BANG expr
    { locate_expr (JPEun(Unot,$2)) }  
| TILDA expr
    { locate_expr (JPEun(Ucompl,$2)) }  
/*

  CAST expressions

  we distinguish cast types because of syntax ambiguities:
  is (id1)-id2  a cast of a unary minus, or a binary - ?

  solution:
       
  if id1 is a base type, it is a cast else it is a binary operation. 
  it is enough because result of unary - cannot be casted to something 
  else than a base type.    

  moreover, we distinguish between cast to a type identifier 
  "(name) expr" and a complex type expr, because of LALR constraint:
  (name) can be both an expr and a cast, so it is factorized. 

*/
| LEFTPAR base_type RIGHTPAR expr %prec CAST
    { locate_expr (JPEcast(Base_type($2),$4)) }
| non_basic_cast
    { $1 }
/* 
  instanceof operator
*/
| expr INSTANCEOF type_expr
    { locate_expr (JPEinstanceof($1,$3)) }
/* annotations only operators */
| BSFORALL quantified_variables_decl SEMICOLON expr %prec BSFORALL
    { locate_expr (JPEquantifier(Forall,$2,$4)) }
| BSEXISTS quantified_variables_decl SEMICOLON expr %prec BSFORALL
    { locate_expr (JPEquantifier(Exists,$2,$4)) }
| expr EQEQGT expr
    { locate_expr (JPEbin($1,Bimpl,$3)) }
| expr LTEQEQGT expr
    { locate_expr (JPEbin($1,Biff,$3)) }
;

quantified_variables_decl:
| type_expr quantified_variables
    { [($1,$2)] }
| type_expr quantified_variables COMMA quantified_variables_decl
    { ($1,$2)::$4 }
;

quantified_variables:
| quantified_variable 
    { [$1] }
| quantified_variable quantified_variables
    { $1::$2 }
;

quantified_variable:
| ident
    { let (loc,id)=$1 in Simple_id(loc,id) }
| quantified_variable LEFTBRACKET RIGHTBRACKET
    { Array_id($1) } 


expr_comma_list:
| expr
    { [$1] }
| expr COMMA expr_comma_list
    { $1::$3 }
;

assign_op:
| EQ 
    { Beq }
| ASSIGNOP
    { $1 }
;

/*s identifiers */


name:
| ident
    { [$1] }
| name DOT ident
    { $3::$1 }
| name DOT CLASS
    { (loc_i 3,"class")::$1 }
| name DOT THIS
    { (loc_i 3,"this")::$1 }
;

name_comma_list:
| name
    { [$1] }
| name COMMA name_comma_list
    { $1::$3 }
;

ident:
| ID
    { (loc(),$1) }
| MODEL
    { (loc(), "model") }
| TYPE
    { (loc(), "type") }
;

/****************************************************/
/*s parsing of annotations: KML */

kml_global_def_eof:
| kml_global_def EOF
     { $1 }
;

kml_global_def:
| PREDICATE ident label_binders method_parameters EQ expr SEMICOLON
    { JPTlogic_def($2,None,$3,$4,$6) }
| INDUCTIVE ident label_binders method_parameters LEFTBRACE indcases RIGHTBRACE
    { JPTinductive($2,$3,$4,$6) }
| LOGIC type_expr ident label_binders method_parameters EQ expr SEMICOLON
    { JPTlogic_def($3,Some $2,$4, $5,$7) }
| AXIOMATIC ident LEFTBRACE kml_global_decls RIGHTBRACE
    { JPTaxiomatic($2,$4) }
| LEMMA ident label_binders COLON expr SEMICOLON
    { JPTlemma($2,false,$3,$5) }
;

kml_global_decls:
| /* epsilon */
    { [] }
| kml_global_decl kml_global_decls
    { $1::$2 }
;

kml_global_decl:
| TYPE ident SEMICOLON
    { JPTlogic_type_decl $2 }
| PREDICATE ident label_binders method_parameters reads_clause SEMICOLON
    { JPTlogic_reads ($2, None, $3, $4, $5) }
| LOGIC type_expr ident label_binders method_parameters reads_clause SEMICOLON
    { JPTlogic_reads($3,Some $2,$4,$5,$6) }
| AXIOM ident label_binders COLON expr SEMICOLON
    { JPTlemma($2,true,$3,$5) }
| kml_global_def
    { $1 }
;

reads_clause:
/* OBSOLETE
| READS expr_comma_list
    { $2 }
*/
| /* epsilon */ 
    { [] }
;
 
indcases:
| /* epsilon */
    { [] }
| CASE ident label_binders COLON expr SEMICOLON indcases
    { ($2,$3,$5)::$7 }
;

label_binders:
| /* epsilon */ { [] }
| LEFTBRACE ident label_list_end RIGHTBRACE { $2::$3 }
;

label_list_end:
| /* epsilon */ { [] }
| COMMA ident label_list_end { $2::$3 }
;


kml_field_decl:
| requires_opt decreases_opt assigns_opt ensures_opt behaviors EOF
    { JPFmethod_spec($1,$2,default_behavior $3 $4 $5) }
| INVARIANT ident COLON expr SEMICOLON EOF
    { JPFinvariant($2,$4) } 
| STATIC INVARIANT ident COLON expr SEMICOLON EOF
    { JPFstatic_invariant($3,$5) } 
| MODEL variable_declaration
    { let vd = $2 in 
      JPFvariable {vd with variable_modifiers = Model::vd.variable_modifiers} }
| GHOST variable_declaration
    { let vd = $2 in 
      JPFvariable {vd with variable_modifiers = Ghost::vd.variable_modifiers} }
;

kml_modifier:
| NON_NULL
    { Non_null }
| NULLABLE
    { Nullable }
;

requires_opt:
| /* $\varepsilon$ */
    { None }
| REQUIRES expr SEMICOLON
    { Some $2 }
;

ensures_opt:
| /* $\varepsilon$ */
    { None }
| ENSURES expr SEMICOLON
    { Some $2 }
;

behaviors:
| /* $\varepsilon$ */
    { [] }
| BEHAVIOR ident COLON behavior behaviors
    { ($2,$4)::$5 }
;

behavior:
| assumes_opt assigns_opt ENSURES expr SEMICOLON
    { { java_pbehavior_assumes = $1;
	java_pbehavior_assigns = $2;
	java_pbehavior_throws = None;
	java_pbehavior_ensures = $4 } }
| assumes_opt assigns_opt SIGNALS LEFTPAR name ident_option RIGHTPAR expr SEMICOLON
    { { java_pbehavior_assumes = $1;
	java_pbehavior_assigns = $2;
	java_pbehavior_throws = Some($5,$6);
	java_pbehavior_ensures = $8 } }
| error
	{ raise (Java_options.Java_error (loc(),"`ensures' expected")) }
;

ident_option:
| /* $\varepsilon$ */
    { None }
| ident
    { Some $1 }
;
 
assumes_opt:
| /* $\varepsilon$ */
    { None }
| ASSUMES expr SEMICOLON
    { Some $2 }
;


assigns_opt:
| /* $\varepsilon$ */
    { None }
| ASSIGNS BSNOTHING SEMICOLON
    { Some (loc_i 2,[]) }
| ASSIGNS expr_comma_list SEMICOLON
    { Some (loc_i 2,$2) }
;

decreases_opt:
| /* $\varepsilon$ */
    { None }
| DECREASES expr SEMICOLON 
    { Some $2 }
;

kml_statement_annot:
| LOOP_INVARIANT expr SEMICOLON beh_loop_inv loop_variant_opt EOF
    { JPSloop_annot($2,$4,$5) }
| beh_loop_inv loop_variant EOF
    { JPSloop_annot({java_pexpr_node = JPElit (Bool true) ; 
		     java_pexpr_loc = loc_i 2 },$1,$2) }
| ASSERT expr SEMICOLON EOF
    { JPSassert(None,$2) }
| ASSERT ident COLON expr SEMICOLON EOF
    { JPSassert(Some $2,$4) }
| GHOST local_variable_declaration SEMICOLON EOF
    { JPSghost_local_decls($2) }
| GHOST expr SEMICOLON EOF
    { JPSghost_statement($2) }
| requires_opt decreases_opt assigns_opt ensures_opt behaviors EOF
    { JPSstatement_spec($1,$2,default_behavior $3 $4 $5) }
;

beh_loop_inv:
| /* $\varepsilon$ */
    { [] }
| FOR ident COLON LOOP_INVARIANT expr SEMICOLON beh_loop_inv
    { ($2,$5)::$7 }
;

loop_variant_opt:
| /* $\varepsilon$ */
    { None }
| loop_variant
    { $1 }
;

loop_variant:
| LOOP_VARIANT expr SEMICOLON 
    { Some $2 }
;


/*
Local Variables: 
compile-command: "make -j -C .. bin/krakatoa.byte"
End: 
*/
