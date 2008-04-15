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

%{
  open Mix_ast
  open Parsing

  let locate n = { node = n; loc = symbol_start_pos () }
%}

/* Tokens */ 

%token <string> IDENT
%token <string> LABEL
%token <string> VERBATIM
%token <string> INVARIANT
%token <string> ASSERT
%token <string> INTEGER
%token <string> STRING
%token <Mix_ast.instr> INSTR
%token COLON COMMA LPAR RPAR PLUS STAR MINUS
%token EQU ORIG
%token EOF

/* Precedences */

%left PLUS MINUS
%nonassoc uminus

/* Entry points */

%type <Mix_ast.pfile> file
%start file

%%

file:
| list0_pseudo list1_stmt EOF 
   { List.rev $1, List.rev $2 }
| EOF 
   { [], [] }
;

list0_pseudo:
| /* epsilon */ { [] }
| list0_pseudo pseudo { $2 :: $1 }
;

pseudo:
| IDENT EQU address  { locate (Equ_addr ($1, $3)) }
| IDENT EQU INTEGER COLON INTEGER { locate (Equ_field ($1, PFrange ($3, $5))) }
| ORIG address       { locate (Orig (None, $2)) }
| IDENT ORIG address { locate (Orig (Some $1, $3)) } 
| VERBATIM           { locate (Verbatim $1) }
;

list1_stmt:
| stmt
   { [$1] }
| list1_stmt stmt
   { $2 :: $1 }
;

stmt:
| opt_label stmt_kind { $1, $2 }
;

stmt_kind:
| INVARIANT { locate (PSinvariant $1) }
| ASSERT { locate (PSassert $1) }
| INSTR operand { locate (PSinstr ($1, $2)) }
;

opt_label:
| LABEL         { Some $1 }
| /* epsilon */ { None }
;

operand:
  address_opt index_opt field_opt 
    { { pop_address = $1; pop_index = $2; pop_field = $3 } }
;

address_opt:
| address { Some $1 }
| /* epsilon */  { None }
;

address:
| STAR { PAself }
| IDENT { PAident $1 }
| INTEGER { PAconst $1 }
| address PLUS address { PAplus ($1, $3) }
| address MINUS address { PAminus ($1, $3) }
| MINUS address %prec uminus { PAuminus $2 }
;

index_opt:
| COMMA INTEGER { Some $2 }
| /* epsilon */ { None }
;

field_opt:
| LPAR INTEGER COLON INTEGER RPAR { Some (PFrange ($2, $4)) }
| LPAR IDENT RPAR { Some (PFident $2) }
| /* epsilon */ { None }
;

