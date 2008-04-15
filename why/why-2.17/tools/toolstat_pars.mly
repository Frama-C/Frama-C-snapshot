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

/* $Id: toolstat_pars.mly,v 1.10 2008/11/25 12:44:22 moy Exp $ */

%{
  open Format
  open Parsing
  open Toolstat_types
    
  let default_prover = ""
  let default_test = let count = ref 0 in function () ->
    incr count; "Unknown" ^ (string_of_int !count)
  let default_summary = (0,0,0,0,0)
  let default_detail = ([],[],[],[],[])
  let default_time = (0,0,0.)
  let div_time (h,m,s) n =
    if n = 0 then (h,m,s) else
      let s = float_of_int (3600 * h + 60 * m) +. s in
      let s = s /. float_of_int n in
      let h = int_of_float s / 3600 in
      let m = int_of_float s / 60 - 60 * h in
      let s = s -. float_of_int (3600 * h + 60 * m) in
      (h,m,s)
  let default_annot = (0,0,0,0)
%}

%token < string > PROJECT
%token < Toolstat_types.prover > PROVER
%token < Toolstat_types.test > TEST
%token < Toolstat_types.result > RESULT
%token < Toolstat_types.time > TIME 

%token PRE POST LOOPINV BWD_LOOPINV RELATION

%token EOF
%type < unit > all
%type < Toolstat_types.annotation list * Toolstat_types.record list > log
%start log all

%%

all:
| any all { }
| EOF { }
;

any: 
| PROJECT { }
| PROVER { }
| TEST { }
| RESULT { }
| TIME { }
| PRE { }
| POST { }
| LOOPINV { }
| BWD_LOOPINV { }
;

log: 
| project_record_list
    { $1 }
| subrecord_list
    { [], $1 }
| EOF 
    { [],[] }
;

project_record_list:
| project_record project_record_list 
    { 
      let annot1,rec1 = $1 in
      let annot2,rec2 = $2 in
      annot1 @ annot2, rec1 @ rec2
    }
| project_record
    { $1 }
;

project_record:
| PROJECT annot_list subrecord_list
    { ([ ($1,$2) ],
       List.map (fun (completed,project,prover,test,summary,detail,time) ->
		   let test = $1 ^ ":" ^ test in
		   (completed,Some $1,prover,test,summary,detail,time)
		) $3)
    }
| PROJECT annot_list
    { (* Error case *)
      ([ ($1,$2) ],
       [ (false,Some $1,default_prover,$1,default_summary,
	  default_detail,default_time) ])
    }
;

annot_list:
| annot annot_list 
    { 
      let (pre1,post1,inv1,bwd1) = $1 in
      let (pre2,post2,inv2,bwd2) = $2 in
      (pre1+pre2,post1+post2,inv1+inv2,bwd1+bwd2)
    }
|   { default_annot }
	

annot:
| PRE count
    { ($2,0,0,0) }
| POST count
    { (0,$2,0,0) }
| LOOPINV count
    { (0,0,$2,0) }
| BWD_LOOPINV count
    { (0,0,0,$2) }
;

count:
| RELATION count 
    { 1 + $2 }
|   { 0 }
;

subrecord_list:
| subrecord subrecord_list 
    { $1 @ $2 }
| subrecord  
    { $1 }
;

subrecord:
| PROVER tests TIME
    { let n = List.length $2 in
      List.map (fun (test,result) ->
		  let summary,detail = result in
		  (true,None,$1,test,summary,detail,div_time $3 n)
	       ) $2 }
| PROVER TEST RESULT
    { (* Case for no VC *)
      let summary,detail = $3 in [ (true,None,$1,$2,summary,detail,default_time) ] }
| PROVER TEST
    { (* Error case *)
      [ (false,None,$1,$2,default_summary,default_detail,default_time) ] }
| PROVER
    { (* Error case *)
      [ (false,None,$1,default_test (),default_summary,default_detail,default_time) ] }
;

tests:
| TEST RESULT tests
    { ($1,$2) :: $3 }
|   { [] }
;
