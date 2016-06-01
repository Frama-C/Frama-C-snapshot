/**************************************************************************/
/*                                                                        */
/*  This file is part of Aorai plug-in of Frama-C.                        */
/*                                                                        */
/*  Copyright (C) 2007-2016                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
/*    INSA  (Institut National des Sciences Appliquees)                   */
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

/* $Id: promelaparser.mly,v 1.2 2008-10-02 13:33:29 uid588 Exp $ */

/* Originated from http://www.ltl2dstar.de/down/ltl2dstar-0.4.2.zip  */
%{
open Promelaast
open Bool3

let observed_states=Hashtbl.create 1

let to_seq c = 
  [{ condition = Some c; nested = [];
    min_rep = Some (PCst (Logic_ptree.IntConstant "1"));
    max_rep = Some (PCst (Logic_ptree.IntConstant "1"));
   }]

%}



%token PROMELA_OR
%token PROMELA_AND
%token PROMELA_NOT PROMELA_TRUE PROMELA_FALSE

%right PROMELA_OR
%right PROMELA_AND
%nonassoc PROMELA_NOT PROMELA_TRUE PROMELA_FALSE
 
%token PROMELA_NEVER PROMELA_IF PROMELA_FI PROMELA_GOTO PROMELA_SKIP
%token <string> PROMELA_LABEL
%token PROMELA_COLON PROMELA_SEMICOLON PROMELA_DOUBLE_COLON 
%token PROMELA_LBRACE PROMELA_RBRACE PROMELA_LPAREN
%token PROMELA_RPAREN PROMELA_RIGHT_ARROW
%token PROMELA_TRUE PROMELA_FALSE

%token <string> PROMELA_CALLOF  PROMELA_RETURNOF  PROMELA_CALLORRETURNOF
%token EOF


%type <Promelaast.parsed_automaton> promela
%start promela
%%

promela
        : PROMELA_NEVER PROMELA_LBRACE states PROMELA_RBRACE EOF { 
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
            (states , $3)
        }
        | PROMELA_NEVER PROMELA_LBRACE states PROMELA_SEMICOLON 
            PROMELA_RBRACE EOF {
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
              (states , $3) }
  ;

states   
        : states PROMELA_SEMICOLON state { 
            $1@$3
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
                    let s = Data_for_aorai.new_state $1 in
                    Hashtbl.add observed_states $1 s;
                    s
            in
            (* Step 1 : setting up the acceptance status *)
            (* Default status : Non acceptation state *)
            old.acceptation <- False;
            
            (* Accept_all state means acceptance state with a 
               reflexive transition without cross condition *)
            (* This case is not exclusive with the following. 
               Acceptation status is set in this last. *)
            if (String.length $1>=10) && 
              (String.compare (String.sub $1 0 10) "accept_all")=0 
            then 
              trans:=
                {start=old;stop=old;cross=Seq (to_seq PTrue);numt=(-1)} ::
                !trans;
            (* If the name includes accept then 
               this state is an acceptation one. *)
            if (String.length $1>=7) && 
              (String.compare (String.sub $1 0 7) "accept_")=0 
            then
              old.acceptation <- True;

            (* Step 2 : setting up the init status *)
            (* If the state name ended with "_init" then 
               it is an initial state. Else, it is not. *)
            if (String.length $1>=5) && 
              (String.compare (String.sub $1 ((String.length $1)-5) 5) 
                 "_init" ) = 0
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
                  let r = Data_for_aorai.new_state $5 in
                  Hashtbl.add observed_states $5 r;
                  r
          in
          ($2,s)
        }
        ;

guard
        : PROMELA_CALLORRETURNOF { POr(PCall ($1,None), PReturn $1) } 
        | PROMELA_CALLOF { PCall ($1,None) }
        | PROMELA_RETURNOF { PReturn $1 }
        | PROMELA_TRUE { PTrue }
        | PROMELA_FALSE { PFalse }
        | PROMELA_NOT guard { PNot $2 }
        | guard PROMELA_AND guard { PAnd ($1,$3) }
        | guard PROMELA_OR guard { POr ($1,$3) }
        | PROMELA_LPAREN guard PROMELA_RPAREN { $2 }
        | PROMELA_LABEL 
            { PRel (Logic_ptree.Neq,PVar $1,PCst(Logic_ptree.IntConstant "0")) }
   ;
