(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(**************************************************************************)

(* $Id: abstract_ai.ml,v 1.2 2008/10/02 13:33:29 uid588 Exp $ *)

open Cil_types
open Cil
open Cilutil
open Ast_info



(** Global information on functions that are collected during each pass. These 
    information are furthermore used torestrict pre or post-condition of 
    fonctions according to there scope of use.
*)
let functions_pre_usecase  = Hashtbl.create 97  
let functions_post_usecase = Hashtbl.create 97 

(* Mark to ensures the completion of functions specification computation *)
let spec_modified = ref false 


(** 
  This visitor requires that each function has a specification. It then 
  computes a finer specification by forward and backard abstract interpretation
  on states.

  This vistor use mainly 2 sub-functions (propagates_pre and propagates_post) 
  that implement respectively forward and backward treatment.
*)
class visit_propagating_pre_post_constraints (auto:Promelaast.buchautomata)  =
  (***************************************************************************)
  (* For the two pass                                                        *) 
  (*                                                                         *)
  (** Associates each statement of the fonction to a pre/post specification  *) 
  let stmts_spec = Hashtbl.create 97  in
  (*                                                                         *)
  (***************************************************************************)
  (* For the pre-condition propagation pass                                  *) 
  (*                                                                         *)
  (** During the pre-condition propagation, it represents the set of 
      statements that need second computation of specification. For instance, 
      it can occurs when the statement is pointed by a goto instruction. *)
  let stmts_to_compute_one_more_time = Hashtbl.create 97  in
  (*                                                                         *)
  (** This variable contains the result of the pre-condition propagation.    *)
  let propagation_result = ref (Ltl_utils.mk_empty_pre_or_post ()) in
  (*                                                                         *)
  (***************************************************************************)
  (* For the post-condition propagation pass                                 *) 
  (*                                                                         *)
  (** Set of observed labeled statement. This information is used for goto 
      treatment.                                                             *)
  let status_of_labeled_stmts = Hashtbl.create 97 in 
  (*                                                                         *)
  (** True if and only if the computation has to be done again in oreder to 
      compute a fix-point result. *)
  let second_computation_needed = ref false in 
  (*                                                                         *)
  (***************************************************************************)




(** Propagates pre-condition to each statement, by following control flow. 
    It returns a couple af bool array, definig the strongest post-condition of the statement list. *)
  let rec propagates_pre stmt_l (pre_st,pre_tr) =

    (** This function returns the curent pre of a statement or an empty 
	pre if no specification exists *)
    let get_pre_of stmt_ref =
      try
	let pre_st,pre_tr,_,_ = Hashtbl.find stmts_spec stmt_ref in 
	pre_st,pre_tr
      with 
	| Not_found -> Ltl_utils.mk_empty_pre_or_post()
    in



    (** This function makes an OR filter between the given pre and the old pre of the given stmt 
        The result is storing as the new pre of the given stmt. *)
    let update_stmt_pre stmt_ref pre =
      try 
	let old_pre_st,old_pre_tr,post_st,post_tr = Hashtbl.find stmts_spec stmt_ref in
	let n_pre_st,n_pre_tr = Ltl_utils.double_bool_array_or (old_pre_st,old_pre_tr) pre in
	Hashtbl.replace stmts_spec stmt_ref (n_pre_st,n_pre_tr,post_st,post_tr)
      with 
	| _ -> 
	    let n_pre_st,n_pre_tr = pre in
	    Hashtbl.replace stmts_spec stmt_ref (n_pre_st,n_pre_tr,n_pre_st,n_pre_tr) 
    in



    (** This function returns the current pre of the given statement.
	WARNING !
	Side effects of this function : 
	  * If the statement is in stmts_to_compute_one_more_time then it is removed
	  * The pre of the current stmt is updated according to the current pre_st and pre_tr
    *)
    let get_stmts_pre stmt_ref =
      (* If this statement is annotated to be computed again then we remove its annotation. *)
      Hashtbl.remove stmts_to_compute_one_more_time stmt_ref;

      (* Registering the new specification. *)
      update_stmt_pre stmt_ref (pre_st,pre_tr);
      let pre = get_pre_of stmt_ref in
      pre 
    in



    match stmt_l with
      | [] ->  
	  (pre_st,pre_tr)


      | ({skind=Instr(Call(_,Lval(Var(vi),_),_,_))} as stmt)::l ->
	  (* Updating pre-condition with previous information *)
	  let pre_st,pre_tr = get_stmts_pre (ref stmt) in

	  (* If the statement is unreachable then we skip the call *)
	  if (Ltl_utils.double_bool_array_eq (pre_st,pre_tr) (Ltl_utils.mk_empty_pre_or_post())) then
  	    propagates_pre l (pre_st,pre_tr)
	  else
	    begin
	
	      (* Registering call context for future reinforcement of pre-condition *)
	      let (pre_call_st,pre_call_tr)=Ltl_utils.get_next vi.vname Promelaast.Call pre_st in

	      let (cur_pre_st,cur_pre_tr) = 
		if Hashtbl.mem functions_pre_usecase vi.vname 
		then Ltl_utils.double_bool_array_or (Hashtbl.find functions_pre_usecase vi.vname) (pre_call_st,pre_call_tr) 
		else (pre_call_st,pre_call_tr)
	      in 
	      Hashtbl.replace functions_pre_usecase vi.vname (cur_pre_st,cur_pre_tr);
	      

              (* From now, pre-condition is the set of configurations from which
		 the operation is callable according to its post-condition. *)
	      let (post_call_st,post_call_tr) = (Data_for_ltl.get_func_post vi.vname) in 

	      (* Add the specification of the current stmt in the hashtbl. *)
	      (* Note that we use the pre of the statement and not the one computed for the called function. *)
	      Hashtbl.replace stmts_spec (ref stmt) (pre_st,pre_tr,post_call_st,post_call_tr);

	      (* Computes next statements specification *)
  	      propagates_pre l (post_call_st,post_call_tr)
	    end



      | {skind=Instr(Call(_,_,_,_))}::_ -> 
	  Format.printf "Ltl_to_acsl plugin internal error. Status : Operation calls has to be done by explicit operation name\n";
	  assert false  



      | ({skind=Instr (_)} as stmt)::l -> 
	  (* Updating pre-condition with previous information *)
	  let pre_st,pre_tr = get_stmts_pre (ref stmt) in

	  (* Add the specification of the current stmt in the hashtbl. *)
  	  Hashtbl.replace stmts_spec (ref stmt) (pre_st,pre_tr,pre_st,pre_tr);
	  
	  (* Computes next statements specification *)
	  propagates_pre l (pre_st,pre_tr)




      | ({skind=Block(b)} as stmt)::l ->  
	  (* Updating pre-condition with previous information *)
	  let pre_st,pre_tr = get_stmts_pre (ref stmt) in

	  (* Propagation into block *)
	  let (post_st,post_tr) = (propagates_pre b.bstmts (pre_st,pre_tr)) in
	    
	  (* Add the specification of the current stmt in the hashtbl. *)
	  Hashtbl.replace stmts_spec (ref stmt) (pre_st,pre_tr,post_st,post_tr);
	  
	  (* Computes next statements specification *)
  	  propagates_pre l (post_st,post_tr)



      | ({skind=If(_,b1,b2,_)} as stmt)::l -> 
	  (* Updating pre-condition with previous information *)
	  let pre_st,pre_tr = get_stmts_pre (ref stmt) in

	  (* Constraints propagation into branches. *)
	  let post_block1 = propagates_pre b1.bstmts (pre_st,pre_tr) in 
	  let post_block2 = propagates_pre b2.bstmts (pre_st,pre_tr) in 
	  
	  (* The new post-condition is the disjunction of branches post-conditions *)
	  let (post_st,post_tr) = Ltl_utils.double_bool_array_or post_block1 post_block2 in 

	  (* Add the specification of the current stmt in the hashtbl. *)
  	  Hashtbl.add stmts_spec (ref stmt) (pre_st,pre_tr,post_st,post_tr);
	  
	  (* Computes next statements specification *)
	  propagates_pre l  (post_st,post_tr)



      | ({skind=Return (_,_)} as stmt)::l -> 
	  (* Updating pre-condition and current result with previous information *)
	  let pre_st,pre_tr = get_stmts_pre (ref stmt) in
	  propagation_result:= (pre_st,pre_tr);

	  (* Add the specification of the current stmt in the hashtbl. *)
  	  Hashtbl.replace stmts_spec (ref stmt) (pre_st,pre_tr,pre_st,pre_tr);
	  
	  (* A pre-treatment of frama-C has to put the return statement at the 
	     end of the function. *)
	  if l<>[] then assert false;

	  (* Return the post-condition of the current function *)
	  !propagation_result
	    


      | ({skind=Goto(stmt_ref,_)} as stmt)::stmt_l -> 

	  (* Updating pre-condition with previous information *)
	  let pre_st,pre_tr = get_stmts_pre (ref stmt) in

	  (* Add the specification of the current stmt in the hashtbl. *)
  	  Hashtbl.replace stmts_spec (ref stmt) (pre_st,pre_tr,pre_st,pre_tr);
	  
	  (* Modifing specification of pointed statement and registering it to be computed *)
	  (* If the statement has not yet been specified *)
	  let ref_pre_st,ref_pre_tr = get_pre_of (stmt_ref) in
	  
	  if not (Ltl_utils.double_bool_array_eq (ref_pre_st,ref_pre_tr) (pre_st,pre_tr)) then 
	    begin
	      (* Updating pre-condition of pointed statement *)
	      update_stmt_pre stmt_ref (pre_st,pre_tr);
	      Hashtbl.replace stmts_to_compute_one_more_time stmt_ref true;
	    end;

	  (* In order to treat statements that are not directly reachable, 
	     consumes following statements until a labeled one with a defined pre-condition. *)
	  (*consumes stmt_l *)
	  let _ = propagates_pre stmt_l (Ltl_utils.mk_empty_pre_or_post ()) in
	  (Ltl_utils.mk_empty_pre_or_post ())




      | ({skind=Loop (_,block,_,_,_)} as stmt)::stmt_l -> 
	  (* In a loop we distinguishe 4 cases of pre or post conditions:
	     {pre1}
	     While (1) {
	     {Pre2}
	     ...
	     if (c) {goto Label_end_loop;}
	     ...
	     {Post2}
	     }
	     {Post1}
	     Label_end_loop:
	     

	     Pre1  : pre-condition before entering the loop
	     Pre2  : pre-condition of each iteration
	     Post1 : False (infinite loop)
	     Post2 : Post condition of an iteration

	     
	     Computation of conditions : 

	     Initially : 
 	       Pre1 is given
	       Pre2 = Pre1

	     do 
	       Post2 = (Pre2[block])
  	       Pre2 = Pre2 \/ Post2
	     while fix-point not reached.

	     Finally, the loop invariant is: 
	       (Init => Pre1)
	     & (not Init => Pre2)
	     (where init is a fresh variable to indicate if the iteration is the first one).

	  *)
	  
	  (* Updating pre-conditions with previous information *)
	  let loop_pre = get_stmts_pre (ref stmt) in
	  let block_pre = loop_pre in 

	  

	  (* First forward propagation into block *)
	  let old_post = ref block_pre in
	  let block_post = ref( propagates_pre block.bstmts block_pre ) in
	  let block_pre  = ref( Ltl_utils.double_bool_array_or block_pre !block_post ) in

	  (* Fix-point computation *)
	  while not (Ltl_utils.double_bool_array_eq !old_post !block_post) do

	    old_post := !block_post;
	    block_post:=propagates_pre block.bstmts !block_pre;
	    block_pre :=Ltl_utils.double_bool_array_or !block_pre !block_post
	      
	  done;
	  
	  (* Finally : Post1  = Pre2 *)
	  let (loop_post_st,loop_post_tr) = !block_pre in (* INTUILE *)
	  

	  (* Updating loop information *)
          Data_for_ltl.set_loop_ext_pre  (ref stmt) loop_pre;
	  Data_for_ltl.set_loop_ext_post (ref stmt) (loop_post_st,loop_post_tr);
	  Data_for_ltl.set_loop_int_pre  (ref stmt) !block_pre;
	  Data_for_ltl.set_loop_int_post (ref stmt) !block_post;
      	  Hashtbl.replace stmts_spec (ref stmt) (fst loop_pre, snd loop_pre,loop_post_st,loop_post_tr);
   


	  (* Computes next statements specification *)
	  (* The end of the loop is done through a goto instruction that 
	     does not appear in the CIL structure. This is why, the 
	     post-condition is the exit case of the loop invariant. *)
          propagates_pre stmt_l (loop_post_st,loop_post_tr);




      | {skind=Switch (_,_,_,_)}::_  -> 
	  Format.printf "Ltl_to_acsl plugin internal Todo (Switch) \n";
	  assert false

      | {skind=Break (_)}::_
      | {skind=Continue (_)}::_ -> 
	  Format.printf "Ltl_to_acsl plugin internal error. Continue and Break statements have to be rewritten into goto by the CFG pass.\n";
	  assert false

      | {skind=UnspecifiedSequence(_)}::_
      | {skind=TryFinally (_,_,_) }::_
      | {skind=TryExcept(_,_,_,_)}::_ -> 
	  Format.printf "Ltl_to_acsl plugin internal error. Status : UnspecifiedSequence and try constructions are not yet supported.\n";
	  assert false 




(** Propagates post-condition to each statement, by following control flow. 
    It returns a couple of bool array, definig the weakest pre-condition of the statement list. 
    Since then analysis is a backward one, the list is first reversed. *)
	  
  in let rec propagates_post stmt_l (post_st,post_tr) =

    (** This function returns the current spec of a statement or an empty 
	spec if no specification exists *)
    let get_spec_of stmt_ref =
      try let spec = Hashtbl.find stmts_spec stmt_ref in spec
      with Not_found -> Ltl_utils.mk_empty_spec()
    in


    (** This function makes an AND filter between the given post and the old post of the given stmt 
        The result is storing as the new post of the given stmt. *)
    let update_stmt_post stmt_ref (post_st,post_tr) =
      let old_pre_st,old_pre_tr,old_post_st,old_post_tr = get_spec_of stmt_ref in
      let new_post_st,new_post_tr = (Ltl_utils.double_bool_array_and (old_post_st,old_post_tr) (post_st,post_tr)) in
      Hashtbl.replace 
	stmts_spec
	stmt_ref
	(old_pre_st,old_pre_tr,new_post_st,new_post_tr)
    in



    (** This function returns the current spec of the given statement.
	WARNING !
	Side effects of this function : 
	  * The post of the current stmt is updated according to the given post_st and post_tr
    *)
    let get_stmts_spec stmt_ref post_st post_tr =
      update_stmt_post stmt_ref (post_st,post_tr);
      get_spec_of stmt_ref
    in



    (** Body of propagates_post (after list.rev) *)
    let rec prop  stmt_l (post_st,post_tr) =  

      if stmt_l <>[] then
	begin
	  let s = (List.hd stmt_l) in
	  if s.labels<>[] then 
	    Hashtbl.replace status_of_labeled_stmts (ref s) true
	end;
      

      match stmt_l with  
	| [] -> (post_st,post_tr) 


	| ({skind=Instr(Call(_,Lval(Var(vi),_),_,_))} as stmt)::l ->
	    (* Retriving old specification information about this statement *)
	    let (pre_st,pre_tr,post_st,post_tr) = get_stmts_spec (ref stmt) post_st post_tr in
	    
	    (* If statement is unreachable then we skip the call *)
	    if (Ltl_utils.double_bool_array_eq (pre_st,pre_tr) (Ltl_utils.mk_empty_pre_or_post())) then
  	      prop l (pre_st,pre_tr)
	    else
	      begin
 		(* Registering call context for future reinforcement of post-condition *)
		let (cur_post_st,cur_post_tr) = 
		  if Hashtbl.mem functions_post_usecase vi.vname 
		  then Ltl_utils.double_bool_array_or (Hashtbl.find functions_post_usecase vi.vname) (post_st,post_tr) 
		  else (post_st,post_tr)
		in Hashtbl.replace functions_post_usecase vi.vname (cur_post_st,cur_post_tr);
		
		(* From now, post-condition is the set of configurations from which  
		   the operation is callable according to its pre-condition and 
		   of the current statement pre-condition. *)
		let pre_call = (Data_for_ltl.get_func_pre vi.vname) in   
		let cur_pre = (Ltl_utils.get_prev vi.vname Promelaast.Call pre_call) in
		let (cur_pre_st,cur_pre_tr) = Ltl_utils.double_bool_array_and cur_pre (pre_st,pre_tr) in
		
		(* Updating the specification of the current stmt in the hashtbl. *)
		(* Note that we use the post of the statement and not the one computed for the called function. *)
		Hashtbl.replace stmts_spec (ref stmt) (cur_pre_st,cur_pre_tr,post_st,post_tr);
		
		(* Computes next statements specification *)
		prop l  (cur_pre_st,cur_pre_tr)
	      end
		


      | {skind=Instr(Call(_,_,_,_))}::_ -> 
	  Format.printf "Ltl_to_acsl plugin internal error. Status : Operation calls has to be done by explicit operation name\n";
	  assert false  

      | ({skind=Instr (_)} as stmt)::l -> 
	  (* Retriving old specification information about this statement *)
	  let (pre_st,pre_tr,post_st,post_tr) = get_stmts_spec (ref stmt) post_st post_tr in

	  (* Updating the specification of the current stmt in the hashtbl. *)
  	  Hashtbl.replace stmts_spec (ref stmt)(pre_st,pre_tr,post_st,post_tr) ;
	  
	  (* Computes next statements specification *)
	  prop l (post_st,post_tr)



      | ({skind=Block(b)} as stmt)::l ->  
	  (* Retriving old specification information about this statement *)
	  let (pre_st,pre_tr,post_st,post_tr) = get_stmts_spec (ref stmt) post_st post_tr in
	  
	  (* Computes recursivly the block specification *)
	  let cur_pre = (propagates_post b.bstmts (post_st,post_tr)) in
	  let (pre_st,pre_tr) = Ltl_utils.double_bool_array_and cur_pre (pre_st,pre_tr) in
	    
	  (* Updating the specification of the current stmt in the hashtbl. *)
	  Hashtbl.replace stmts_spec (ref stmt) (pre_st,pre_tr,post_st,post_tr);
	  
	  (* Computes next statements specification *)
  	  prop l (pre_st,pre_tr)




      | ({skind=If(_,b1,b2,_)} as stmt)::l -> 
	  (* Retriving old specification information about this statement *)
	  let (pre_st,pre_tr,post_st,post_tr) = get_stmts_spec (ref stmt) post_st post_tr in


	  (* Constraints propagation into branches. *)
	  let pre_block1 = propagates_post b1.bstmts (post_st,post_tr) in 
	  let pre_block2 = propagates_post b2.bstmts (post_st,post_tr) in 
	  

	  (* The new pre-condition is the disjunction of branches pre-conditions *)
	  let pre_blocks = Ltl_utils.double_bool_array_or pre_block1 pre_block2 in
	  let (pre_st,pre_tr) = Ltl_utils.double_bool_array_and pre_blocks (pre_st,pre_tr) in

	  
	  (* Updating the specification of the current stmt in the hashtbl. *)
  	  Hashtbl.replace stmts_spec (ref stmt) (pre_st,pre_tr,post_st,post_tr);
	  
	  (* Computes next statements specification *)
	  prop l  (pre_st,pre_tr)




      | ({skind=Return (_,_)} as stmt)::l -> 
	  (* Retriving old specification information about this statement *)
	  let (pre_st,pre_tr,post_st,post_tr) = get_stmts_spec (ref stmt) post_st post_tr in

	  (* The 'prev' according to the return will be done be the caller of the propagates function. *)
	  (* Updating the specification of the current stmt in the hashtbl. *)
  	  Hashtbl.replace stmts_spec (ref stmt) (pre_st,pre_tr,post_st,post_tr);
	  
	  (* Return the post-condition of the current function *)
	  prop l (post_st,post_tr)
	  
	    


      | ({skind=Goto(stmt_ref,_)} as stmt)::stmt_l -> 
	  (* Retriving old specification information about this statement and the pointed one. *)
	  let (ref_pre_st,ref_pre_tr,_,_) = get_spec_of stmt_ref in
	  let (_,_,post_st,post_tr) = get_spec_of (ref stmt) in

	  (* Second computation needed if the pointed stmt has not yet been treated 
	     or if its pre differs from the current post *)
	  if (not !second_computation_needed) 
	    && (Hashtbl.mem status_of_labeled_stmts (ref stmt) )
	    && (not (Ltl_utils.double_bool_array_eq (ref_pre_st,ref_pre_tr) (post_st,post_tr)))
	  then
	    second_computation_needed:=not (Hashtbl.find status_of_labeled_stmts (ref stmt));


	  (* Current post_st and post_tr are lose because they have no sense in the case of a goto instruction. *)
	  let (pre_st,pre_tr,post_st,post_tr) = get_stmts_spec (ref stmt) ref_pre_st ref_pre_tr in

	  (* Add the specification of the current stmt in the hashtbl. *)
  	  Hashtbl.add stmts_spec (ref stmt) (pre_st,pre_tr,post_st,post_tr);

	  prop stmt_l (pre_st,pre_tr)



      | ({skind=Loop (_,block,_,_,_)} as stmt)::stmt_l -> 
	  (* In a loop we distinguishe 4 cases of pre or post conditions:
	     {pre1}
	     While (1) {
	     {Pre2}
	     ...
	     if (c) {goto Label_end_loop;}
	     ...
	     {Post2}
	     }
	     {Post1}
	     Label_end_loop:


	     Pre1  : pre-condition before entering the loop
	     Pre2  : pre-condition of each iteration
	     Post1 : False (Infinite loop)
	     Post2 : Post condition of an iteration

	     
	     Initially : 
	     Since the forward AI is done, an initial value is known for each annotation.
	       
	     do 
  	       Pre2 = ([block]Post2) /\ Pre2
	       Post2 = Pre2 /\ Post2
	     while fix-point not reached.
	     
	     Finally :
	       Pre1  = Pre1 /\ Pre2


	     The loop invariant is then : 
	        (c  => Pre2)
	      & (!c => Post1)
	      & (Init => Pre1)
	      & (not Init => Post2)
	     (where init is a fresh variable to indicate if the iteration is the first one).
	  *)

	  (* Retriving old specification information about this statement *)
	  let (loop_pre_st,loop_pre_tr,loop_post_st,loop_post_tr) = get_stmts_spec (ref stmt) post_st post_tr in
	  let loop_pre   = loop_pre_st ,loop_pre_tr  in
	  let loop_post  = loop_post_st,loop_post_tr in
	  let block_pre  = Data_for_ltl.get_loop_int_pre  (ref stmt) in
	  let block_post = Data_for_ltl.get_loop_int_post (ref stmt) in



          (* First backward propagation into block 
  	       Pre2 = ([block]Post2) /\ Pre2
	       Post2 = Pre2 /\ Post2
	  *)
	  let old_pre = ref block_pre in
	  let block_pre  = propagates_post block.bstmts block_post in
          let block_pre  = Ltl_utils.double_bool_array_and block_pre !old_pre in
          let block_post = Ltl_utils.double_bool_array_and block_pre block_post in


	  (* Loop initialisation for fix-point computation *)
	  let block_pre = ref block_pre in
	  let block_post = ref block_post in 

	  while not (Ltl_utils.double_bool_array_eq !old_pre !block_pre) do

	    old_pre := !block_pre ;
	    block_pre  := propagates_post block.bstmts !block_post ;
            block_pre  := Ltl_utils.double_bool_array_and !block_pre !old_pre ;
	    block_post := Ltl_utils.double_bool_array_and !block_pre !block_post 

	  done;

	  (* The result is dereferenced *)
	  let block_pre = !block_pre in	  
	  let block_post = !block_post in 
    

	  (* Finally : Pre1  = Pre1 /\ Pre2 *)
	  let loop_pre = Ltl_utils.double_bool_array_and loop_pre block_pre in



	  (* Updating loop information *)
          Data_for_ltl.set_loop_ext_pre  (ref stmt) loop_pre;
	  Data_for_ltl.set_loop_ext_post (ref stmt) loop_post;
	  Data_for_ltl.set_loop_int_pre  (ref stmt) block_pre;
	  Data_for_ltl.set_loop_int_post (ref stmt) block_post;
	  Hashtbl.replace stmts_spec (ref stmt) (fst loop_pre, snd loop_pre,fst loop_post, snd block_post);

(*
    Format.printf "\n\nNew loop pre : \n";
    Ltl_utils.debug_display_stmt_all_pre loop_pre ;
    Format.printf "\nNew loop post : \n";
    Ltl_utils.debug_display_stmt_all_pre loop_post;
    Format.printf "\n\nNew loop int_pre : \n";
    Ltl_utils.debug_display_stmt_all_pre block_pre ;
    Format.printf "\nNew loop int_post : \n";
    Ltl_utils.debug_display_stmt_all_pre block_post;
    Format.printf "\n\n";
*)
	  
	  (* Computes next statements specification *)
  	  prop stmt_l loop_pre




      | {skind=Switch (_,_,_,_)}::_ -> 
	  Format.printf "Ltl_to_acsl plugin internal Todo (Swith or loop instruction) \n";
	  assert false




      | {skind=Break (_)}::_
      | {skind=Continue (_)}::_ -> 
	  Format.printf "Ltl_to_acsl plugin internal error. Status : Goto, Break and Continue instructions are not yet supported.\n";
	  assert false 
      | {skind=UnspecifiedSequence(_)}::_
      | {skind=TryFinally (_,_,_) }::_
      | {skind=TryExcept(_,_,_,_)}::_ -> 
	  Format.printf "Ltl_to_acsl plugin internal error. Status : UnspecifiedSequence and try constructions are not yet supported.\n";
	  assert false 


    in 
    (* This computation is done from end to begining *)
    prop (List.rev stmt_l) (post_st,post_tr)  
  in  



object (*(self) *)

  inherit Visitor.generic_frama_c_visitor  
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    let starting_pre  = (Data_for_ltl.get_func_pre f.svar.vname) in 
    let starting_post = (Data_for_ltl.get_func_post f.svar.vname) in 

(*    Format.printf "\nAvant passe 1       : ";
    Ltl_utils.debug_display_func_status f.svar.vname;
*)

    Hashtbl.clear stmts_spec;
    Hashtbl.clear stmts_to_compute_one_more_time;
    propagation_result := (Ltl_utils.mk_empty_pre_or_post ());

    (* Pre-condition forward propagation *) 
    let cur_pre = (Data_for_ltl.get_func_pre f.svar.vname) in
    let _ = propagates_pre f.sbody.bstmts cur_pre in
    let cur_post_st = ref (fst !propagation_result) in
    while (Hashtbl.length stmts_to_compute_one_more_time) > 0 do
      let _ = propagates_pre f.sbody.bstmts cur_pre in
      cur_post_st := fst (!propagation_result)
    done; 
 

    (* Registration of the new post-condition *)
    let post     = Ltl_utils.get_next f.svar.vname Promelaast.Return !cur_post_st in
    let old_post = (Data_for_ltl.get_func_post f.svar.vname)  in
    let post     = Ltl_utils.double_bool_array_and post old_post in
 
    Data_for_ltl.set_func_post f.svar.vname post;

(*    Format.printf "Entre passes 1 et 2 : ";
    Ltl_utils.debug_display_func_status f.svar.vname;
*)


    (* Post-condition backward propagation *) 
    let cur_post = (Data_for_ltl.get_func_post f.svar.vname) in
    let cur_post = Ltl_utils.get_prev f.svar.vname Promelaast.Return cur_post in

   
    Hashtbl.clear status_of_labeled_stmts;
    second_computation_needed:=false;
    let cur_pre  = ref (propagates_post f.sbody.bstmts cur_post) in

    while !second_computation_needed do
      Hashtbl.clear status_of_labeled_stmts;
      second_computation_needed:=false;
      cur_pre := propagates_post f.sbody.bstmts cur_post
    done;
    let cur_pre = !cur_pre in
    


    (* Registration of the new pre-condition *)
    let old_pre = Data_for_ltl.get_func_pre f.svar.vname in
    let pre     = Ltl_utils.double_bool_array_and cur_pre old_pre in
 
    Data_for_ltl.set_func_pre f.svar.vname pre;

(*    Format.printf "Apres passe 2       : ";
    Ltl_utils.debug_display_func_status f.svar.vname;
*)
 
    let ending_pre  = (Data_for_ltl.get_func_pre f.svar.vname) in 
    let ending_post = (Data_for_ltl.get_func_post f.svar.vname) in 
    
    if   (not (Ltl_utils.double_bool_array_eq starting_pre  ending_pre ) )
    then begin spec_modified:=true;  end;
    if   (not (Ltl_utils.double_bool_array_eq starting_post ending_post) )
    then begin spec_modified:=true;  end;
    
    DoChildren  
end 





let propagates_pre_post_constraints file root =
  Hashtbl.clear functions_pre_usecase ;
  Hashtbl.clear functions_post_usecase;
  spec_modified:=false;

  let visitor = new visit_propagating_pre_post_constraints (Data_for_ltl.getAutomata()) in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file;

  Globals.Functions.iter 
    (fun f -> 
       let name = (Kernel_function.get_name f) in
       if name <> root then 
	 begin
	   let old_pre  = (Data_for_ltl.get_func_pre name) in 
	   let old_post = (Data_for_ltl.get_func_post name) in 
	   let pre  = try Hashtbl.find functions_pre_usecase  name with Not_found -> (Ltl_utils.mk_empty_pre_or_post()) in
	   let post = try Hashtbl.find functions_post_usecase name with Not_found -> (Ltl_utils.mk_empty_pre_or_post()) in


	   let cur_pre  = Ltl_utils.double_bool_array_and pre  old_pre  in
	   let cur_post = Ltl_utils.double_bool_array_and post old_post in


(*
    Format.printf "\nPost of %s: \n  old    :" name;
    Ltl_utils.debug_display_stmt_all_pre old_post ;
    Format.printf              "\n  usecase:";
    Ltl_utils.debug_display_stmt_all_pre post;
    Format.printf              "\n  keeped :";
    Ltl_utils.debug_display_stmt_all_pre cur_post;
    Format.printf "\n\n";
*)  


	   if   (not (Ltl_utils.double_bool_array_eq old_pre  cur_pre ) )
	   then begin spec_modified:=true;  end;
	   if   (not (Ltl_utils.double_bool_array_eq old_post cur_post) )
	   then begin spec_modified:=true;  end;


	   Data_for_ltl.set_func_pre  name cur_pre;
	   Data_for_ltl.set_func_post name cur_post
	 end
    );
  !spec_modified



(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
