(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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
(*                                                                        *)
(**************************************************************************)

(* $Id: bycase_ai.ml,v 1.4 2008-12-19 15:30:56 uid588 Exp $ *)

open Cil_types
open Cil
open Cilutil
open Ast_info
open Spec_tools






let init_specification () =
  List.iter
    (fun name ->
       let pre,_ = Data_for_ltl.get_func_pre name in
       let post_st,post_tr = mk_empty_pre_or_post_bycase () in
       Array.iteri (fun index _ -> if pre.(index) then post_st.(index)<- fst (Data_for_ltl.get_func_post name) ) post_st;
       Array.iteri (fun index _ -> if pre.(index) then post_tr.(index)<- snd (Data_for_ltl.get_func_post name) ) post_tr;
       
       Data_for_ltl.set_func_post_bycase name (post_st,post_tr)
    )
    (Data_for_ltl.getFunctions_from_c ())







(** Global information on functions that are collected during each pass. These
    information are furthermore used torestrict pre or post-condition of
    fonctions according to there scope of use.
*)
let functions_pre_usecase : (string , (bool array array * bool array array)) Hashtbl.t = Hashtbl.create 97
let functions_post_usecase : (string , (bool array array * bool array array)) Hashtbl.t = Hashtbl.create 97
(* let functions_pre_usecase_Recursive : (string , (bool array array * bool array array)) Hashtbl.t  = Hashtbl.create 97 *)
(* let functions_post_usecase_Recursive : (string , (bool array array * bool array array)) Hashtbl.t   = Hashtbl.create 97 *)

(* Mark to ensures the completion of functions specification computation *)
let spec_modified = ref false





(**
  This visitor requires that each function has a specification. It then
  computes a finer specification by forward and backard abstract interpretation
  on states.

  This vistor use mainly 2 sub-functions (propagates_pre and propagates_post)
  that implement respectively forward and backward treatment.
*)
class visit_propagating_pre_post_constraints_bycase (auto:Promelaast.buchautomata)  =
  (***************************************************************************)
  (* For the two pass                                                        *)
  (*                                                                         *)
  (** Associates each statement of the fonction to a pre/post specification  *)
(*  let labelled_stmts_spec : (int, (bool array array * bool array array * bool array array * bool array array)) Hashtbl.t  = Hashtbl.create 97  in*)
  (*                                                                         *)
  (** Associates each labeled statement to a pre-condition                   *)
  let old_observed_labelled_stmts_pre : (int, (bool array array * bool array array )) Hashtbl.t  = Hashtbl.create 97  in
  let labelled_stmts_pre : (int, (bool array array * bool array array )) Hashtbl.t  = Hashtbl.create 97  in
  (*                                                                         *)
  (** Associates each labeled statement to a post-condition                  *)
  (*let labelled_stmts_post : (int, (bool array array * bool array array )) Hashtbl.t  = Hashtbl.create 97  in*)
  (*                                                                         *)
  (***************************************************************************)
  (* For the pre-condition propagation pass                                  *)
  (*                                                                         *)
  (** During the pre-condition propagation, it represents the set of
      statements that need second computation of specification. For instance,
      it can occurs when the statement is pointed by a goto instruction. *)
  let stmts_to_compute_one_more_time : (int , bool) Hashtbl.t  = Hashtbl.create 97  in
  (*                                                                         *)
  (** This variable contains the result of the pre-condition propagation.    *)
  let propagation_result = ref (mk_empty_pre_or_post_bycase ()) in
  (*                                                                         *)
  (***************************************************************************)
  (* For the post-condition propagation pass                                 *)
  (*                                                                         *)
  (** Set of observed labeled statement. This information is used for goto
      treatment.                                                             *)
  let status_of_labelled_stmts : (int , bool) Hashtbl.t  = Hashtbl.create 97 in
  (*                                                                         *)
  (** True if and only if the computation has to be done again in oreder to
      compute a fix-point result. *)
  let second_computation_needed = ref false in
  (*                                                                         *)
  (***************************************************************************)
  (* Used for spec memorization before each call                             *)
  (** Name of the current function.                                          *)
  let currentFuncName = ref "" in
  (*                                                                         *)
  (***************************************************************************)
  (* Used for loop specification                                             *)
  (** Pre or post condition of each loop key stmt, and for FWD and BWD AI    *)
  let loop_fwd_ext_pre   = Hashtbl.create 97 in
  let loop_fwd_int_pre   = Hashtbl.create 97 in
  let loop_fwd_real_post = Hashtbl.create 97 in
  let loop_fwd_int_post  = Hashtbl.create 97 in
  let loop_bwd_ext_pre   = Hashtbl.create 97 in
  let loop_bwd_int_pre   = Hashtbl.create 97 in
  let loop_bwd_real_post = Hashtbl.create 97 in
  let loop_bwd_int_post  = Hashtbl.create 97 in
  (*                                                                         *)
  (* Accessors for loop specification                                        *)
  let get_loop_local_info hashtbl stmt_ref =
    try Hashtbl.find hashtbl stmt_ref
    with _ -> mk_full_pre_or_post_bycase()
  in
	(* status des functions a insérer *)
  (*                                                                         *)
  let update_loop_local_info hashtbl stmt_ref value =
    let info = 
      if (Hashtbl.mem hashtbl stmt_ref )
      then double_bool_array_or_bycase value (Hashtbl.find hashtbl stmt_ref)
      else value
    in
    Hashtbl.replace hashtbl stmt_ref info
  in
  (*                                                                         *)
  (***************************************************************************)


  let update_hashtbl_or hasbtbl key value =
    let new_value =
      if Hashtbl.mem hasbtbl key
      then double_bool_array_or_bycase (Hashtbl.find hasbtbl key) value
      else value
    in
    Hashtbl.replace hasbtbl key new_value
  in






(** Propagates pre-condition to each statement, by following control flow.
    It returns a couple af bool array, definig the strongest post-condition of the statement list. *)
  let rec propagates_pre stmt_l (pre_st,pre_tr) =
    
    (** This function returns the curent pre of a statement or an empty
	pre if no specification exists *)
    let get_labelled_stmt_pre stmt_sid =
      try
	let pre_st,pre_tr = Hashtbl.find labelled_stmts_pre stmt_sid in
	pre_st,pre_tr
      with
	| Not_found -> mk_empty_pre_or_post_bycase()
    in



    (** This function makes an OR filter between the given pre and the old pre of the given stmt
        The result is storing as the new pre of the given stmt. *)
    let update_labelled_stmt_pre stmt_sid pre =
      try
	let old_pre = Hashtbl.find labelled_stmts_pre stmt_sid in
	let n_pre = double_bool_array_or_bycase old_pre pre in
	Hashtbl.replace labelled_stmts_pre stmt_sid n_pre
      with
	| _ ->
	    Hashtbl.replace labelled_stmts_pre stmt_sid pre
    in

    (** This function returns the current pre of the given statement.
	WARNING !
	Side effects of this function :
	  * If the statement is in stmts_to_compute_one_more_time then it is removed
	  * The pre of the current stmt is updated according to the current pre_st and pre_tr
    *)
    let update_and_get_stmts_pre stmt_sid with_labels =
      (* If this statement is annotated to be computed again then we remove its annotation. *)
      Hashtbl.remove stmts_to_compute_one_more_time stmt_sid;

      (* Registering the new specification only if it is a stmt with multiple entry points (labelled stmt) *)
      if with_labels then
	begin
	  update_labelled_stmt_pre stmt_sid (pre_st,pre_tr);
	  get_labelled_stmt_pre stmt_sid 
	end
      else
	(pre_st,pre_tr)
    in




    (* Updating pre-condition with previous information *)
    let pre_st,pre_tr = 
      if stmt_l <>[] then
	let s = (List.hd stmt_l) in 
	update_and_get_stmts_pre s.sid (s.labels<>[])
      else
	pre_st,pre_tr
    in

    match stmt_l with
      | [] ->
	  (pre_st,pre_tr)



      | ({skind=Instr(Call(_,{enode = Lval(Var(vi),_)},_,_))} as stmt)::l ->
	  if (Data_for_ltl.isIgnoredFunction vi.vname) then 
	    propagates_pre l (pre_st,pre_tr)
	  else
	    begin

	      (* If the statement is unreachable then we skip the call *)
	      if (double_bool_array_eq_bycase (pre_st,pre_tr) (mk_empty_pre_or_post_bycase())) then
  		propagates_pre l (pre_st,pre_tr)
	      else
		begin
		  (* Simulating crossing transition *)
		  let pre_call=Ltl_utils.get_next_bycase vi.vname Promelaast.Call pre_st in
		  
		  (* When stmt=call => the spec has to be memorized as pre of the call *)
		  Data_for_ltl.set_func_pre_call_bycase !currentFuncName stmt.sid pre_call;


	      (* Registering call context for future reinforcement of pre-condition. Treatment depends on call recursivity*)
(* 	      if (String.compare vi.vname !currentFuncName)<>0 then *)
		(*    * Case 1 : none recursive call *)
		(*               No recursive calls are stored in a table for further special treatment *)
		  update_hashtbl_or functions_pre_usecase vi.vname pre_call;
(* 	      else *)
(* 		(\*    * Case 2 : recursive call *\) *)
(* 		(\*               Recursive calls are stored in another table for different further treatment *\) *)
(* 		update_hashtbl_or functions_pre_usecase_Recursive vi.vname pre_call; *)


	      (* From now, pre-condition is the set of configurations from which
		 the operation is callable according to its post-condition. *)
		  let (post_call_st,post_call_tr) = Ltl_utils.mk_forward_composition (fst pre_call) (Data_for_ltl.get_func_post_bycase vi.vname) in



		  propagates_pre l (post_call_st,post_call_tr)
		end 
	    end



      | {skind=Instr(Call(_,_,_,_))}::_ ->
	  Ltl_to_acsl_option.fatal "Ltl_to_acsl plugin internal error. Status : Operation calls has to be done by explicit operation name\n";




      | ({skind=Instr (_)})::l ->
	  (* Computes next statements specification *)
	  propagates_pre l (pre_st,pre_tr)



      | ({skind=Block(b)})::l ->
	  (* Propagation into block *)
	  let post = (propagates_pre b.bstmts (pre_st,pre_tr)) in

	  (* Computes next statements specification *)
  	  propagates_pre l post



      | ({skind=If(_,b1,b2,_)})::l ->
	  (* Constraints propagation into branches. *)
	  let post_block1 = propagates_pre b1.bstmts (pre_st,pre_tr)  in
	  let post_block2 = propagates_pre b2.bstmts (pre_st,pre_tr)  in

	  (* The new post-condition is the disjunction of branches post-conditions *)
	  let post = double_bool_array_or_bycase post_block1 post_block2 in

	  (* Computes next statements specification *)
	  propagates_pre l post




      | ({skind=Return (_,_)})::l ->
	  (* Updating pre-condition and current result with previous information *)
	  propagation_result:= (pre_st,pre_tr);

	  (* A pre-treatment of frama-C has to put the return statement at the
	     end of the function. *)
	  if l<>[] then assert false;

	  (* Return the post-condition of the current function *)
	  !propagation_result



      | ({skind=Goto(stmt_ref,_)})::stmt_l ->
	  (* Modifing specification of pointed statement and registering it to be computed *)
	  (* If the statement has not yet been specified *)
	  let ref_pre = get_labelled_stmt_pre !stmt_ref.sid in

	  (* If Current statement is not include into the pointed one, then we update it. *)
	  let disjunction = (double_bool_array_or_bycase ref_pre (pre_st,pre_tr)) in
	  if not (double_bool_array_eq_bycase ref_pre disjunction) then
	    begin  
	      (* Updating pre-condition of pointed statement *)
	      update_labelled_stmt_pre !stmt_ref.sid (pre_st,pre_tr);
	      Hashtbl.replace stmts_to_compute_one_more_time !stmt_ref.sid true;
	    end;

	  (* In order to treat statements that are not directly reachable,
	     consumes following statements until a labeled one with a defined pre-condition. *)
(* 	  let _ = propagates_pre stmt_l (mk_empty_pre_or_post_bycase ()) in *)
(* 	  (mk_empty_pre_or_post_bycase ()) *)
	  propagates_pre stmt_l (mk_empty_pre_or_post_bycase ()) 






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
	     {Real_loop_post}


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

	     Finally :
	       Real_loop_post  = Pre2 /\ BWDed_real_loop_post
	       and the invariant is:
	         (Init => Pre1)
	       & (not Init => Post2)
	     (where init is a fresh variable to indicate if the iteration is the first one).


	  *)

	  (* Updating pre-conditions with previous information *)
	  let loop_pre  = double_bool_array_and_bycase (pre_st,pre_tr) (get_loop_local_info loop_bwd_ext_pre (ref stmt)) in
	  let block_pre = loop_pre in


	  (* First forward propagation into block *)
	  let old_post   = ref block_pre in
	  let block_post = ref( propagates_pre block.bstmts block_pre ) in
	  let block_pre  = ref( double_bool_array_or_bycase block_pre !block_post ) in

	  (* Fix-point computation *)
	  while not (double_bool_array_eq_bycase !old_post !block_post) do

	    old_post := !block_post;
	    block_post:=propagates_pre block.bstmts !block_pre;
	    block_pre :=double_bool_array_or_bycase !block_pre !block_post

	  done;

	  (* Finally : Real_loop_post  = Pre2 /\ BWDed_real_loop_post *)
	  let  real_loop_post = double_bool_array_and_bycase !block_pre (get_loop_local_info loop_bwd_real_post (ref stmt)) in

	  (* Updating loop information *)
          update_loop_local_info loop_fwd_ext_pre (ref stmt) loop_pre;
          update_loop_local_info loop_fwd_int_pre (ref stmt) !block_pre;
          update_loop_local_info loop_fwd_real_post (ref stmt) real_loop_post; 
          update_loop_local_info loop_fwd_int_post (ref stmt) !block_post;

	  (* Computes next statements specification *)
	  (* The end of the loop is done through a goto instruction that
	     does not appear in the CIL structure. This is why, the
	     post-condition is the exit case of the loop invariant. *)
          propagates_pre stmt_l (mk_empty_pre_or_post_bycase ())(*loop_post_st,loop_post_tr*)




      | {skind=UnspecifiedSequence(b)}::l ->
	  let post = propagates_pre (Cil.block_from_unspecified_sequence(b)).bstmts (pre_st,pre_tr) in

	  propagates_pre l post

(*	  propagates_pre
	    ((mkStmt(Block(Cil.block_from_unspecified_sequence(b))))::l)
	    (pre_st,pre_tr)*)



      | {skind=Switch (_,bl,stmtl,_)}::l  ->
	  (* Step 1 : For each case, the pre-condition is set to pre_st,pre_tr. *)
	  List.iter 
	    (fun stmt -> update_labelled_stmt_pre stmt.sid (pre_st,pre_tr))
	    stmtl;

	  (* Step 2 : The block is put into the todo list *)
(* 	  propagates_pre *)
(* 	    ((mkStmt(Block(bl)))::l) *)
(* 	    (pre_st,pre_tr) *)
	  let post = propagates_pre bl.bstmts (pre_st,pre_tr) in

	  propagates_pre l post
	    



      | {skind=Break (_)}::_
      | {skind=Continue (_)}::_ ->
	  Ltl_to_acsl_option.fatal "Ltl_to_acsl plugin internal error. Continue and Break statements have to be rewritten into goto by the CFG pass.\n";

      | {skind=TryFinally (_,_,_) }::_
      | {skind=TryExcept(_,_,_,_)}::_ ->
	  Ltl_to_acsl_option.fatal "Ltl_to_acsl plugin internal error. Status : try constructions are not yet supported.\n";


















(** Propagates post-condition to each statement, by following control flow.
    It returns a couple of bool array, definig the weakest pre-condition of the statement list.
    Since then analysis is a backward one, the list is first reversed. *)

  in let rec propagates_post stmt_l (post_st,post_tr) =

    (** This function returns the current spec of a statement or an empty
	spec if no specification exists *)
    let get_labelled_stmt_pre stmt_sid =
      try Hashtbl.find labelled_stmts_pre stmt_sid
      with Not_found -> mk_empty_pre_or_post_bycase()
    in


    (** This function makes an OR filter between the given pre and the old pre of the given stmt
        The result is storing as the new pre of the given stmt. *)
    let update_labelled_stmt_pre stmt_sid pre =
      let old_pre = get_labelled_stmt_pre stmt_sid in
      let new_pre = double_bool_array_or_bycase old_pre pre in

      Hashtbl.replace labelled_stmts_pre stmt_sid new_pre
    in


    (** This function returns the current spec of the given statement.
	WARNING !
	Side effects of this function :
	  * The pre of the current stmt is updated according to the given pre
    *)
    let update_labelled_stmt_pre stmt_ref pre =
      (* Registering the new pre-condition if the stmt is labelled *)
      if !stmt_ref.labels<>[] then 
	begin
	  update_labelled_stmt_pre !stmt_ref.sid pre;
    	  Hashtbl.replace status_of_labelled_stmts !stmt_ref.sid true
	end
    in


    (** Body of propagates_post (after list.rev) *)
    let rec prop  stmt_l (post_st,post_tr) =


      match stmt_l with 
	| [] -> (post_st,post_tr)



	| ({skind=Instr(Call(_,{enode = Lval(Var(vi),_)},_,_))} as stmt)::l ->
	    if (Data_for_ltl.isIgnoredFunction vi.vname) then begin
	      (* Updating the specification of the current stmt in the hashtbl. *)
              update_labelled_stmt_pre (ref stmt) (post_st,post_tr);

	      (* Computes next statements specification *)
	      prop l (post_st,post_tr)
	    end    
	    else
	      begin

	    (* If statement is unreachable then we skip the call *)
		if (double_bool_array_eq_bycase (post_st,post_tr) (mk_empty_pre_or_post_bycase())) then
 		  prop l (post_st,post_tr)
		else
		  begin
		(* Registering call context for future reinforcement of post-condition. Treatment depends on call recursivity*)

(* 		if (String.compare vi.vname !currentFuncName)<>0 then *)
		    (*    * Case 1 : none recursive call *)
		    (*               No recursive calls are stored in a table for further special treatment *)
		    update_hashtbl_or functions_post_usecase vi.vname (post_st,post_tr);
(* 		else *)
(* 		    (\*    * Case 2 : recursive call *\) *)
(* 		    (\*               Recursive calls are stored in another table for different further treatment *\) *)
(* 		  update_hashtbl_or functions_post_usecase_Recursive vi.vname (post_st,post_tr); *)
		
		(* From now, post-condition is the set of configurations from which
		   the operation is callable according to its pre-condition and
		   of the current statement pre-condition. *)
		    let pre_call =
		      Ltl_utils.mk_backward_composition
			post_st 
			(Data_for_ltl.get_func_pre vi.vname) 
			(Data_for_ltl.get_func_post_bycase vi.vname)
		    in
		    let cur_pre = Ltl_utils.get_prev_bycase vi.vname Promelaast.Call (pre_call) in


		    (* When stmt=call => the spec has to be memorized as pre of the call *)
		    Data_for_ltl.set_func_pre_call_bycase !currentFuncName stmt.sid cur_pre;

		    (* Updating the specification of the current stmt in the hashtbl. *)
                    update_labelled_stmt_pre (ref stmt) cur_pre;
		    
		    (* Computes next statements specification *)
		    prop l cur_pre
		  end
	      end



      | {skind=Instr(Call(_,_,_,_))}::_ ->
	  Ltl_to_acsl_option.fatal "Ltl_to_acsl plugin internal error. Status : Operation calls has to be done by explicit operation name\n";



      | ({skind=Instr (_)} as stmt)::l ->
	  (* Updating the specification of the current stmt in the hashtbl. *)
          update_labelled_stmt_pre (ref stmt) (post_st,post_tr);

	  (* Computes next statements specification *)
	  prop l (post_st,post_tr)



      | ({skind=Block(b)} as stmt)::l ->
	  (* Computes recursivly the block specification *)
	  let cur_pre = (propagates_post b.bstmts (post_st,post_tr)) in

	  (* Updating the specification of the current stmt in the hashtbl. *)
	  update_labelled_stmt_pre (ref stmt) cur_pre ;

	  (* Computes next statements specification *)
  	  prop l cur_pre



      | ({skind=If(_,b1,b2,_)} as stmt)::l ->
	  (* Constraints propagation into branches. *)
	  let pre_block1 = propagates_post b1.bstmts (post_st,post_tr) in
	  let pre_block2 = propagates_post b2.bstmts (post_st,post_tr) in

	  (* The new pre-condition is the disjunction of branches pre-conditions *)
	  let pre = double_bool_array_or_bycase pre_block1 pre_block2 in

	  (* Updating the specification of the current stmt in the hashtbl. *)
          update_labelled_stmt_pre (ref stmt) pre;

	  (* Computes next statements specification *)
	  prop l pre




      | ({skind=Return (_,_)} as stmt)::l ->
	  (* The 'prev' according to the return will be done be the caller of the propagates function. *)
	  (* Updating the specification of the current stmt in the hashtbl. *)
          update_labelled_stmt_pre (ref stmt) (post_st,post_tr);

	  (* Return the post-condition of the current function *)
	  prop l (post_st,post_tr)




      | ({skind=Goto(stmt_ref,_)} as stmt)::stmt_l ->
	  (* Retriving old specification information about this statement and the pointed one. *)
	  let ref_pre = get_labelled_stmt_pre !stmt_ref.sid in

	  let old_ref_pre = 
	    try Hashtbl.find old_observed_labelled_stmts_pre stmt.sid 
	    with Not_found -> mk_empty_pre_or_post_bycase()
	  in
	  

	  (* Second computation needed if the pointed stmt has not yet been treated
	     or if its pre differs from the current post *)
	  if not (double_bool_array_eq_bycase ref_pre old_ref_pre) then 
	    begin
	      second_computation_needed:= true;
	      Hashtbl.replace old_observed_labelled_stmts_pre stmt.sid ref_pre
	    end;

	  (* Add the specification of the current stmt in the hashtbl. *)
          update_labelled_stmt_pre (ref stmt) ref_pre;

	  prop stmt_l ref_pre



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
	     {RealLoopPost}


	     Pre1  : pre-condition before entering the loop
	     Pre2  : pre-condition of each iteration
	     Post1 : False (Infinite loop)
	     Post2 : Post condition of an iteration
	     RealLoopPost : Real post-condition of the loop


	     Initially :
  	     RealLoopPost = [block]EmptyPost
	     Pre2  = RealLoopPost
	     Post2 = Pre2 

	     do
  	       Pre2 = ([block]Post2) \/ Pre2 // Adding reachable pre states
	       Post2 = Pre2 \/ Post2         // Is there any new states ?
	     until fix-point reached.

	     Finally :
	       Pre1 = Pre2

	     The loop invariant is then :
	        (c  => Pre2)
	      & (!c => Post1)
	      & (Init => Pre1)
	      & (not Init => Post2)
	     (where init is a fresh variable to indicate if the iteration is the first one).

	  *)

          (* First backward propagation into block
  	       RealLoopPost = ([block]EmptyPost) /\ FWD_real_post
	       Pre2  = RealLoopPost
	       Post2 = Pre2 
	  *)
	  let real_loop_post  = 
	    double_bool_array_and_bycase
	      (propagates_post block.bstmts (mk_empty_pre_or_post_bycase())) 
              (get_loop_local_info loop_fwd_real_post (ref stmt)) 
	  in
	  let block_pre  = real_loop_post in
          let block_post = block_pre in


          (* Looped backward propagation into block
	     do
  	       Pre2 = ([block]Post2) \/ Pre2 // Adding reachable pre states
	       Post2 = Pre2 \/ Post2         // Is there any new states ?
	     until fix-point reached.

	  *)
	  (* Loop initialisation for fix-point computation *)
          let old_pre = ref( mk_empty_pre_or_post_bycase()) in
	  let block_pre = ref block_pre in
	  let block_post = ref block_post in

	  while not (double_bool_array_eq_bycase !old_pre !block_pre) do

	    old_pre    := !block_pre ;
	    block_pre  := propagates_post block.bstmts !block_post ;
            block_pre  := double_bool_array_or_bycase !block_pre !old_pre ;
	    block_post := double_bool_array_or_bycase !block_pre !block_post

	  done;

	  (* The result is dereferenced *)
	  let block_pre = !block_pre in
	  let block_post = !block_post in


          (* Finally : Pre1  = Pre2 /\ FWD_pre1  *)
	  let loop_pre = double_bool_array_and_bycase block_pre (get_loop_local_info loop_fwd_ext_pre (ref stmt)) in


	  (* Updating loop information *)
          update_loop_local_info loop_bwd_ext_pre (ref stmt) loop_pre;
          update_loop_local_info loop_bwd_int_pre (ref stmt) block_pre; 
          update_loop_local_info loop_bwd_real_post (ref stmt) real_loop_post; 
          update_loop_local_info loop_bwd_int_post (ref stmt) block_post;


	  (* Add the specification of the current stmt in the hashtbl. *)
	  update_labelled_stmt_pre (ref stmt) loop_pre ;

	  (* Computes next statements specification *)
  	  prop stmt_l loop_pre




      | ({skind=UnspecifiedSequence(b)} as stmt)::l ->
	  (* Sequence is packed in a block which is added to the todo list. *)
	  let pre = propagates_post (Cil.block_from_unspecified_sequence(b)).bstmts (post_st,post_tr) in

	  (* Add the specification of the current stmt in the hashtbl. *)
	  update_labelled_stmt_pre (ref stmt) pre ;

	  (* Computes next statements specification *)
	  prop l (post_st,post_tr)



      | ({skind=Switch (_,bl,stmtl,_)} as stmt)::l  ->
	  (* Step 1 : The block is treated by the classical block analysis *)
(* 	  let pre = ref (prop [mkStmt(Block(bl))] (post_st,post_tr)) in *)
(* 	  let _ = propagates_post bl.bstmts (post_st,post_tr) in *)
	  let pre = ref (propagates_post bl.bstmts (post_st,post_tr)) in


	  (* Step 2 : For each case, the current pre-condition is set to pre_case OR cur_pre . *)
(* 	  let pre = ref (mk_empty_pre_or_post_bycase())  *)
	  List.iter
	    (fun stmt -> 
	       let stmt_pre = (get_labelled_stmt_pre stmt.sid) in
	       pre:=Spec_tools.double_bool_array_or_bycase stmt_pre !pre)
	    stmtl;

	  (* Add the specification of the current stmt in the hashtbl. *)
	  update_labelled_stmt_pre (ref stmt) !pre ;

	  (* Computes next statements specification *)
	  prop l !pre








      | {skind=Break (_)}::_
      | {skind=Continue (_)}::_ ->
	  Ltl_to_acsl_option.fatal "Ltl_to_acsl plugin internal error. Status : Break and Continue instructions are not yet supported.\n";
      | {skind=TryFinally (_,_,_) }::_
      | {skind=TryExcept(_,_,_,_)}::_ ->
	  Ltl_to_acsl_option.fatal "Ltl_to_acsl plugin internal error. Status : try constructions are not yet supported.\n";



    in
    (* This computation is done from end to begining *)
    prop (List.rev stmt_l) (post_st,post_tr)

  in

object (*(self) *)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    currentFuncName:=f.svar.vname;

    let starting_pre  = (Data_for_ltl.get_func_pre f.svar.vname) in
    let starting_post = (Data_for_ltl.get_func_post_bycase f.svar.vname) in

(* Format.printf "\nAvant passe 1       : "; *)
(* Ltl_utils.debug_display_func_status_bycase f.svar.vname; *)
 

    Hashtbl.clear labelled_stmts_pre;
    Hashtbl.clear stmts_to_compute_one_more_time;
    propagation_result := (mk_empty_pre_or_post_bycase ());

    (* Pre-condition forward propagation *)
    let cur_pre =  Ltl_utils.mk_pre_or_post_bycase_from_pre_or_post (Data_for_ltl.get_func_pre f.svar.vname) in
    let _ = propagates_pre f.sbody.bstmts cur_pre in
    let cur_post_st = ref (fst !propagation_result) in
    while (Hashtbl.length stmts_to_compute_one_more_time) > 0 do
      let _ = propagates_pre f.sbody.bstmts cur_pre in
      cur_post_st := fst (!propagation_result)
    done;


    (* Registration of the new post-condition *)
    let post     = Ltl_utils.get_next_bycase f.svar.vname Promelaast.Return !cur_post_st in
    let old_post = (Data_for_ltl.get_func_post_bycase f.svar.vname)  in
    let post     = double_bool_array_and_bycase post old_post in

    Data_for_ltl.set_func_post_bycase f.svar.vname post;

(* Format.printf "Entre passes 1 et 2 : "; *)
(* Ltl_utils.debug_display_func_status_bycase f.svar.vname; *)


    (* Post-condition backward propagation *)
    (* cur_post : bool array array * bool array array The first index is in term of the function input states *)
    let cur_post = (Data_for_ltl.get_func_post_bycase f.svar.vname) in
    let cur_post = Ltl_utils.get_prev_bycase f.svar.vname Promelaast.Return cur_post in

    Hashtbl.clear labelled_stmts_pre;
    Hashtbl.clear status_of_labelled_stmts;
    Hashtbl.clear old_observed_labelled_stmts_pre;

    second_computation_needed:=false;
    let cur_pre  = ref (propagates_post f.sbody.bstmts cur_post) in
    
    while !second_computation_needed do
      Hashtbl.clear status_of_labelled_stmts;
      second_computation_needed:=false;
      cur_pre := propagates_post f.sbody.bstmts cur_post
    done;


    (* Registration of the new pre-condition *)
    let cur_pre = pre_flattening !cur_pre in
    let old_pre = Data_for_ltl.get_func_pre f.svar.vname in
    let pre     = double_bool_array_and cur_pre old_pre in

    Data_for_ltl.set_func_pre f.svar.vname pre;

    
    let merge tbl1 tbl2 get set =
      Hashtbl.iter 
	(fun key value -> 
	   let v1 = double_bool_array_and_bycase value (Hashtbl.find tbl2 key) in
	   let v2 = get key in
	   if not( double_bool_array_eq_bycase v2 v1 ) then begin
	     set key ( double_bool_array_and_bycase v2 v1 );
	     spec_modified:=true
	   end
	)
	tbl1
    in
    merge loop_fwd_ext_pre loop_bwd_ext_pre Data_for_ltl.get_loop_ext_pre_bycase Data_for_ltl.set_loop_ext_pre_bycase;
    merge loop_fwd_int_pre loop_bwd_int_pre Data_for_ltl.get_loop_int_pre_bycase Data_for_ltl.set_loop_int_pre_bycase;
    merge loop_fwd_int_post loop_bwd_int_post Data_for_ltl.get_loop_int_post_bycase Data_for_ltl.set_loop_int_post_bycase;
(*     merge loop_fwd_real_post loop_bwd_real_post Data_for_ltl.get_loop_real_post_bycase Data_for_ltl.set_loop_real_post_bycase; *)




(*  Format.printf "Apres passe 2       : "; *)
(*  Ltl_utils.debug_display_func_status_bycase f.svar.vname; *)


    let ending_pre  = (Data_for_ltl.get_func_pre f.svar.vname) in
    let ending_post = (Data_for_ltl.get_func_post_bycase f.svar.vname) in

    if   (not (double_bool_array_eq starting_pre  ending_pre ) )
    then begin spec_modified:=true;  end;
    if   (not (double_bool_array_eq_bycase starting_post ending_post) )
    then begin spec_modified:=true;  end;

    DoChildren
end





let propagates_pre_post_constraints_bycase file root =
  Hashtbl.clear functions_pre_usecase ;
  Hashtbl.clear functions_post_usecase;
  spec_modified:=false;

  let visitor = new visit_propagating_pre_post_constraints_bycase (Data_for_ltl.getAutomata()) in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file;

  (* Refining specification according to use-cases. *)
  List.iter
    (fun name ->
       if name <> root then
	 begin
	   let old_pre  = (Data_for_ltl.get_func_pre name) in
	   let old_post = (Data_for_ltl.get_func_post_bycase name) in
	   let used_pre =
	     try Hashtbl.find functions_pre_usecase  name with Not_found -> (mk_empty_pre_or_post_bycase()) in
	   let used_post =
	     try Hashtbl.find functions_post_usecase name with Not_found -> (mk_empty_pre_or_post_bycase()) in

(* Format.printf "\n\nFunction : %s\n" name; *)
(* Format.printf "    Observed pre use_case :"; *)
(* debug_display_stmt_all_pre_bycase used_pre; *)
(* Format.printf "\n    Observed post_use_case :"; *)
(* debug_display_stmt_all_pre_bycase used_post; *)
(* Format.printf "\n"; *)


	   (* Reformating usecases of pre and post *)
	   let used_pre_st,used_pre_tr = pre_flattening used_pre in 
	   let used_pre_st,used_pre_tr = (ref used_pre_st),(ref used_pre_tr) in 

	   let used_post_st,used_post_tr = post_pseudo_flattening used_post in 
	   let used_post_st,used_post_tr = (ref used_post_st),(ref used_post_tr) in 


(* 	   (\* If recursive calls, then using it to update used_pre *\) *)
(* 	   begin *)
(* 	     try  *)
(* 	       (\* Recursive calls are stored in another table *\) *)
(* 	       let rec_pre =  *)
(* 	         Hashtbl.find functions_pre_usecase_Recursive  name  *)
(* 	       in *)
(* 	       (\* Reflexive and transitive closure on recursive calls in order to update used_pre *\) *)
(* (\* DEBUG TRACES !!!*\) *)
(* (\* Format.printf "\n\nFunction : %s\n" name; *\) *)
(* (\* Format.printf "    Observed pre use_case :"; *\) *)
(* (\* debug_display_stmt_all_pre (!used_pre_st,!used_pre_tr); *\) *)
(* (\* Format.printf "\n    Observed special recursion calls :"; *\) *)
(* (\* debug_display_stmt_all_pre_bycase rec_pre; *\) *)
(* (\* Format.printf "\n"; *\) *)
(* (\* END DEBUG TRACES !!!*\) *)

(*                (\* Recursive calls are considered as use_case only if external calls are done with the same starting state *\) *)
(*                (\* For each state used for calling, extending used_pre according to recursive extensions *\) *)
(*                let oneMoreTime = ref true in  *)
(* 	       while !oneMoreTime do *)
(* 		 oneMoreTime:=false; *)
(* 		 Array.iteri *)
(* 		   (fun st value ->  *)
(* 		      if value then begin *)
(* 			let tmp_st=bool_array_or !used_pre_st (fst rec_pre).(st) in *)
(* 			let tmp_tr=bool_array_or !used_pre_tr (snd rec_pre).(st) in *)
			
(* 			if not (double_bool_array_eq (!used_pre_st,!used_pre_tr) (tmp_st,tmp_tr)) then oneMoreTime:=true; *)
			
(* 			used_pre_st:= tmp_st; *)
(* 			used_pre_tr:= tmp_tr *)
(* 		      end *)
(* 		   ) *)
(* 		   !used_pre_st; *)
(* 	       done; *)

(* (\* DEBUG TRACES !!!*\) *)
(* (\* Format.printf "    Updated pre use_case :"; *\) *)
(* (\* debug_display_stmt_all_pre (!used_pre_st,!used_pre_tr); *\) *)
(* (\* Format.printf "\n"; *\) *)
(* (\* END DEBUG TRACES !!!*\) *)

(* 	     with  *)
(* 	       | Not_found -> () *)
(* 	   end; *)
	

(* 	   (\* If recursive calls, then using it to update used_post *\) *)
(* 	   begin *)
(* 	     try  *)
(* 	       (\* Recursive calls are stored in another table *\) *)
(* 	       let rec_post =  *)
(* 	         Hashtbl.find functions_post_usecase_Recursive  name  *)
(* 	       in *)
(* 	       (\* Reflexive and transitive closure on recursive calls in order to update used_pre *\) *)
(* (\* DEBUG TRACES !!!*\) *)
(* (\* Format.printf "\n\nFunction : %s\n" name; *\) *)
(* (\* Format.printf "    Observed pre use_case :"; *\) *)
(* (\* debug_display_stmt_all_pre (!used_pre_st,!used_pre_tr); *\) *)
(* (\* Format.printf "\n    Observed special recursion calls :"; *\) *)
(* (\* debug_display_stmt_all_pre_bycase rec_pre; *\) *)
(* (\* Format.printf "\n"; *\) *)
(* (\* END DEBUG TRACES !!!*\) *)

(*                (\* Recursive calls are considered as use_case only if external calls are done with the same starting state *\) *)
(*                (\* For each state used for calling, extending used_post according to recursive extensions *\) *)
(*                let oneMoreTime = ref true in  *)
(* 	       while !oneMoreTime do *)
(* 		 oneMoreTime:=false; *)
(* 		 Array.iteri *)
(* 		   (fun input_st case_post_st ->  *)
(* 		      let case_post_tr = !used_post_tr.(input_st) in *)
(* 		      Array.iteri *)
(* 			(fun st value ->  *)
(* 			   if value then begin *)
(* 			     (\* !!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* 				!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* 				!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* 				Ici (fst rec_post).(st) est  incorrect. *)
(* 				Je voudrais dire :  *)
(* 				  etant donne un etat sortie de la fonction, quel sont les etats/trans courant qui y mene *)

(* 				APRES AVOIR SAUVEGARDER CETTE VERSION SANS LE REC_USED_CASE : *)
				
(* 				Refaire tout le backarwd propagation en utilisant le bycase suivant :  *)
(* 				exit state -> curstate -> true / false *)

(* 				Actuellement (adapte pour le FWD propagation) :  *)
(* 				entry state -> curstate -> true / false *)

(* 			     *\) *)
			     
(* 			     let tmp_st=bool_array_or case_post_st (fst rec_post).(st) in *)
(* 			     let tmp_tr=bool_array_or case_post_tr (snd rec_post).(st) in *)
(* 			     (\*!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* 			       !!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* 			       !!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* 			       !!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* 			     *\) *)

(* 			     if not (double_bool_array_eq (case_post_st,case_post_tr) (tmp_st,tmp_tr)) then oneMoreTime:=true; *)
			     
			     
(* 			     !used_post_st.(input_st) <- tmp_st; *)
(* 			     !used_post_tr.(input_st) <- tmp_tr *)
(* 			   end *)
(* 			) *)
(* 			case_post_st *)
(* 		   ) *)
(* 		   !used_post_st; *)
(* 	       done; *)

(* (\* DEBUG TRACES !!!*\) *)
(* (\* Format.printf "    Updated pre use_case :"; *\) *)
(* (\* debug_display_stmt_all_pre (!used_pre_st,!used_pre_tr); *\) *)
(* (\* Format.printf "\n"; *\) *)
(* (\* END DEBUG TRACES !!!*\) *)

(* 	     with  *)
(* 	       | Not_found -> () *)
(* 	   end; *)



	   (* Computing new pre/post *)
	   let cur_pre  = double_bool_array_and (!used_pre_st,!used_pre_tr)  old_pre  in
	   let cur_post = double_bool_array_and_bycase (!used_post_st,!used_post_tr) old_post in 

	   
	   if   (not (double_bool_array_eq old_pre  cur_pre ) )
	   then begin spec_modified:=true;  end;
	   if   (not (double_bool_array_eq_bycase old_post cur_post) )
	   then begin spec_modified:=true;  end;


	   Data_for_ltl.set_func_pre  name cur_pre;
	   Data_for_ltl.set_func_post_bycase name cur_post
	 end
    )
    (Data_for_ltl.getFunctions_from_c ());

  !spec_modified



(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
