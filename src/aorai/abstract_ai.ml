(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

(* $Id: abstract_ai.ml,v 1.4 2008-12-19 15:30:56 uid588 Exp $ *)

open Cil_types
open Cil
open Cilutil
open Ast_info
open Spec_tools


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
class visit_propagating_pre_post_constraints 
  (auto:Promelaast.typed_automaton) =
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
  let propagation_result = ref (mk_empty_pre_or_post ()) in
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
  (* Used for spec memorization before each call                             *)
  (** Name of the current function.                                          *)
  let currentFuncName = ref "" in
  (*                                                                         *)
  (***************************************************************************)




(** Propagates pre-condition to each statement, by following control flow.
    It returns a couple af bool array, definig the strongest post-condition of the statement list. *)
  let rec propagates_pre stmt_l (pre_st,pre_tr) (*lastFuncStatusSet*) =

    (** This function returns the curent pre of a statement or an empty
        pre if no specification exists *)
    let get_pre_of stmt =
      try
        let pre_st,pre_tr,_,_ = Hashtbl.find stmts_spec stmt in
        pre_st,pre_tr
      with
        | Not_found -> mk_empty_pre_or_post()
    in



    (** This function makes an OR filter between the given pre and 
        the old pre of the given stmt. The result is stored as the new 
        pre of the given stmt. *)
    let update_stmt_pre stmt pre =
      try
        let old_pre_st,old_pre_tr,post_st,post_tr = 
          Hashtbl.find stmts_spec stmt
        in
        let n_pre_st,n_pre_tr = 
          double_bool_array_or (old_pre_st,old_pre_tr) pre 
        in
        Hashtbl.replace stmts_spec stmt (n_pre_st,n_pre_tr,post_st,post_tr)
      with
        | _ ->
            let n_pre_st,n_pre_tr = pre in
            Hashtbl.replace stmts_spec stmt 
              (n_pre_st,n_pre_tr,n_pre_st,n_pre_tr)
    in
    (** This function returns the current pre of the given statement.
        WARNING !
        Side effects of this function :
          * If the statement is in stmts_to_compute_one_more_time then it is removed
          * The pre of the current stmt is updated according to the current pre_st and pre_tr
    *)
    let get_stmts_pre stmt =
      (* If this statement is annotated to be computed again then we remove its annotation. *)
      Hashtbl.remove stmts_to_compute_one_more_time stmt;

      (* Registering the new specification. *)
      update_stmt_pre stmt (pre_st,pre_tr);
      let pre = get_pre_of stmt in
      pre
    in
    match stmt_l with
      | [] ->
          (pre_st,pre_tr) (*lastFuncStatusSet*)


      | ({skind=Instr(Call(_,{enode = (Lval(Var(vi),_)|
                                       CastE(_,{enode = Lval (Var vi,_)}))
                                      },_,_))} as stmt)::l ->
          if (Data_for_aorai.isIgnoredFunction vi.vname) then
            begin
              (* Updating pre-condition with previous information *)
              let pre_st,pre_tr = get_stmts_pre stmt in

              (* Add the specification of the current stmt in the hashtbl. *)
              Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,pre_st,pre_tr);

              (* Computes next statements specification *)
              propagates_pre l (pre_st,pre_tr) (*lastFuncStatusSet*)
            end
          else
            begin
              (* Updating pre-condition with previous information *)
              let pre_st,pre_tr = get_stmts_pre stmt in

              (*== add new status call for stmt ==*)
	      (* If the statement is unreachable then we skip the call *)
	      if double_bool_array_eq (pre_st,pre_tr) (mk_empty_pre_or_post())
              then
		begin
		  (* When stmt=call => 
                     the spec has to be memorized as pre of the call *)
		  Data_for_aorai.set_func_pre_call 
                    !currentFuncName stmt.sid (pre_st,pre_tr);
		  propagates_pre l (pre_st,pre_tr)
		end
	      else
		begin
                  let kf = Globals.Functions.get vi in
		  (* Simulating crossing transition *)
		  let pre_call =
                    Aorai_utils.get_next kf Promelaast.Call pre_st 
                  in
		  (* When stmt=call => 
                     the spec has to be memorized as pre of the call *)
		  Data_for_aorai.set_func_pre_call 
                    !currentFuncName stmt.sid pre_call;
		  (* Registering call context for future reinforcement 
                     of pre-condition *)
		  let (pre_usecase_st,pre_usecase_tr) =
		    if Hashtbl.mem functions_pre_usecase vi.vname
		    then double_bool_array_or 
                      (Hashtbl.find functions_pre_usecase vi.vname) pre_call
		    else pre_call
		  in
		  Hashtbl.replace functions_pre_usecase vi.vname 
                    (pre_usecase_st,pre_usecase_tr);
		  (* From now, pre-condition is the set of configurations 
                     from which the operation is callable according 
                     to its post-condition. *)
		  let (post_call_st,post_call_tr) = 
                    Data_for_aorai.get_func_post vi.vname
                  in
		  (* Add the specification of the current stmt in the hashtbl.
		     Note that we use the pre of the statement and not the 
                     one computed for the called function. *)
		  Hashtbl.replace stmts_spec 
                    stmt (pre_st,pre_tr,post_call_st,post_call_tr);
		  (* Computes next statements specification *)
  		  propagates_pre l (post_call_st,post_call_tr)
		end
	    end
      | {skind=Instr(Call(_,_,_,_))}::_ ->
	Aorai_option.fatal "Indirect calls are not supported yet"
      | ({skind=Instr (_)} as stmt)::l ->
          (* Updating pre-condition with previous information *)
          let pre_st,pre_tr = get_stmts_pre stmt in

          (* Add the specification of the current stmt in the hashtbl. *)
          Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,pre_st,pre_tr);

          (* Computes next statements specification *)
          propagates_pre l (pre_st,pre_tr) (*lastFuncStatusSet*)

      | ({skind=Block(b)} as stmt)::l ->
          (* Updating pre-condition with previous information *)
          let pre_st,pre_tr = get_stmts_pre stmt in
	  (* Propagation into block *)
	  let (post_st,post_tr) = (propagates_pre b.bstmts (pre_st,pre_tr)) in
          (* Add the specification of the current stmt in the hashtbl. *)
          Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,post_st,post_tr);
	  (* Computes next statements specification *)
  	  propagates_pre l (post_st,post_tr)
      | ({skind=If(_,b1,b2,_)} as stmt)::l ->
          (* Updating pre-condition with previous information *)
          let pre_st,pre_tr = get_stmts_pre stmt in

          (* Constraints propagation into branches. *)
          let post_block1 (*lastFuncStatusSet1*)= propagates_pre b1.bstmts (pre_st,pre_tr) (*lastFuncStatusSet*) in
          let post_block2 (*lastFuncStatusSet2*)= propagates_pre b2.bstmts (pre_st,pre_tr) (*lastFuncStatusSet*) in

    (* ====== Attention traitement necessaire pour le lastFunct en faisant le regroupement!!!*)
    (* ====== Attention traitement necessaire pour le lastFunct en faisant le regroupement!!!*)
    (* ====== Attention traitement necessaire pour le lastFunct en faisant le regroupement!!!*)

(*    let lastFuncStatusSet = Data_for_aorai.NamedFunctStatusSet.union lastFuncStatusSet1 lastFuncStatusSet2 in*)




          (* The new post-condition is the disjunction of branches post-conditions *)
          let (post_st,post_tr) = double_bool_array_or post_block1 post_block2 in

          (* Add the specification of the current stmt in the hashtbl. *)
          Hashtbl.add stmts_spec stmt (pre_st,pre_tr,post_st,post_tr);

          (* Computes next statements specification *)
          propagates_pre l  (post_st,post_tr) (*lastFuncStatusSet*)



      | ({skind=Return (_,_)} as stmt)::l ->
          (* Updating pre-condition and current result with previous information *)
          let pre_st,pre_tr = get_stmts_pre stmt in
          propagation_result:= (pre_st,pre_tr);

          (* Add the specification of the current stmt in the hashtbl. *)
          Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,pre_st,pre_tr);

          (* A pre-treatment of frama-C has to put the return statement at the
             end of the function. *)
          if l<>[] then assert false;

          (* Return the post-condition of the current function *)
          !propagation_result (*lastFuncStatusSet*)



      | ({skind=Goto(stmt_ref,_)} as stmt)::stmt_l ->

          (* Updating pre-condition with previous information *)
          let pre_st,pre_tr = get_stmts_pre stmt in

          (* Add the specification of the current stmt in the hashtbl. *)
          Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,pre_st,pre_tr);

          (* Modifing specification of pointed statement and registering it to be computed *)
          (* If the statement has not yet been specified *)
          let ref_pre_st,ref_pre_tr = get_pre_of !stmt_ref in

          if not (double_bool_array_eq (ref_pre_st,ref_pre_tr) (pre_st,pre_tr))
          then
            begin
              (* Updating pre-condition of pointed statement *)
              update_stmt_pre !stmt_ref (pre_st,pre_tr);
              Hashtbl.replace stmts_to_compute_one_more_time !stmt_ref true;
            end;

          (* In order to treat statements that are not directly reachable,
             consumes following statements until a labeled one with a defined pre-condition. *)
          (*consumes stmt_l *)
          let _ = propagates_pre stmt_l (mk_empty_pre_or_post ()) (*lastFuncStatusSet----> attention la sauvegarde doit être faite*) in
          (mk_empty_pre_or_post ())




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


             State_builder.of conditions :

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
          let loop_pre = get_stmts_pre stmt in
          let block_pre = loop_pre in



          (* First forward propagation into block *)
          let old_post = ref block_pre in
          let block_post = ref( propagates_pre block.bstmts block_pre ) in
          let block_pre  = ref( double_bool_array_or block_pre !block_post ) in

          (* Fix-point computation *)
          while not (double_bool_array_eq !old_post !block_post) do

            old_post := !block_post;
            block_post:=propagates_pre block.bstmts !block_pre;
            block_pre :=double_bool_array_or !block_pre !block_post

          done;
          (* Finally : Post1  = Pre2 *)
          let (loop_post_st,loop_post_tr) = !block_pre in (* INTUILE *)

          (* Updating loop information *)
          Data_for_aorai.set_loop_ext_pre  stmt loop_pre;
          Data_for_aorai.set_loop_ext_post stmt (loop_post_st,loop_post_tr);
          Data_for_aorai.set_loop_int_pre  stmt !block_pre;
          Data_for_aorai.set_loop_int_post stmt !block_post;
          Hashtbl.replace stmts_spec stmt 
            (fst loop_pre, snd loop_pre,loop_post_st,loop_post_tr);

          (* Computes next statements specification *)
          (* The end of the loop is done through a goto instruction that
             does not appear in the CIL structure. This is why, the
             post-condition is the exit case of the loop invariant. *)
          propagates_pre stmt_l (loop_post_st,loop_post_tr) 
          (*lastFuncStatusSetPerformed ---------->
            depend de la loop, du nombre de tour etc*);




      | {skind=UnspecifiedSequence(b)}::l ->
          propagates_pre
            ((mkStmt(Block(Cil.block_from_unspecified_sequence(b))))::l)
            (pre_st,pre_tr) (*lastFuncStatusSet*)


      | {skind=Switch (_,bl,stmtl,_)}::l  ->
          (* Step 1 : For each case, the pre-condition is set to pre_st,pre_tr. *)
          List.iter
            (fun stmt -> Hashtbl.replace stmts_spec stmt 
              (pre_st,pre_tr,pre_st,pre_tr))
            stmtl;

          (* Step 2 : The block is put into the todo list *)
          propagates_pre
            ((mkStmt(Block(bl)))::l)
            (pre_st,pre_tr) (*lastFuncStatusSet*)



      | {skind=TryFinally (_,_,_) }::_
      | {skind=TryExcept(_,_,_,_)}::_ ->
                Aorai_option.fatal "Aorai plugin internal error. Status : UnspecifiedSequence and try constructions are not yet supported.\n";
(*        Format.printf "Aorai plugin internal error. Status : UnspecifiedSequence and try constructions are not yet supported.\n";*)
(*        assert false                                                                                                                   *)

      | {skind=Break (_)}::_
      | {skind=Continue (_)}::_ ->
                Aorai_option.fatal "Aorai plugin internal error. Continue and Break statements have to be rewritten into statements goto during the CFG pass.\n";
(*        Format.printf "Aorai plugin internal error. Continue and Break statements have to be rewritten into statements goto during the CFG pass.\n";*)
(*        assert false                                                                                                                                      *)



(** Propagates post-condition to each statement, by following control flow.
    It returns a couple of bool array, definig the weakest pre-condition of the statement list.
    Since then analysis is a backward one, the list is first reversed. *)

  in let rec propagates_post stmt_l (post_st,post_tr) =

    (** This function returns the current spec of a statement or an empty
        spec if no specification exists *)
    let get_spec_of stmt =
      try let spec = Hashtbl.find stmts_spec stmt in spec
      with Not_found -> mk_empty_spec()
    in


    (** This function makes an AND filter between the given post and the old post of the given stmt
        The result is storing as the new post of the given stmt. *)
    let update_stmt_post stmt (post_st,post_tr) =
      let old_pre_st,old_pre_tr,old_post_st,old_post_tr = get_spec_of stmt in
      let new_post_st,new_post_tr =
        (double_bool_array_and (old_post_st,old_post_tr) (post_st,post_tr))
      in
      Hashtbl.replace stmts_spec stmt
        (old_pre_st,old_pre_tr,new_post_st,new_post_tr)
    in
    (** This function makes an OR filter between the given post and
        the old post of the given stmt
        The result is storing as the new post of the given stmt. *)
    let update_stmt_post_OR stmt (post_st,post_tr) =
      let old_pre_st,old_pre_tr,old_post_st,old_post_tr = get_spec_of stmt in
      let new_post_st,new_post_tr = 
        (double_bool_array_or (old_post_st,old_post_tr) (post_st,post_tr))
      in
      Hashtbl.replace
        stmts_spec
        stmt
        (old_pre_st,old_pre_tr,new_post_st,new_post_tr)
    in
    (** This function returns the current spec of the given statement.
        WARNING !
        Side effects of this function :
          * The post of the current stmt is updated according
            to the given post_st and post_tr
    *)
    let get_stmts_spec stmt post_st post_tr =
      update_stmt_post stmt (post_st,post_tr);
      get_spec_of stmt
    in
    (** Body of propagates_post (after list.rev) *)
    let rec prop  stmt_l (post_st,post_tr) =

      if stmt_l <>[] then
        begin
          let s = (List.hd stmt_l) in
          if s.labels<>[] then
            begin
              if List.exists (fun s -> match s with Label (_) -> true | _ -> false) s.labels then
                (Hashtbl.replace status_of_labeled_stmts s true);

              if List.exists 
                (function Case (_) | Default(_)  -> true | _ -> false) s.labels
              then
                (update_stmt_post_OR s (post_st,post_tr))
            end
        end;
      match stmt_l with
        | [] -> (post_st,post_tr)
        | ({skind=Instr(Call(_,{enode =
                                 (Lval(Var(vi),_)
                                 | CastE(_,{enode = Lval(Var vi,_)})
                                 )},_,_))} as stmt)::l ->
            if (Data_for_aorai.isIgnoredFunction vi.vname) then begin
              (* Retriving old specification information about this statement *)
              let (pre_st,pre_tr,post_st,post_tr) = 
                get_stmts_spec stmt post_st post_tr 
              in
              (* Updating the specification of current stmt in the hashtbl. *)
              Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,post_st,post_tr);

              (* Computes next statements specification *)
              prop l (post_st,post_tr)
            end
            else
              begin
                (* Retrieving old specification information 
                   about this statement *)
                let (pre_st,pre_tr,post_st,post_tr) =
                  get_stmts_spec stmt post_st post_tr 
                in
		(* If statement is unreachable then we skip the call *)
		if (double_bool_array_eq 
                      (pre_st,pre_tr) (mk_empty_pre_or_post()))
                then
		  begin
		    (* When stmt=call => the spec has to be 
                       memorized as pre of the call *)
		    Data_for_aorai.set_func_pre_call 
                      !currentFuncName stmt.sid (pre_st,pre_tr);
  		    prop l (pre_st,pre_tr)
		  end
		else
		  begin
 		    (* Registering call context for future reinforcement 
                       of post-condition *)
                    let kf = Globals.Functions.get vi in
		    let (cur_post_st,cur_post_tr) =
		      if Hashtbl.mem functions_post_usecase vi.vname
		      then double_bool_array_or 
                        (Hashtbl.find functions_post_usecase vi.vname)
                        (post_st,post_tr)
		      else (post_st,post_tr)
		    in
                    Hashtbl.replace 
                      functions_post_usecase vi.vname (cur_post_st,cur_post_tr);
		    (* From now, post-condition is the set of configurations
                       from which the operation is callable according to its
                       pre-condition and of the current statement 
                       pre-condition. *)
		    let pre_call = (Data_for_aorai.get_func_pre vi.vname) in
		    let cur_pre = 
                      Aorai_utils.get_prev kf Promelaast.Call pre_call
                    in
		    let (cur_pre_st,cur_pre_tr) = double_bool_array_and cur_pre (pre_st,pre_tr) in

                    (* Updating the specification of the current stmt in the hashtbl. *)
                    (* Note that we use the post of the statement and not the one computed for the called function. *)
                    Hashtbl.replace stmts_spec stmt 
                      (cur_pre_st,cur_pre_tr,post_st,post_tr);

                    (* When stmt=call => the spec has to be memorized as pre of the call *)
                    Data_for_aorai.set_func_pre_call !currentFuncName stmt.sid (cur_pre_st,cur_pre_tr);

                    (* Computes next statements specification *)
                    prop l  (cur_pre_st,cur_pre_tr)
                  end
              end



      | {skind=Instr(Call(_,_,_,_))}::_ ->
                Aorai_option.fatal "Aorai plugin internal error. Status : Operation calls has to be done by explicit operation name\n";
(*        Format.printf "Aorai plugin internal error. Status : Operation calls has to be done by explicit operation name\n";*)
(*        assert false                                                                                                            *)

      | ({skind=Instr (_)} as stmt)::l ->
          (* Retriving old specification information about this statement *)
          let (pre_st,pre_tr,post_st,post_tr) = get_stmts_spec stmt post_st post_tr in

          (* Updating the specification of the current stmt in the hashtbl. *)
          Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,post_st,post_tr) ;

          (* Computes next statements specification *)
          prop l (post_st,post_tr)



      | ({skind=Block(b)} as stmt)::l ->
          (* Retriving old specification information about this statement *)
          let (pre_st,pre_tr,post_st,post_tr) =
            get_stmts_spec stmt post_st post_tr
          in

          (* Computes recursivly the block specification *)
          let cur_pre = (propagates_post b.bstmts (post_st,post_tr)) in
          let (pre_st,pre_tr) = double_bool_array_and cur_pre (pre_st,pre_tr) in

          (* Updating the specification of the current stmt in the hashtbl. *)
          Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,post_st,post_tr);

          (* Computes next statements specification *)
          prop l (pre_st,pre_tr)




      | ({skind=If(_,b1,b2,_)} as stmt)::l ->
          (* Retriving old specification information about this statement *)
          let (pre_st,pre_tr,post_st,post_tr) =
            get_stmts_spec stmt post_st post_tr
          in
          (* Constraints propagation into branches. *)
          let pre_block1 = propagates_post b1.bstmts (post_st,post_tr) in
          let pre_block2 = propagates_post b2.bstmts (post_st,post_tr) in
          (* The new pre-condition is the disjunction
             of branches pre-conditions *)
          let pre_blocks = double_bool_array_or pre_block1 pre_block2 in
          let (pre_st,pre_tr) =
            double_bool_array_and pre_blocks (pre_st,pre_tr)
          in
          (* Updating the specification of the current stmt in the hashtbl. *)
          Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,post_st,post_tr);

          (* Computes next statements specification *)
          prop l  (pre_st,pre_tr)




      | ({skind=Return (_,_)} as stmt)::l ->
          (* Retriving old specification information about this statement *)
          let (pre_st,pre_tr,post_st,post_tr) = 
            get_stmts_spec stmt post_st post_tr
          in
          (* The 'prev' according to the return will be done be the caller 
             of the propagates function. *)
          (* Updating the specification of the current stmt in the hashtbl. *)
          Hashtbl.replace stmts_spec stmt (pre_st,pre_tr,post_st,post_tr);

          (* Return the post-condition of the current function *)
          prop l (post_st,post_tr)




      | ({skind=Goto(stmt_ref,_)} as stmt)::stmt_l ->
          (* Retriving old specification information about this statement and the pointed one. *)
          let (ref_pre_st,ref_pre_tr,_,_) = get_spec_of !stmt_ref in
          let (_,_,post_st,post_tr) = get_spec_of stmt in

          (* Second computation needed if the pointed stmt has not yet been treated
             or if its pre differs from the current post *)
          if (not !second_computation_needed)
            && (Hashtbl.mem status_of_labeled_stmts stmt)
            && (not
                  (double_bool_array_eq 
                     (ref_pre_st,ref_pre_tr) (post_st,post_tr)))
          then
            second_computation_needed:=
              not (Hashtbl.find status_of_labeled_stmts stmt);
          (* Current post_st and post_tr are lost because
             they have no sense in the case of a goto instruction. *)
          let (pre_st,pre_tr,post_st,post_tr) =
            get_stmts_spec stmt ref_pre_st ref_pre_tr
          in
          (* Add the specification of the current stmt in the hashtbl. *)
          Hashtbl.add stmts_spec stmt (pre_st,pre_tr,post_st,post_tr);

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
          let (loop_pre_st,loop_pre_tr,loop_post_st,loop_post_tr) =
            get_stmts_spec stmt post_st post_tr
          in
          let loop_pre   = loop_pre_st ,loop_pre_tr  in
          let loop_post  = loop_post_st,loop_post_tr in
          let block_pre  = Data_for_aorai.get_loop_int_pre  stmt in
          let block_post = Data_for_aorai.get_loop_int_post stmt in



          (* First backward propagation into block
               Pre2 = ([block]Post2) /\ Pre2
               Post2 = Pre2 /\ Post2
          *)
          let old_pre = ref block_pre in
          let block_pre  = propagates_post block.bstmts block_post in
          let block_pre  = double_bool_array_and block_pre !old_pre in
          let block_post = double_bool_array_and block_pre block_post in


          (* Loop initialisation for fix-point computation *)
          let block_pre = ref block_pre in
          let block_post = ref block_post in

          while not (double_bool_array_eq !old_pre !block_pre) do

            old_pre := !block_pre ;
            block_pre  := propagates_post block.bstmts !block_post ;
            block_pre  := double_bool_array_and !block_pre !old_pre ;
            block_post := double_bool_array_and !block_pre !block_post

          done;

          (* The result is dereferenced *)
          let block_pre = !block_pre in
          let block_post = !block_post in


          (* Finally : Pre1  = Pre1 /\ Pre2 *)
          let loop_pre = double_bool_array_and loop_pre block_pre in



          (* Updating loop information *)
          Data_for_aorai.set_loop_ext_pre  stmt loop_pre;
          Data_for_aorai.set_loop_ext_post stmt loop_post;
          Data_for_aorai.set_loop_int_pre  stmt block_pre;
          Data_for_aorai.set_loop_int_post stmt block_post;
          Hashtbl.replace stmts_spec stmt 
            (fst loop_pre, snd loop_pre,fst loop_post, snd block_post);

(*
    Format.printf "\n\nNew loop pre : \n";
    Aorai_utils.debug_display_stmt_all_pre loop_pre ;
    Format.printf "\nNew loop post : \n";
    Aorai_utils.debug_display_stmt_all_pre loop_post;
    Format.printf "\n\nNew loop int_pre : \n";
    Aorai_utils.debug_display_stmt_all_pre block_pre ;
    Format.printf "\nNew loop int_post : \n";
    Aorai_utils.debug_display_stmt_all_pre block_post;
    Format.printf "\n\n";
*)

          (* Computes next statements specification *)
          prop stmt_l loop_pre




      | {skind=UnspecifiedSequence(b)}::l ->
          prop
            ((mkStmt(Block(Cil.block_from_unspecified_sequence(b))))::l)
            (post_st,post_tr)




      | {skind=Switch (_,bl,stmtl,_)}::l  ->

          (* Step 1 : For each case, the pre-condition is set to pre_st,pre_tr. *)
          List.iter
            (fun stmt -> update_stmt_post_OR stmt (post_st,post_tr))
            stmtl;

          (* Step 2 : The block is put into the todo list *)
          prop
            ((mkStmt(Block(bl)))::l)
            (post_st,post_tr)






      | {skind=Break (_)}::_
      | {skind=Continue (_)}::_ ->
                Aorai_option.fatal "Aorai plugin internal error. Status : Goto, Break and Continue instructions are not yet supported.\n";
(*        Format.printf "Aorai plugin internal error. Status : Goto, Break and Continue instructions are not yet supported.\n";*)
(*        assert false                                                                                                               *)
      | {skind=TryFinally (_,_,_) }::_
      | {skind=TryExcept(_,_,_,_)}::_ ->
                Aorai_option.fatal "Aorai plugin internal error. Status : try constructions are not yet supported.\n";
(*        Format.printf "Aorai plugin internal error. Status : try constructions are not yet supported.\n";*)
(*        assert false                                                                                           *)


    in
    (* This computation is done from end to beginning *)
    prop (List.rev stmt_l) (post_st,post_tr)
  in



object (self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    currentFuncName:=f.svar.vname;
    let kf = Extlib.the self#current_kf in
    let starting_pre  = (Data_for_aorai.get_func_pre f.svar.vname) in
    let starting_post = (Data_for_aorai.get_func_post f.svar.vname) in

    Aorai_option.debug "Before step 1:";
    Aorai_utils.debug_display_func_status f.svar.vname;


    Hashtbl.clear stmts_spec;
    Hashtbl.clear stmts_to_compute_one_more_time;
    propagation_result := (mk_empty_pre_or_post ());

    (* Pre-condition forward propagation *)
    let cur_pre = (Data_for_aorai.get_func_pre f.svar.vname) in
    let _ = propagates_pre f.sbody.bstmts cur_pre (*lastFuncStatusSet*) in
    let cur_post_st = ref (fst !propagation_result) in
    while (Hashtbl.length stmts_to_compute_one_more_time) > 0 do
      let _ = propagates_pre f.sbody.bstmts cur_pre in
      cur_post_st := fst (!propagation_result)
    done;
    (* Registration of the new post-condition *)
    let post     = Aorai_utils.get_next kf Promelaast.Return !cur_post_st in
    let old_post = (Data_for_aorai.get_func_post f.svar.vname)  in
    let post     = double_bool_array_and post old_post in

    Data_for_aorai.set_func_post f.svar.vname post;

    Aorai_option.debug "Between steps 1 and 2 : ";
    Aorai_utils.debug_display_func_status f.svar.vname;

    (* Post-condition backward propagation *)
    let cur_post = (Data_for_aorai.get_func_post f.svar.vname) in
    let cur_post = Aorai_utils.get_prev kf Promelaast.Return cur_post in
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
    let old_pre = Data_for_aorai.get_func_pre f.svar.vname in
    let pre     = double_bool_array_and cur_pre old_pre in

    Data_for_aorai.set_func_pre f.svar.vname pre;

    Aorai_option.debug "After step 2 for function %s" f.svar.vname;
    Aorai_utils.debug_display_func_status f.svar.vname;

    let ending_pre  = (Data_for_aorai.get_func_pre f.svar.vname) in
    let ending_post = (Data_for_aorai.get_func_post f.svar.vname) in

    if   (not (double_bool_array_eq starting_pre  ending_pre ) )
    then begin spec_modified:=true;  end;
    if   (not (double_bool_array_eq starting_post ending_post) )
    then begin spec_modified:=true;  end;

    DoChildren
end

let propagates_pre_post_constraints file root =
  Hashtbl.clear functions_pre_usecase ;
  Hashtbl.clear functions_post_usecase;
  spec_modified:=false;

  let visitor = 
    new visit_propagating_pre_post_constraints (Data_for_aorai.getAutomata()) 
  in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file;

  List.iter
    (fun name ->
       if name <> root then
	 begin
	   let old_pre  = (Data_for_aorai.get_func_pre name) in
	   let old_post = (Data_for_aorai.get_func_post name) in
	   let pre = 
             try Hashtbl.find functions_pre_usecase  name 
             with Not_found -> (mk_empty_pre_or_post())
           in
	   let post = 
             try Hashtbl.find functions_post_usecase name 
             with Not_found -> (mk_empty_pre_or_post()) 
           in
           let cur_pre  = double_bool_array_and pre  old_pre  in
           let cur_post = double_bool_array_and post old_post in
           if   (not (double_bool_array_eq old_pre  cur_pre ) )
           then begin spec_modified:=true;  end;
           if   (not (double_bool_array_eq old_post cur_post) )
           then begin spec_modified:=true;  end;
           Data_for_aorai.set_func_pre  name cur_pre;
           Data_for_aorai.set_func_post name cur_post
         end
    )
    (Data_for_aorai.getFunctions_from_c ());
  !spec_modified

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
