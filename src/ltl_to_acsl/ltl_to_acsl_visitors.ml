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

(* $Id: ltl_to_acsl_visitors.ml,v 1.3 2008/10/06 08:56:08 uid588 Exp $ *)

open Cil_types
open Cil
open Cilutil
open Ast_info


(** 
  This visitor does not modify the AST.
  It just generates a first abstract specification for each function.
  This specification is stored into Data_for_ltl and can be accessed by using get_func_pre or get_func_post.
*)
class visit_computing_abstract_pre_post_from_buch (auto:Promelaast.buchautomata) (root:string)  =

object (*(self) *)  
  inherit Visitor.generic_frama_c_visitor  
    (Project.current ()) (Cil.inplace_visit ()) as super 

  method vfunc f =  
(* Extraction of a first abstraction of pre/post condition of the current function *)
    let pre_st,pre_tr  = (Ltl_utils.mk_asbstract_pre  auto f.svar.vname) in
    let post = (Ltl_utils.mk_asbstract_post auto f.svar.vname) in
    

    
    if f.svar.vname = root then
      begin
	List.iter (
	  fun tr -> 
	    if ((pre_tr.(tr.Promelaast.numt)) || pre_st.(tr.Promelaast.stop.Promelaast.nums)) &&
	      ((tr.Promelaast.start.Promelaast.init==Bool3.False) || not (Ltl_utils.isCrossableAtInit tr root) )
	    then 
	      begin 
		pre_tr.(tr.Promelaast.numt)<- false;
		pre_st.(tr.Promelaast.stop.Promelaast.nums)<- false
	      end
	) ((snd auto):Promelaast.trans list);
	

	List.iter (
	  fun tr -> 
	    if (pre_tr.(tr.Promelaast.numt)) then
	      pre_st.(tr.Promelaast.stop.Promelaast.nums) <- true
	) ((snd auto):Promelaast.trans list);
      end;

    Data_for_ltl.set_func_pre  f.svar.vname (pre_st,pre_tr) ;
    Data_for_ltl.set_func_post f.svar.vname post; 
    DoChildren    
end 







(** 
  This visitor add a ghost code before each call and return functions in order to compute the modification of the buchi automata.
*)
class visit_adding_code_for_synchronisation (auto:Promelaast.buchautomata) =
  let current_function = ref "" in
  let get_call_name exp = 
    match exp with
      | Const(CStr(s)) -> s
      | Lval(Var(vi),NoOffset) -> vi.vname
      | _ -> Format.printf "At this time, only explicite call are allowed by the Ltl_to_acsl plugin.\n"; assert(false)
  in
object (*(self) *)  
  inherit Visitor.generic_frama_c_visitor  
    (Project.current ()) (Cil.inplace_visit ()) as super 

  method vfunc f = 
    current_function := f.svar.vname;
    DoChildren


  method vstmt_aux stmt = 
    match stmt.skind with 
      | Return (_,loc)  ->
	  let sync_inst_l = Ltl_utils.synch_upd auto (!current_function) Promelaast.Return loc in
	  let new_return = mkStmt stmt.skind in 
	  new_return.sid<-(Cil.Sid.next ());
	  let new_stmts = 
	    List.fold_left
	      (fun stmt_l inst -> 
		 let n_stmt=(Cil.mkStmtOneInstr inst) in
		 n_stmt.sid<-(Cil.Sid.next ());
		 n_stmt::stmt_l
	      )
	      [new_return]
	      sync_inst_l
	    
	  in
  	    stmt.skind<-Block(Cil.mkBlock(new_stmts));
	    SkipChildren



      (* This second treatment can be done easierly with vinst method, but sid is then set to -1 *)
      | Instr(Call (_,funcexp,_,loc)) ->  
	  let sync_inst_l = Ltl_utils.synch_upd auto (get_call_name funcexp) Promelaast.Call loc in
	  let new_call = mkStmt stmt.skind in 
	  new_call.sid<-(Cil.Sid.next ());
	  let new_stmts = 
	    List.fold_left
	      (fun stmt_l inst -> 
		 let n_stmt=(Cil.mkStmtOneInstr inst) in
		 n_stmt.sid<-(Cil.Sid.next ());
		 n_stmt::stmt_l
	      )
	      [new_call]
	      sync_inst_l
	    
	  in
  	    stmt.skind<-Block(Cil.mkBlock(new_stmts));
	    SkipChildren



      | _ -> DoChildren

end 






let post_treatment_loops = Hashtbl.create 97;

(** 
  This visitor add a specification to each fonction and to each loop, according to specifications stored into Data_for_ltl.
*)
class visit_adding_pre_post_from_buch (auto:Promelaast.buchautomata)  =


  let predicate_to_invariant ref_stmt pred =
    Annotations.add !ref_stmt (Db_types.Before(Db_types.User (Logic_const.new_code_annotation (AInvariant([],true,pred)))))
  in

      
  (** Given a couple of bool array (States , Transitions), 
      this function computes a predicate and add it as an invariant.
  *)
  let condition_to_invariant cond stmt_ref =
    let pred_l = Ltl_utils.pre_post_to_term cond in
    let pred = Ltl_utils.mk_conjunction_named pred_l in
    predicate_to_invariant stmt_ref pred
  (*let inv = Logic_const.new_code_annotation  (AInvariant([],true,pred)) in
    Annotations.add !stmt_ref (Db_types.Before(Db_types.User inv))*)
  in

  
  (** Given the number of states a by-case post-condition and a state number, 
      it returns a bool array with nb_states cells. 
      A cell is true if and only if the associated post-condition is 
      equivalent to the one of the given state. *)
  let get_other_states_with_equivalent_post nb_states (post_bc_st,post_bc_tr) index =
    let eq_states=Array.make nb_states false in
    eq_states.(index)<-true;
    Array.iteri
      (fun i post_st -> 
	 if i<>index &&
	   (Ltl_utils.bool_array_eq post_st post_bc_st.(index))&&
	   (Ltl_utils.bool_array_eq post_bc_tr.(i) post_bc_tr.(index)) (* Toujours Faux tq l'on veut eq des tr *)
	 then
	   eq_states.(i)<-true;
      )
      post_bc_st;
    eq_states
  in
  

    
object (*(self) *)

  inherit Visitor.generic_frama_c_visitor  
    (Project.current ()) (Cil.inplace_visit ()) as super

  method vfunc f =
    let spec= Kernel_function.get_spec (Globals.Functions.get f.svar) in

(* Rewriting arrays carracterizing status into predicates *)
    let preds_pre  = Ltl_utils.pre_post_to_term (Data_for_ltl.get_func_pre  f.svar.vname) in
    let preds_post_bc = Data_for_ltl.get_func_post_bycase f.svar.vname in

(* Registration of the new specification *)

(*   + Pre-condition registration *)     
    List.iter 
      (fun p -> spec.spec_requires <- ((Logic_const.new_predicate p) :: (spec.spec_requires)) )
      preds_pre;


(*   + Post-condition registration *) 
(*      If several states are associated to the same post-condition, 
	then their specification is factorised. *)
    let nb_states=Data_for_ltl.getNumberOfStates() in
    let treated=ref (Array.make nb_states false) in
    Array.iteri
      (fun case preds_post -> 
	 
	 if   (not (Ltl_utils.is_empty_behavior preds_post) )
	   && (not (!treated).(case))
	 then begin
	   let new_behavior = 
	     {
   	       b_name = "Buchi_property_behavior_"^(string_of_int case); (*	name of the behavior.	*)
   	       b_assumes = [] ;                    (*	assume clauses.	*)
   	       b_ensures = [] ;                    (*	ensure clauses.	*)
   	       b_assigns = [] ;                    (*	assignments.	*)
	     } 
	   in

	   let all_eqs_states = get_other_states_with_equivalent_post nb_states preds_post_bc case in
	   let assumes_l = ref [] in
	   Array.iteri
	     (fun i b -> if b then 
		assumes_l:=Logic_const.prel(
		  Rneq, 
		  Ltl_utils.zero_term(), 
		  Ltl_utils.mk_offseted_array 
		    (Logic_const.lval_to_term_lval (Cil.var (Data_for_ltl.get_varinfo Data_for_ltl.curState))) 
		    i
		)::!assumes_l
	     )
	     all_eqs_states;
	   new_behavior.b_assumes<-[Logic_const.new_predicate (Ltl_utils.mk_disjunction_named !assumes_l)];
	   treated:=Ltl_utils.bool_array_or !treated all_eqs_states;

	   (*
	   new_behavior.b_assumes<- 
	     [Logic_const.new_predicate
		(Logic_const.prel( 
		   Rneq, 
		   Ltl_utils.zero_term(), 
		   Ltl_utils.mk_offseted_array  
		     (Logic_const.lval_to_term_lval (Cil.var (Data_for_ltl.get_varinfo Data_for_ltl.curState))) 
		     case))];*)
	   
	   
	   
	   let preds_list = Ltl_utils.pre_post_to_term (preds_post,(snd preds_post_bc).(case)) in
	   List.iter 
	     (fun p -> new_behavior.b_ensures <- ((Logic_const.new_predicate p) :: (new_behavior.b_ensures)) )
	     preds_list;
	   spec.spec_behavior <- new_behavior::spec.spec_behavior

	 end
      )
      (fst preds_post_bc);
(*      spec.spec_complete_behaviors <- new_behavior.b_name::spec.spec_complete_behaviors;*)


    DoChildren  








  method vstmt_aux stmt = 
    let treat_loop body_ref stmt =
     
      (* varinfo of the init_var associated to this loop *)
      let vi_init = Data_for_ltl.get_varinfo (Data_for_ltl.loopInit^"_"^(string_of_int stmt.sid)) in 


(*    1) The associated init variable is set to 0 in first position 
         (or in second position if the first stmt is a if)*)

      let stmt_varset = Cil.mkStmtOneInstr (Set((Var(vi_init),NoOffset), Ltl_utils.mk_int_exp 0, Cilutil.locUnknown)) in
      stmt_varset.sid<-(Cil.Sid.next ());
      stmt_varset.ghost<-true;

      begin
        (* Function adaptated from the cil printer *)
        try
	  let rec skipEmpty = function
              [] -> []
            | {skind=Instr (Skip _);labels=[]} :: rest -> skipEmpty rest
            | x -> x
          in

          match skipEmpty !body_ref.bstmts with
            | {skind=If(_,tb,fb,_)} as head:: _ ->
                begin
                  match skipEmpty tb.bstmts, skipEmpty fb.bstmts with
                    | [], {skind=Break _}:: _  
                    | [], {skind=Goto _} :: _  
                    | {skind=Goto _} :: _, [] 
                    | {skind=Break _} :: _, [] -> 
			!body_ref.bstmts<-head::(stmt_varset::(List.tl !body_ref.bstmts))
			  
		    | _ -> 
			raise Not_found
                end
            | _ -> raise Not_found 
		
	with 
	  | Not_found -> 
	      !body_ref.bstmts<-stmt_varset::!body_ref.bstmts
      end;


(*    2) The associated init variable is set to 1 before the loop *)
      let new_loop = mkStmt stmt.skind in 
      new_loop.sid<-(Cil.Sid.next ());
      let stmt_varset = Cil.mkStmtOneInstr (Set((Var(vi_init),NoOffset), Ltl_utils.mk_int_exp 1, Cilutil.locUnknown)) in
      stmt_varset.sid<-(Cil.Sid.next ());
      stmt_varset.ghost<-true;
      let block = mkBlock [stmt_varset;new_loop] in
      stmt.skind<-Block(block);
      


(*    3) Generation of the loop invariant *)
(* The loop invariant is : 
     (c  => Pre2)
   & (!c => Post1) !! -> Since loops are while (1), this case seems to be useless
   & (Init => Pre1)
   & (not Init => Post2)
   (init : fresh variable which indicates if the iteration is the first one).
   (c :  fresh variable allowing multiple read of the condition even if its expression includes some side effects.)
*)

      let global_loop_inv = 
	Ltl_utils.double_bool_array_or 
	  (Ltl_utils.double_bool_array_or 
	     (Data_for_ltl.get_loop_int_pre (ref stmt))
	     (Data_for_ltl.get_loop_ext_pre (ref stmt)))
	  (Data_for_ltl.get_loop_int_post (ref stmt))
      in
      condition_to_invariant global_loop_inv (ref new_loop);

      let p1 = Ltl_utils.force_condition_to_predicate global_loop_inv (Data_for_ltl.get_loop_int_pre (ref stmt))  in
      if p1<>Cil_types.Ptrue then
	predicate_to_invariant (ref new_loop) (Logic_const.unamed p1);

(*      force_condition_to_predicate global_loop_inv (Data_for_ltl.get_loop_ext_post (ref stmt))*)

      let p2= Ltl_utils.force_condition_to_predicate global_loop_inv (Data_for_ltl.get_loop_ext_pre (ref stmt)) in
      if p2<>Cil_types.Ptrue then
	predicate_to_invariant 
	  (ref new_loop) 
	  (Logic_const.unamed (Pimplies (Logic_const.unamed (Prel(Rneq,Ltl_utils.mk_term_from_vi vi_init,Ltl_utils.zero_term())),Logic_const.unamed p2)));
      
      let p3= Ltl_utils.force_condition_to_predicate global_loop_inv (Data_for_ltl.get_loop_int_post (ref stmt)) in
      if p3<>Cil_types.Ptrue then
	predicate_to_invariant 
	  (ref new_loop) 
	  (Logic_const.unamed (Pimplies (Logic_const.unamed (Prel(Req,Ltl_utils.mk_term_from_vi vi_init,Ltl_utils.zero_term())),Logic_const.unamed p3)));
      


(*    4) Keeping old annotations *)
      Hashtbl.add post_treatment_loops (ref stmt) (ref new_loop); 


(*    5) Updated stmt is returned *)
	  stmt
    in 

    match stmt.skind with 
      | Loop (_,block,_,_,_) ->  
	  ChangeDoChildrenPost(stmt, treat_loop (ref block))
	    
      | _ -> DoChildren
	   
  
end






(* Call of the visitors *)
let compute_abstract file root  = 
  let visitor = new visit_computing_abstract_pre_post_from_buch (Data_for_ltl.getAutomata()) (root)  in
    Cil.visitCilFile (visitor :> Cil.cilVisitor) file




let add_pre_post_from_buch file  = 
  let visitor = new visit_adding_pre_post_from_buch (Data_for_ltl.getAutomata()) in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file;

  (* Transfert previous annotation on the new loop statement *)
  Hashtbl.iter 
    (fun old_stmt new_stmt -> 
       let old_annot_l = Annotations.get !old_stmt in
       Annotations.reset_stmt !old_stmt;
       
       List.iter 
	 (fun an -> Annotations.add !new_stmt an)
	 old_annot_l
    )
    post_treatment_loops 
      





let add_sync_with_buch file  = 
  let visitor = new visit_adding_code_for_synchronisation (Data_for_ltl.getAutomata()) in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
