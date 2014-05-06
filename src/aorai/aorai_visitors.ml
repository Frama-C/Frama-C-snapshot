(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Promelaast
open Extlib
open Logic_const
open Cil_types
open Cil

(**************************************************************************)

let dkey = Aorai_option.register_category "action"

let get_acceptance_pred () =
  let (st,_) = Data_for_aorai.getAutomata () in
  List.fold_left
    (fun acc s ->
      match s.acceptation with
          Bool3.True -> Logic_const.por (acc, Aorai_utils.is_state_pred s)
        | Bool3.False | Bool3.Undefined -> acc)
    Logic_const.pfalse st

let get_call_name exp = match exp.enode with
  | Const(CStr(s)) -> s
  | Lval(Var(vi),NoOffset) -> vi.vname
  | _ ->
    Aorai_option.not_yet_implemented
      "At this time, only explicit calls are allowed by the Aorai plugin."

(****************************************************************************)

(* The instrumentation is done in two passes:
   1) creating auxiliary functions for each non-ignored C function, that update
   automaton's state when entering and exiting the function
   2) generating specifications for all the functions.

   We maintain tables from aux to orig so that the second visitor knows which
   is which. Note that this tables are cleared after each visit, and thus need
   not be projectified.
*)

(* the various kinds of auxiliary functions. *)
type func_auto_mode =
    Not_auto_func (* original C function. *)
  | Pre_func of kernel_function (* Pre_func f denotes a function updating
                                   the automaton before call to f. *)
  | Post_func of kernel_function (* Post_func f denotes a function updating
                                    the automaton when returning from f. *)

(* table from auxiliary functions to the corresponding original one. *)
let func_orig_table = Cil_datatype.Varinfo.Hashtbl.create 17

let kind_of_func vi =
  try Cil_datatype.Varinfo.Hashtbl.find func_orig_table vi
  with Not_found -> Not_auto_func

(**
   This visitor adds an auxiliary function for each C function which takes
   care of setting the automaton in a correct state before calling the
   original one, and replaces each occurrence of the original function by
   the auxiliary one. It also takes care of changing the automaton at function's
   return.
*)
class visit_adding_code_for_synchronisation =
object (self)
  inherit Visitor.frama_c_inplace

  val aux_post_table = Kernel_function.Hashtbl.create 17

  method! vglob_aux g =
    match g with
    | GFun (fundec,loc) ->
      let kf = Extlib.the self#current_kf in
      let vi = Kernel_function.get_vi kf in
      let vi_pre = Cil_const.copy_with_new_vid vi in
      vi_pre.vname <- Data_for_aorai.get_fresh (vi_pre.vname ^ "_pre_func");
      vi_pre.vdefined <- false;
      Cil_datatype.Varinfo.Hashtbl.add func_orig_table vi_pre (Pre_func kf);
        (* TODO:
           - what about protos that have no specified args
           (NB: cannot be identified here because of implem of Kernel_function).
           - what about varargs?
         *)
      let (rettype,args,varargs,_) = Cil.splitFunctionTypeVI vi_pre in
      vi_pre.vtype <- TFun(Cil.voidType, args, varargs,[]);
      vi_pre.vattr <- [];
        (* in particular get rid of __no_return if set in vi*)
      let arg =
        if Cil.isVoidType rettype
        then []
        else ["res",rettype,[]]
      in
      let vi_post =
        Cil.makeGlobalVar
          (Data_for_aorai.get_fresh (vi.vname ^ "_post_func"))
          (TFun(voidType,Some arg,false,[]))
      in
      Kernel_function.Hashtbl.add aux_post_table kf vi_post;
      Cil_datatype.Varinfo.Hashtbl.add func_orig_table vi_post (Post_func kf);
      let globs =
        [ GVarDecl(Cil.empty_funspec (), vi_pre, loc);
          GVarDecl(Cil.empty_funspec (), vi_post,loc) ]
      in
      fundec.sbody.bstmts <-
        Cil.mkStmtOneInstr
        (Call(None,Cil.evar ~loc vi_pre,
              List.map (fun x -> Cil.evar ~loc x)
                (Kernel_function.get_formals kf),
              loc))
      :: fundec.sbody.bstmts;
      Globals.Functions.replace_by_declaration 
        (Cil.empty_funspec ()) vi_pre loc;
      Globals.Functions.replace_by_declaration
        (Cil.empty_funspec()) vi_post loc;
      ChangeDoChildrenPost([g], fun x -> globs @ x)
    | _ -> DoChildren

  method! vstmt_aux stmt =
    match stmt.skind with
      | Return (res,loc)  ->
        let kf = Extlib.the self#current_kf in
        let vi = Kernel_function.get_vi kf in
        let current_function = vi.vname in
        if not (Data_for_aorai.isIgnoredFunction current_function) then begin
          let args = match res with
            | None -> []
            | Some exp -> [Cil.copy_exp exp]
          in
          let aux_vi = Kernel_function.Hashtbl.find aux_post_table kf in
          let call =
            mkStmtOneInstr (Call (None,Cil.evar ~loc aux_vi,args,loc))
          in
          let new_return = mkStmt ~valid_sid:true stmt.skind in
          let new_stmts = [call; new_return] in
          stmt.skind<-Block(Cil.mkBlock(new_stmts))
        end;
        SkipChildren
      | _ -> DoChildren

end

(*********************************************************************)

(* update from formals of original C function to one of the auxiliary
   function (f_aux or f_pre)
 *)
class change_formals old_kf new_kf =
  let old_formals = Kernel_function.get_formals old_kf in
  let new_formals = Kernel_function.get_formals new_kf in
  let formals = List.combine old_formals new_formals in
object
  inherit Visitor.frama_c_inplace
  method! vlogic_var_use lv =
    match lv.lv_origin with
      | None -> SkipChildren
      | Some vi ->
        try
          let vi'= List.assq vi formals in
          ChangeTo (Cil.cvar_to_lvar vi')
        with Not_found -> SkipChildren
end

(* update \result to param of f_post when it exists. Must not be called if
   f_post has no parameter (original f returns void). *)
class change_result new_kf =
  let v = List.hd (Kernel_function.get_formals new_kf) in
object
  inherit Visitor.frama_c_inplace
  method! vterm_lhost lh =
    match lh with
        TResult _ -> ChangeTo (TVar (Cil.cvar_to_lvar v))
      | _ -> DoChildren
end

let post_treatment_loops = Hashtbl.create 97

let update_loop_assigns kf stmt state vi code_annot =
  let loc = Cil_datatype.Stmt.loc stmt in
  let assigns = Aorai_utils.aorai_assigns state loc in
  let assigns =
    Logic_utils.concat_assigns 
      (Writes
         [Logic_const.new_identified_term (Logic_const.tvar ~loc vi), From []])
      assigns
  in
  let new_assigns =
    match code_annot.annot_content with
      | AAssigns (bhvs,old_assigns) ->
          Logic_const.new_code_annotation
            (AAssigns (bhvs, Logic_utils.concat_assigns old_assigns assigns))
      | _ -> Aorai_option.fatal "Expecting an assigns clause here"
  in
  Annotations.add_code_annot Aorai_option.emitter ~kf stmt new_assigns

let get_action_post_cond kf ?init_trans return_states =
  let to_consider pre_state int_states =
    match init_trans with
      | None -> true
      | Some init_trans ->
        try
          let possible_states =
            Data_for_aorai.Aorai_state.Map.find pre_state init_trans
          in
          not (Data_for_aorai.Aorai_state.Set.is_empty
                 (Data_for_aorai.Aorai_state.Set.inter
                    int_states possible_states))
        with Not_found -> false
  in
  let treat_one_path pre_state post_state (int_states,_,bindings) acc =
    if to_consider pre_state int_states then begin
      let start = Logic_const.pre_label in
      let post_conds =
        Aorai_utils.action_to_pred ~start ~pre_state ~post_state bindings
      in
      Aorai_option.debug ~dkey
        "Getting action post-conditions for %a, from state %s to state %s@\n%a"
        Kernel_function.pretty kf
        pre_state.Promelaast.name post_state.Promelaast.name
        (Pretty_utils.pp_list ~sep:"@\n" Printer.pp_predicate_named)
        post_conds;
      post_conds @ acc
    end
    else acc
  in
  let treat_one_pre_state pre_state map acc =
    Data_for_aorai.Aorai_state.Map.fold (treat_one_path pre_state) map acc
  in
  let post_cond =
    Data_for_aorai.Aorai_state.Map.fold treat_one_pre_state return_states []
  in
  List.map
    (fun post_cond -> (Normal, Logic_const.new_predicate post_cond))
    post_cond

let make_zero_one_choice reachable_states =
  let treat_one_state state _ acc =
    (Logic_const.por
       (Aorai_utils.is_state_pred state,
        Aorai_utils.is_out_of_state_pred state)) :: acc
  in
  Data_for_aorai.Aorai_state.Map.fold treat_one_state reachable_states []

let needs_zero_one_choice states =
  let needs_choice =
    try
      ignore
        (Data_for_aorai.Aorai_state.Map.fold
           (fun _ _ flag -> if flag then raise Exit else true)
           states false);
      false
    with Exit -> true
  in
  if needs_choice then 
    List.map 
      Logic_const.new_predicate 
      (make_zero_one_choice states)
  else []

let pred_reachable reachable_states =
  let treat_one_state (nb, reachable, unreachable) state =
    if Data_for_aorai.Aorai_state.Map.mem state reachable_states then
      (nb+1,
       Logic_const.por (reachable, Aorai_utils.is_state_pred state),
       unreachable)
    else
      (nb, reachable,
       Logic_const.pand (unreachable, Aorai_utils.is_out_of_state_pred state))
  in
  let (states,_) = Data_for_aorai.getAutomata () in
  let (nb, reachable, unreachable) =
    List.fold_left treat_one_state (0,pfalse,ptrue) states
  in
  (nb > 1, reachable, unreachable)

let possible_start kf (start,int) =
  let auto = Data_for_aorai.getAutomata () in
  let trans = Path_analysis.get_edges start int auto in
  let treat_one_trans cond tr =
    Logic_const.por
      (cond, Aorai_utils.crosscond_to_pred (fst tr.cross) kf Promelaast.Call)
  in
  let cond = List.fold_left treat_one_trans Logic_const.pfalse trans in
  Logic_const.pand (Aorai_utils.is_state_pred start, cond)

let neg_trans kf trans =
  let auto = Data_for_aorai.getAutomata () in
  let rec aux l acc =
    match l with
      | [] -> acc
      | (start,stop) :: l ->
        let same_start, rest =
          List.fold_left 
            (fun (same_start, rest) (start', stop' as elt) -> 
                  if Data_for_aorai.Aorai_state.equal start start' then 
                    stop' :: same_start, rest
                  else
                    same_start, elt :: rest)
            ([stop],[]) l
        in
        let cond =
          List.fold_left
            (fun cond stop ->
              let trans = Path_analysis.get_edges start stop auto in
              List.fold_left
                (fun cond tr -> 
                  Logic_simplification.tand 
                    cond (Logic_simplification.tnot (fst tr.cross)))
                cond trans)
            TTrue same_start
        in
        let cond = fst (Logic_simplification.simplifyCond cond) in
        let cond = Aorai_utils.crosscond_to_pred cond kf Promelaast.Call in
        let cond = 
          Logic_const.por (Aorai_utils.is_out_of_state_pred start, cond)
        in
        aux rest (Logic_const.pand (acc,cond))
  in
  aux trans Logic_const.ptrue

let get_unchanged_aux_var loc current_state =
  let partition_action state (_,_,map) (actions,possible_states) =
    let possible_states =
      Data_for_aorai.Aorai_state.Set.add state possible_states
    in
    let treat_one_action t _ acc =
      let states =
        try Cil_datatype.Term.Map.find t acc
        with Not_found -> Data_for_aorai.Aorai_state.Set.empty
      in
      Cil_datatype.Term.Map.add t
        (Data_for_aorai.Aorai_state.Set.add state states) acc
    in
    let actions =
      Cil_datatype.Term.Map.fold treat_one_action map actions
    in (actions,possible_states)
  in
  let treat_one_action pre_hyp possible_states t action_states acc =
    if not (Data_for_aorai.Aorai_state.Set.is_empty
              (Data_for_aorai.Aorai_state.Set.diff 
                 possible_states action_states))
    then begin
      let post_hyp =
        Data_for_aorai.Aorai_state.Set.fold
          (fun st acc ->
            Logic_const.pand ~loc (acc,Aorai_utils.is_out_of_state_pred st))
          action_states Logic_const.ptrue
      in
      let pred =
        Logic_const.new_predicate
          (Logic_const.pimplies ~loc 
             (pre_hyp,
              Logic_const.pimplies ~loc
                (post_hyp,
                 Logic_const.prel ~loc (Req,t,Logic_const.told ~loc t))))
      in
      (Normal,pred) :: acc
    end else acc
  (* all possible states will update this lval, no need to
     make a special case here.
   *)
  in
  let treat_one_pre_state start map acc =
    let pre_hyp = Logic_const.pold ~loc (Aorai_utils.is_state_pred start) in
    let actions_map, possible_states =
      Data_for_aorai.Aorai_state.Map.fold
        partition_action map 
        (Cil_datatype.Term.Map.empty, Data_for_aorai.Aorai_state.Set.empty)
    in
    Cil_datatype.Term.Map.fold 
      (treat_one_action pre_hyp possible_states) actions_map acc
  in
  Data_for_aorai.Aorai_state.Map.fold treat_one_pre_state current_state []

(**
   This visitor adds a specification to each fonction and to each loop,
   according to specifications stored into Data_for_aorai.
*)
class visit_adding_pre_post_from_buch treatloops =

  let predicate_to_invariant kf stmt pred =
    Annotations.add_code_annot
      Aorai_option.emitter
      ~kf
      stmt
      (Logic_const.new_code_annotation (AInvariant([],true,pred)));
  in
  let all_possible_states state =
    let treat_one_state _ = Data_for_aorai.merge_end_state in
    Data_for_aorai.Aorai_state.Map.fold
      treat_one_state state Data_for_aorai.Aorai_state.Map.empty
  in
  let condition_to_invariant kf possible_states stmt =
    (* Checks whether we have at least two possible automaton's states in the
       invariant. *)
    let has_multiple_choice =
      try
        ignore
          (Data_for_aorai.Aorai_state.Map.fold
             (fun _ _ b -> if b then raise Exit else true)
             possible_states false);
        false
      with Exit -> 
	true
    in
    let treat_one_state s =
      if Data_for_aorai.Aorai_state.Map.mem s possible_states then begin
        if has_multiple_choice then begin
          let pred =
            Logic_const.por
              (Aorai_utils.is_state_pred s, Aorai_utils.is_out_of_state_pred s)
          in
          predicate_to_invariant kf stmt pred
        end else begin
          (* We can only be in one state. Since we must be in at least one
             state, the invariant is quite simple.
           *)
          predicate_to_invariant kf stmt (Aorai_utils.is_state_pred s)
        end
      end else begin
        let pred = Aorai_utils.is_out_of_state_pred s in
        predicate_to_invariant kf stmt pred
      end
    in
    let (states,_) = Data_for_aorai.getAutomata () in
    List.iter treat_one_state states;
    if has_multiple_choice then begin
      let add_possible_state state _ acc =
        if Data_for_aorai.is_reject_state state then acc
        else
          Logic_const.por (acc,Aorai_utils.is_state_pred state)
      in
      let pred =
        Data_for_aorai.Aorai_state.Map.fold
          add_possible_state possible_states Logic_const.pfalse
      in
      predicate_to_invariant kf stmt pred
    end
  in
  let impossible_states_preds start possible_states my_state =
    let treat_one_start_state state start_state end_states acc =
      if not (Data_for_aorai.Aorai_state.Map.mem state end_states) then
        Logic_const.pimplies
          (Logic_const.pat(Aorai_utils.is_state_pred start_state, start),
           Aorai_utils.is_out_of_state_pred state)
        :: acc
      else acc
    in
    let treat_one_state state _ acc =
      Data_for_aorai.Aorai_state.Map.fold
        (treat_one_start_state state) my_state acc
    in
    Data_for_aorai.Aorai_state.Map.fold treat_one_state possible_states []
  in
  let impossible_states_preds_inv start possible_states my_state =
    let treat_one_start_state state start_state end_states acc =
      if Data_for_aorai.Aorai_state.Map.mem state end_states then
        Logic_const.pand
          (acc,
           Logic_const.pat(Aorai_utils.is_out_of_state_pred start_state, start))
      else acc
    in
    let treat_one_state state _ acc =
      let out_states =
        Data_for_aorai.Aorai_state.Map.fold
          (treat_one_start_state state) my_state Logic_const.ptrue
      in
      if Data_for_aorai.Aorai_state.Map.cardinal my_state = 1 &&
        not (Logic_utils.is_trivially_true out_states)
      then acc (* we only have a single entry state: we can't possibly be
                  out of it, or another annotation above is invalid. No need
                  to put an implication with a false lhs.
                *)
      else
        Logic_const.pimplies
          (out_states, Aorai_utils.is_out_of_state_pred state)
        ::acc
    in
    Data_for_aorai.Aorai_state.Map.fold treat_one_state possible_states []
  in
  let partition_pre_state map =
    let (states,_) = Data_for_aorai.getAutomata () in
    let is_equiv st1 st2 =
      let check_one _ o1 o2 =
        match o1, o2 with
        | None, None | Some _, Some _ -> Some ()
        | None, Some _ | Some _, None -> raise Not_found
      in
      try
        ignore (Data_for_aorai.Aorai_state.Map.merge check_one st1 st2); true
      with Not_found -> false
    in
    let find_equivs (start,state, end_states) equivs =
      let rec aux = function
        | [] -> [[start,state],end_states]
        | (equiv_class,end_states2 as infos) :: l ->
          if is_equiv end_states end_states2 then
            ((start, state) :: equiv_class, end_states2) :: l
          else infos :: aux l
      in aux equivs
    in
    let filter equivs state =
      let check_one_state start end_states equivs =
        let end_states =
          Data_for_aorai.Aorai_state.Map.filter
            (fun _ (int_states,_,_) ->
              Data_for_aorai.Aorai_state.Set.mem state int_states)
            end_states
        in
        if Data_for_aorai.Aorai_state.Map.is_empty end_states then equivs
        else find_equivs (start, state, end_states) equivs
      in
      Data_for_aorai.Aorai_state.Map.fold check_one_state map equivs
    in
    let res = List.fold_left filter [] states in
    List.map fst res
  in
  let update_assigns loc kf ki spec =
    let update_assigns bhv =
      (* NB: The assigns for a statement contract is a bit overapproximated,
         (includes assigns of the whole function), but we don't really have
         a better information at this point.
       *)
      let assigns =
        Aorai_utils.aorai_assigns (Data_for_aorai.get_kf_return_state kf) loc
      in
      match ki with
        | Kstmt _ -> (* stmt contract *)
	  bhv.b_assigns <- Logic_utils.concat_assigns bhv.b_assigns assigns
        | Kglobal -> (* function contract *)
	  Annotations.add_assigns
	    ~keep_empty:true
	    Aorai_option.emitter
	    kf
	    bhv.b_name
	    assigns;
    in
    List.iter update_assigns spec.spec_behavior
  in
  let mk_auto_fct_spec kf status auto_state =
    let loc = Kernel_function.get_location kf in
    Aorai_utils.auto_func_behaviors loc kf status auto_state
  in
  let mk_pre_fct_spec kf =
    mk_auto_fct_spec kf Promelaast.Call (Data_for_aorai.get_kf_init_state kf)
  in
  let mk_post_fct_spec kf =
    mk_auto_fct_spec kf
      Promelaast.Return (Data_for_aorai.get_kf_return_state kf)
  in
  let needs_post kf =
    let loc = Kernel_function.get_location kf in
    let return_state = Data_for_aorai.get_kf_return_state kf in
    let possible_states =
      Data_for_aorai.Aorai_state.Map.fold
        (fun _ map acc ->
          Data_for_aorai.Aorai_state.Map.fold
            (fun st _ acc -> Data_for_aorai.Aorai_state.Set.add st acc)
            map acc)
        return_state Data_for_aorai.Aorai_state.Set.empty
    in
    let action_post = get_unchanged_aux_var loc return_state in
    if 
      Data_for_aorai.Aorai_state.Set.exists
        Data_for_aorai.is_reject_state possible_states
    then
      (* We must ensure that there is at least one active state
         beside the rejection state *)
      let cond =
        Data_for_aorai.Aorai_state.Set.fold
          (fun st acc ->
            if Data_for_aorai.is_reject_state st then acc
            else Logic_const.por (Aorai_utils.is_state_pred st,acc))
          possible_states Logic_const.pfalse
      in
      (Normal,Logic_const.new_predicate cond) :: action_post
    else action_post
  in
  let mk_post kf =
    let return_state = Data_for_aorai.get_kf_return_state kf in
    (*   + Post-condition registration *)
    (* If several states are associated to the same post-condition,
       then their specification is factorised. *)
    let equivs = partition_pre_state return_state in
    let bhvs =
      match equivs with
      | [ e ] -> (* we just have one possible case, no need to generate
                    assumes and a negative behavior
                  *)
        let name = "Buchi_property_behavior" in
        let s = fst (List.hd e) in
        let reachable_states =
          Data_for_aorai.Aorai_state.Map.find s return_state
        in
        let (multi_choice, reachable, unreachable) =
          pred_reachable reachable_states
        in
        let post_cond = Normal, Logic_const.new_predicate reachable in
        let post_cond =
          if Aorai_option.Deterministic.get () then [post_cond]
          else
            [Normal, Logic_const.new_predicate unreachable; post_cond]
        in
        let post_cond =
          if multi_choice && not (Aorai_option.Deterministic.get ()) then
            begin
              let preds = make_zero_one_choice reachable_states in
              List.fold_left
                (fun acc p ->
                  (Normal, Logic_const.new_predicate p) :: acc)
                post_cond preds
            end
          else post_cond
        in
        let infos = Aorai_utils.get_preds_post_bc_wrt_params kf in
        let post_cond =
          if Logic_utils.is_trivially_true infos then post_cond
          else (Normal, Logic_const.new_predicate infos) :: post_cond
        in
        let post_cond = post_cond @ get_action_post_cond kf return_state in
        [Cil.mk_behavior ~name ~post_cond ()]
      | _ ->
        let _,bhvs =
          List.fold_left
            (fun (i,acc) equiv ->
              let (case_start, case_int) = List.hd equiv in
              let assumes_l =
                List.map (possible_start kf) equiv
              in
              let name = "Buchi_behavior_in_" ^ (string_of_int i) in
              let assumes =
                [Logic_const.new_predicate (Logic_const.pors assumes_l)]
              in
              let reachable_states =
                Data_for_aorai.Aorai_state.Map.find case_start return_state
              in
              let reachable_states =
                Data_for_aorai.Aorai_state.Map.filter
                  (fun _ (int,_,_) ->
                    Data_for_aorai.Aorai_state.Set.mem case_int int)
                  reachable_states
              in
              let (multi_choice, reachable, _) =
                pred_reachable reachable_states
              in
              let post_cond =
                [Normal, Logic_const.new_predicate reachable]
              in
              let post_cond =
                if multi_choice && not (Aorai_option.Deterministic.get()) then
                  begin
                    let preds = make_zero_one_choice reachable_states in
                    List.fold_left
                      (fun acc p ->
                        (Normal, Logic_const.new_predicate p) :: acc)
                      post_cond preds
                  end
                else post_cond
              in
              let infos = Aorai_utils.get_preds_post_bc_wrt_params kf in
              let post_cond =
                if Logic_utils.is_trivially_true infos then post_cond
                else (Normal, Logic_const.new_predicate infos) :: post_cond
              in
              let init_trans =
                List.fold_left
                  (fun acc (start, int) ->
                    let set =
                      try Data_for_aorai.Aorai_state.Map.find start acc
                      with Not_found -> Data_for_aorai.Aorai_state.Set.empty
                    in
                    Data_for_aorai.Aorai_state.Map.add
                      start
                      (Data_for_aorai.Aorai_state.Set.add int set)
                      acc)
                  Data_for_aorai.Aorai_state.Map.empty
                  equiv
              in
              let post_cond =
                post_cond @
                  (get_action_post_cond kf ~init_trans return_state)
              in
              (i+1,
               Cil.mk_behavior ~name ~assumes ~post_cond () :: acc))
            (0,[])
            equivs
        in
        if Aorai_option.Deterministic.get () then bhvs
        else begin
          (* post-conditions for state in which we are not at the
             end of the functions. They have to be grouped differently
             than positive information because of non-determinism (if two
             non-equivalent states are active when entering the function
             and activate the same state at exit) *)
          let aux (i,bhvs) state =
            let name = "Buchi_behavior_out_" ^ (string_of_int i) in
            let select_equivalence_class equiv =
              let (start, int) = List.hd equiv in
              try
                let map =
                  Data_for_aorai.Aorai_state.Map.find start return_state
                in
                let (int_states, _,_) =
                  Data_for_aorai.Aorai_state.Map.find state map
                in
                Data_for_aorai.Aorai_state.Set.mem int int_states
              with Not_found -> false
            in
            let my_trans =
              List.fold_left
                (fun acc equiv ->
                  if select_equivalence_class equiv then
                    acc @ equiv
                  else acc)
                [] equivs
            in
            let assumes = neg_trans kf my_trans in
            if Logic_utils.is_trivially_false assumes then (i+1,bhvs)
            else
              let p = Aorai_utils.is_out_of_state_pred state in
              let post_cond = [Normal, Logic_const.new_predicate p] in
              let bhv =
                if Logic_utils.is_trivially_true assumes then
                  Cil.mk_behavior ~name ~post_cond ()
                else begin
                  let assumes = [Logic_const.new_predicate assumes] in
                  Cil.mk_behavior ~name ~assumes ~post_cond ()
                end
              in
              (i+1,bhv :: bhvs)
          in
          let (states,_) = Data_for_aorai.getAutomata () in
          List.rev (snd (List.fold_left aux (0,bhvs) states))
        end
    in
    (* If this is the main function, we should exit in at least one
       acceptance state.
     *)
    let bhvs =
      if Aorai_option.ConsiderAcceptance.get () &&
        Datatype.String.equal
        (Kernel_function.get_name kf) (Kernel.MainFunction.get())
      then
        let accept = Logic_const.new_predicate (get_acceptance_pred()) in
        let post_cond = [Normal, accept] in
        let name = "aorai_acceptance" in
        Cil.mk_behavior ~name ~post_cond () :: bhvs
      else bhvs
    in
    if Aorai_option.AddingOperationNameAndStatusInSpecification.get()
    then begin
      let called_post =
        Logic_const.new_predicate
          (Logic_const.prel
             (Req ,
              Logic_const.tvar
                (Data_for_aorai.get_logic_var Data_for_aorai.curOpStatus),
              Logic_const.term
                (TConst
                   (Logic_utils.constant_to_lconstant
		      (Data_for_aorai.op_status_to_cenum Promelaast.Return)))
                (Ctype Cil.intType)))
      in
      let called_post_2 =
        Logic_const.new_predicate
          (Logic_const.prel
             (Req,
              Logic_const.tvar
                (Data_for_aorai.get_logic_var Data_for_aorai.curOp),
              Logic_const.term
                (TConst
                   (Logic_utils.constant_to_lconstant
		      (Data_for_aorai.func_to_cenum
			 (Kernel_function.get_name kf))))
                (Ctype Cil.intType)))
      in
      let name = "Buchi_property_behavior_function_states" in
      let post_cond = [Normal, called_post; Normal, called_post_2] in
      Cil.mk_behavior ~name ~post_cond () :: bhvs
    end else bhvs
  in
object(self)

  inherit Visitor.frama_c_inplace

  (* We have to update assigns whenever a call occurs in the scope of
     a statement contract (function always update the automaton's state,
     so assigns there have to be changed anyway.) *)
  val has_call = Stack.create ()

  method private enter_block () = Stack.push (ref false) has_call

  method private call () = Stack.iter (fun x -> x := true) has_call

  method private leave_block () = !(Stack.pop has_call)

  method! vfunc f =
    let my_kf = Extlib.the self#current_kf in
    let vi = Kernel_function.get_vi my_kf in
    let spec = Annotations.funspec my_kf in
    let loc = Kernel_function.get_location my_kf in
    (match kind_of_func vi with
    | Pre_func _ | Post_func _ ->
      Aorai_option.fatal
        "functions managing automaton's state are \
             not supposed to have a body"
    | Not_auto_func -> (* Normal C function *)
      let bhvs = mk_post my_kf in
      let my_state = Data_for_aorai.get_kf_init_state my_kf in
      let requires = needs_zero_one_choice my_state in
      let requires =
        Aorai_utils.auto_func_preconditions 
          loc my_kf Promelaast.Call my_state
        @ requires
      in
      let post_cond = needs_post my_kf in
      match Cil.find_default_behavior spec with
        | Some b ->
	  Annotations.add_requires Aorai_option.emitter my_kf b.b_name requires;
	  Annotations.add_ensures Aorai_option.emitter my_kf b.b_name post_cond;
	  Annotations.add_behaviors Aorai_option.emitter my_kf bhvs
        | None ->
          let bhv = Cil.mk_behavior ~requires ~post_cond () in
	  Annotations.add_behaviors Aorai_option.emitter my_kf (bhv :: bhvs));
    let after f = update_assigns f.svar.vdecl my_kf Kglobal spec; f in
    ChangeDoChildrenPost(f,after)

  method! vglob_aux g =
    match g with
    | GVarDecl(_,v,_) when
        Cil.isFunctionType v.vtype
        && not (Kernel_function.is_definition (Extlib.the self#current_kf))
        ->
      let my_kf = Extlib.the self#current_kf in
      (* don't use get_spec, as we'd generate default assigns,
         while we'll fill the spec just below. *)
      let vi = Kernel_function.get_vi my_kf in
      (match kind_of_func vi with
      | Pre_func kf ->
        (* must advance the automaton according to current call. *)
        let bhvs = mk_pre_fct_spec kf in
        let bhvs =
          Visitor.visitFramacBehaviors (new change_formals kf my_kf) bhvs
        in
	Annotations.add_behaviors Aorai_option.emitter my_kf bhvs;
        SkipChildren
      | Post_func kf ->
          (* must advance the automaton according to return event. *)
        let (rt, _, _, _) =
          Cil.splitFunctionTypeVI (Kernel_function.get_vi kf)
        in
        let bhvs = mk_post_fct_spec kf in
        let bhvs =
          (* if return type is not void, convert \result in the formal
             arg of current kf. Otherwise, there's no conversion to do. *)
          if Cil.isVoidType rt then bhvs
          else Visitor.visitFramacBehaviors (new change_result my_kf) bhvs
        in
	Annotations.add_behaviors Aorai_option.emitter my_kf bhvs;
        SkipChildren
      | Not_auto_func -> DoChildren (* they are not considered here. *))
    | _ -> DoChildren

  method! vstmt_aux stmt =
    let kf = Extlib.the self#current_kf in
    let treat_loop body_ref stmt =
      let init_state = Data_for_aorai.get_loop_init_state stmt in
      let inv_state = Data_for_aorai.get_loop_invariant_state stmt in

      let possible_states =
        Data_for_aorai.merge_end_state
          (all_possible_states init_state) (all_possible_states inv_state)
      in
      let loop_assigns =
        Annotations.code_annot ~filter:Logic_utils.is_assigns stmt
      in
      (* varinfo of the init_var associated to this loop *)
      let vi_init =
        Data_for_aorai.get_varinfo
          (Data_for_aorai.loopInit ^ "_" ^ string_of_int stmt.sid)
      in

      (*    1) The associated init variable is set to 0 in first position
            (or in second position if the first stmt is a if)*)

      let loc = Cil_datatype.Stmt.loc stmt in
      let stmt_varset =
        Cil.mkStmtOneInstr
          (Set((Var vi_init,NoOffset), Cil.zero ~loc, loc))
      in
      stmt_varset.sid<-(Cil.Sid.next ());
      stmt_varset.ghost<-true;
      begin
        (* Function adapted from the cil printer *)
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
              | _, {skind=Break _}:: _
              | _, {skind=Goto _} :: _
              | {skind=Goto _} :: _, _
              | {skind=Break _} :: _, _ ->
                !body_ref.bstmts <-
		  head :: stmt_varset :: List.tl !body_ref.bstmts
              | _ ->
                raise Not_found
            end
          | _ -> raise Not_found
        with Not_found ->
          !body_ref.bstmts<-stmt_varset::!body_ref.bstmts
      end;

      (*    2) The associated init variable is set to 1 before the loop *)
      let new_loop = mkStmt ~valid_sid:true stmt.skind in
      let stmt_varset =
        Cil.mkStmtOneInstr ~valid_sid:true
          (Set((Var(vi_init),NoOffset), Cil.one ~loc, loc))
      in
      stmt_varset.ghost <- true;
      let block = mkBlock [stmt_varset;new_loop] in
      stmt.skind<-Block(block);

      (* Overcome WP limitation wrt LoopEntry. See bug 1353 *)
      new_loop.labels <- 
        [ Label ("aorai_loop_" ^ string_of_int stmt.sid,
                 Cil_datatype.Stmt.loc stmt, false)];
      let loop_entry_label = StmtLabel (ref new_loop) in

      (*    3) Generation of the loop invariant *)
      let mk_imply operator predicate =
        pimplies
          (prel(operator,
                Aorai_utils.mk_term_from_vi vi_init,
                Aorai_utils.zero_term()),
           predicate)
      in
      (* The loop invariant is :
	 (Global invariant)  // all never reached state are set to zero
	 & (Init => Pre1)      // external pre-condition
	 & (not Init => Post2) // internal post-condition
	 & counter_invariant   // values of counters.
	 (init: fresh variable which indicates if the iteration is the first
	 one). *)
      condition_to_invariant kf possible_states new_loop;

      let init_preds =
        impossible_states_preds Logic_const.pre_label possible_states init_state
      in
      let treat_init_pred pred =
        let pred = mk_imply Rneq pred in
        predicate_to_invariant kf new_loop pred
      in
      List.iter treat_init_pred init_preds;
      let invariant_preds =
        impossible_states_preds_inv loop_entry_label possible_states inv_state
      in
      let treat_inv_pred pred =
        let pred = mk_imply Req pred in
        predicate_to_invariant kf new_loop pred
      in
      List.iter treat_inv_pred invariant_preds;

      let action_inv_preds =
        Aorai_utils.all_actions_preds loop_entry_label inv_state
      in
      List.iter (predicate_to_invariant kf new_loop) action_inv_preds;
      
      List.iter
        (update_loop_assigns kf new_loop inv_state (Cil.cvar_to_lvar vi_init))
        loop_assigns;

      (*    4) Keeping in mind to preserve old annotations after visitor end *)
      Hashtbl.add post_treatment_loops (ref stmt) (ref new_loop);

      (*    5) Updated stmt is returned *)
      stmt
    in
    self#enter_block ();
    let after s =
      if self#leave_block () then
        let annots = Annotations.code_annot stmt in
        let _, specs = List.split (Logic_utils.extract_contract annots) in
        List.iter
          (update_assigns
             (Cil_datatype.Stmt.loc stmt) 
             (Extlib.the self#current_kf)
             (Kstmt stmt))
          specs;
        s
      else 
	s
    in
    if treatloops then
      match stmt.skind with
      | Loop (_,block,_,_,_) ->
        ChangeDoChildrenPost(stmt, after $ (treat_loop (ref block)))

      | _ -> ChangeDoChildrenPost(stmt, after)
    else
      ChangeDoChildrenPost(stmt,after)

  method! vinst = function
  | Call _ -> self#call (); DoChildren
  | _ -> DoChildren

end





(****************************************************************************)
(**
  This visitor computes the list of ignored functions.
  A function is ignored if its call is present in the C program,
  while its definition is not available.
*)
class visit_computing_ignored_functions () =
  let declaredFunctions = Data_for_aorai.getFunctions_from_c () in
  let isDeclaredInC fname =
    List.exists
      (fun s -> (String.compare fname s)=0)
      declaredFunctions
  in
object (*(self) *)

  inherit Visitor.frama_c_inplace

  method! vfunc _f = DoChildren

  method! vstmt_aux stmt =
    match stmt.skind with
      | Instr(Call (_,funcexp,_,_)) ->
          let name = get_call_name funcexp in
          (* If the called function is neither ignored, nor declared,
             then it has to be added to ignored functions. *)
          if (not (Data_for_aorai.isIgnoredFunction name))
            && (not (isDeclaredInC name)) then
                (Data_for_aorai.addIgnoredFunction name);
          DoChildren
      | _ -> DoChildren

end

let add_pre_post_from_buch file treatloops  =
  let visitor = new visit_adding_pre_post_from_buch treatloops in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file;
  (* Transfer previous annotation on the new loop statement.
     Variant clause has to be preserved at the end of the annotation.*)
  Hashtbl.iter
    (fun old_stmt new_stmt ->
      let new_s = !new_stmt in
      let old_s = !old_stmt in
      let kf = Kernel_function.find_englobing_kf old_s in
      (* Erasing annotations from the old statement before attaching them with
	 the new one *)
      let annots = 
	Annotations.fold_code_annot
	  (fun e a acc -> 
	    Annotations.remove_code_annot e ~kf old_s a;
            if (Logic_utils.is_assigns a) then acc else (e, a) :: acc)
	  old_s
	  [];
      in
      List.iter (fun (e, a) -> Annotations.add_code_annot e ~kf new_s a) annots)
   post_treatment_loops

let add_sync_with_buch file  =
  let visitor = new visit_adding_code_for_synchronisation in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file

(* Call of the visitor *)
let compute_ignored_functions file =
  let visitor = new visit_computing_ignored_functions () in
  Cil.visitCilFile (visitor :> Cil.cilVisitor) file

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
