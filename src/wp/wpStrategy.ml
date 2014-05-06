(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
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

let dkey = Wp_parameters.register_category "strategy" (* debugging key *)
let debug fmt = Wp_parameters.debug ~dkey fmt

open Cil_types
open LogicUsage

(* -------------------------------------------------------------------------- *)
(** An annotation can be used for different purpose. *)
type annot_kind =
  | Ahyp  (* annotation is an hypothesis,
             but not a goal (see Aboth) : A => ...*)
  | Agoal (* annotation is a goal,
             but not an hypothesis (see Aboth): A /\ ...*)
  | Aboth of bool 
      (* annotation can be used as both hypothesis and goal :
	 - with true : considerer as both : A /\ A=>..
	 - with false : we just want to use it as hyp right now. *)
  | AcutB of bool 
      (* annotation is use as a cut :
	 - with true (A is also a goal) -> A (+ proof obligation A => ...)
	 - with false (A is an hyp only) -> True (+ proof obligation A => ...) *)
  | AcallHyp of kernel_function
      (* annotation is a called function property to consider as an Hyp.
       * The pre are not here but in AcallPre since they can also
       * be considered as goals. *)
  | AcallPre of bool * kernel_function
      (* annotation is a called function precondition :
         to be considered as hyp, and goal if bool=true *)

(* -------------------------------------------------------------------------- *)
(* --- Annotations for one program point.                                 --- *)
(* -------------------------------------------------------------------------- *)

module ForCall = Kernel_function.Map

(** Some elements can be used as both Hyp and Goal : because of the selection
 * mecanism, we need to add a boolean [as_goal] to tell if the element is to be
 * considered as a goal. If [false], the element can still be used as hypthesis.
 *)
type annots = {
  p_hyp : WpPropId.pred_info list;
  p_goal : WpPropId.pred_info list;
  p_both : (bool * WpPropId.pred_info) list;
  p_cut : (bool * WpPropId.pred_info) list;
  call_hyp : WpPropId.pred_info list ForCall.t; (* post and pre *)
  call_pre : (bool * WpPropId.pred_info) list ForCall.t; (* goal only *)
  call_asgn : WpPropId.assigns_full_info ForCall.t;
  a_goal : WpPropId.assigns_full_info;
  a_hyp : WpPropId.assigns_full_info;
  a_call : WpPropId.assigns_full_info; (* dynamic calls *)
}

type t_annots = { has_asgn_goal : bool; has_prop_goal : bool; info: annots }

(* --- Add annotations --- *)

let empty_acc =
  let a = { 
    p_hyp = []; p_goal = []; p_both = []; p_cut = [];
    call_hyp = ForCall.empty; 
    call_pre = ForCall.empty; 
    call_asgn = ForCall.empty;
    a_goal = WpPropId.empty_assigns_info; 
    a_hyp = WpPropId.empty_assigns_info;
    a_call = WpPropId.empty_assigns_info;
  } in
  { has_asgn_goal = false; has_prop_goal = false; info = a; }

let add_prop acc kind labels id p =
  let get_p debug_txt =
    try
      let p = NormAtLabels.preproc_annot labels p in
      let _ =
        debug "take as %s (@[%a:@ %a@])@." debug_txt
	  WpPropId.pretty id Printer.pp_predicate_named p
      in Some (WpPropId.mk_pred_info id p)
    with e -> NormAtLabels.catch_label_error e 
      (WpPropId.get_propid id) "annotation"; None
  in
  let add_hyp l = match get_p "hyp" with None -> l | Some p -> p::l in
  let add_goal l =
    (* if goal_to_select config id
    then *) match get_p "goal" with None -> l
      | Some p -> ( (* has_prop_goal := true; *) p::l )
        (* else l *)
  in
  let add_both goal l =
    match get_p ("both goal=" ^ if goal then "true" else "false") with 
      | None -> l
      | Some p ->
            (* if goal then has_prop_goal := true;*)
            (goal, p)::l
  in
  let add_hyp_call fct calls =
    let l = try ForCall.find fct calls with Not_found -> [] in
    ForCall.add fct (add_hyp l) calls in
  let add_both_call fct goal calls =
    let l = try ForCall.find fct calls with Not_found -> [] in
    ForCall.add fct (add_both goal l) calls in
  let info = acc.info in
  let goal, info = match kind with
      | Ahyp -> 
          false, { info with p_hyp = add_hyp info.p_hyp }
      | Agoal -> 
          true, { info with p_goal = add_goal info.p_goal }
      | Aboth goal ->
          goal, { info with p_both = add_both goal info.p_both }
      | AcutB goal ->
          goal, { info with p_cut = add_both goal info.p_cut }
      | AcallHyp fct -> 
          false, { info with call_hyp = add_hyp_call fct info.call_hyp }
      | AcallPre (goal,fct) ->
          goal, { info with call_pre = add_both_call fct goal info.call_pre }
  in let acc = { acc with info = info } in
    if goal then { acc with has_prop_goal = true} else acc

(* -------------------------------------------------------------------------- *)
(* adding some specific properties. *)

let add_prop_fct_pre acc kind kf bhv ~assumes pre =
  let id = WpPropId.mk_pre_id kf Kglobal bhv pre in
  let labels = NormAtLabels.labels_fct_pre in
  let p = Logic_const.pred_of_id_pred pre in
  let p = match assumes with None -> p
    | Some assumes -> Logic_const.pimplies (assumes, p)
  in
  let p = Logic_const.pat (p, Logic_const.pre_label) in
    (* TODO: why this at ??? [2011-07-08-Anne] *)
    add_prop acc kind labels id p

let add_prop_fct_post acc kind kf  bhv tkind post =
  let id = WpPropId.mk_fct_post_id kf bhv (tkind, post) in
  let labels = NormAtLabels.labels_fct_post in
  let p = Logic_const.pred_of_id_pred post in
    add_prop acc kind labels id p

let add_prop_fct_bhv_pre acc kind kf bhv ~impl_assumes =
  let assumes = 
    if impl_assumes then Some (Ast_info.behavior_assumes bhv) else None
  in 
  let add acc p = add_prop_fct_pre acc kind kf bhv ~assumes p in
  let acc = List.fold_left add acc bhv.b_requires in
    if impl_assumes then acc
    else List.fold_left add acc bhv.b_assumes

let add_prop_stmt_pre acc kind kf s bhv ~assumes pre =
  let id = WpPropId.mk_pre_id kf (Kstmt s) bhv pre in
  let labels = NormAtLabels.labels_stmt_pre s in
  let p = Logic_const.pred_of_id_pred pre in
  let p = match assumes with None -> p
    | Some assumes -> Logic_const.pimplies (assumes, p)
  in add_prop acc kind labels id p

let add_prop_stmt_bhv_requires acc kind kf s bhv ~with_assumes =
  let assumes = 
    if with_assumes then Some (Ast_info.behavior_assumes bhv) else None
  in let add acc pre =
      add_prop_stmt_pre acc kind kf s bhv ~assumes pre
    in List.fold_left add acc bhv.b_requires

(** Process the stmt spec precondition as an hypothesis for external properties.
 * Add [assumes => requires] for all the behaviors. *)
let add_prop_stmt_spec_pre acc kind kf s spec =
  let add_bhv_pre acc bhv =
    add_prop_stmt_bhv_requires acc kind kf s bhv ~with_assumes:true
  in List.fold_left add_bhv_pre acc spec.spec_behavior

let add_prop_stmt_post acc kind kf s bhv tkind l_post ~assumes post =
  let id = WpPropId.mk_stmt_post_id kf s bhv (tkind, post) in
  let labels = NormAtLabels.labels_stmt_post s l_post in
  let p = Logic_const.pred_of_id_pred post in
  let p = match assumes with None -> p
    | Some assumes ->
        let assumes = Logic_const.pold assumes in
          (* can use old because label normalisation will be called *)
          Logic_const.pimplies (assumes, p)
  in add_prop acc kind labels id p

let add_prop_call_pre acc kind id ~assumes pre =
  (* TODO: we don't build the id here yet because of strange things in wpAnnot.
  *        Find out how to deal with it. [2011-07-13-Anne] *)
  let labels = NormAtLabels.labels_fct_pre in
  let p = Logic_const.pred_of_id_pred pre in
  let p = Logic_const.pimplies (assumes, p) in
    add_prop acc kind labels id p

let add_prop_call_post acc kind called_kf bhv tkind ~assumes post =
  let id = WpPropId.mk_fct_post_id called_kf bhv (tkind, post) in
  let labels = NormAtLabels.labels_fct_post in
  let p = Logic_const.pred_of_id_pred post in
  let assumes = Logic_const.pold assumes in
  let p = Logic_const.pimplies (assumes, p) in
    add_prop acc kind labels id p

let add_prop_assert acc kind kf s ca p =
  let id = WpPropId.mk_assert_id kf s ca in
  let labels = NormAtLabels.labels_assert_before s in
    add_prop acc kind labels id p

let add_prop_loop_inv acc kind s id p =
  let labels = NormAtLabels.labels_loop_inv s in
    add_prop acc kind labels id p

(** apply [f_normal] on the [Normal] postconditions,
* [f_exits] on the [Exits] postconditions, and warn on the others. *)
let fold_bhv_post_cond ~warn f_normal f_exits acc b =
  let add (p_acc, e_acc) ((termination_kind, pe) as e) =
    match termination_kind with
      | Normal -> f_normal p_acc pe, e_acc
      | Exits -> p_acc, f_exits e_acc pe
      | (Breaks|Continues|Returns) -> (* TODO *)
          begin
            if warn then
              Wp_parameters.warning
                "Abrupt statement termination property ignored:@, %a"
                Printer.pp_post_cond e;
            p_acc, e_acc
          end
  in List.fold_left add acc b.b_post_cond

(* -------------------------------------------------------------------------- *)

let add_assigns acc kind id a_desc =
  let take_assigns () =
    debug "take %a %a" WpPropId.pp_propid id WpPropId.pp_assigns_desc a_desc;
    WpPropId.mk_assigns_info id a_desc
  in
  let take_assigns_call fct info =
    let asgn = take_assigns () in
    { info with call_asgn = ForCall.add fct asgn info.call_asgn }
  in
  let info = acc.info in
  let goal, info = match kind with
    | Ahyp -> false, {info with a_hyp = take_assigns ()}
    | AcallHyp fct -> false, take_assigns_call fct info
    | Agoal -> true, {info with a_goal = take_assigns ()}
    | _ -> Wp_parameters.fatal "Assigns prop can only be Hyp or Goal"
  in let acc = { acc with info = info } in
    if goal then { acc with has_asgn_goal = true} else acc

let add_assigns_any acc kind asgn =
  let take_call fct asgn info =
    { info with call_asgn = ForCall.add fct asgn info.call_asgn } in
  match kind with
    | Ahyp ->  {acc with info = { acc.info with a_hyp = asgn}}
    | AcallHyp fct -> {acc with info = take_call fct asgn acc.info}
    | _ -> Wp_parameters.fatal "Assigns Any prop can only be Hyp"
	
let assigns_upper_bound spec =
  let bhvs = spec.spec_behavior in
  let upper a b =
    match a, b.b_assigns with
        | None, Writes a when Cil.is_default_behavior b ->
          Some (b,a) (* default behavior always applies. *)
        | None, _ -> None (* WritesAny U X -> WritesAny *)
        | Some (b,_), _ when Cil.is_default_behavior b ->
          a (* default behavior prevails over other behaviors. *)
        | Some _, WritesAny ->
          None (* No default behavior and one behavior assigns everything. *)
        | Some(b,a1), Writes a2 -> Some (b,a1 @ a2)
          (* take the whole list of assigns. *)
  in
  match bhvs with
    | [] -> None
    | bhv::bhvs ->
      (* [VP 2011-02-04] Note that if there is no default and each
         behavior has a proper assigns clause we put dependencies only
         to the assigns of a more or less randomly selected behavior,
         but the datatypes above can't handle anything better.  *)
      let acc =
        match bhv.b_assigns with
            WritesAny -> None
          | Writes a -> Some(bhv,a)
      in
      List.fold_left upper acc bhvs

(* [VP 2011-02-04] These two functions below mix all the assigns of
   a function regardless of the behavior. At least now that we take
   WritesAny as soon as at least one behavior has no assigns clause,
   this is correct, but still imprecise. Needs refactoring of t_annots to
   go further, though.
   [AP 2011-03-11] I think that the merge of all assigns properties
   is intended because we are using it as an hypothesis to skip the statement
   or the function call.
 *)
let add_stmt_spec_assigns_hyp acc kf s l_post spec =
  match assigns_upper_bound spec with
    | None ->
        add_assigns_any acc Ahyp
          (WpPropId.mk_stmt_any_assigns_info s)
    | Some(bhv, assigns) ->
        let id = WpPropId.mk_stmt_assigns_id kf s bhv assigns in
        match id with
          | None -> add_assigns_any acc Ahyp
              (WpPropId.mk_stmt_any_assigns_info s)
          | Some id ->
              let labels = NormAtLabels.labels_stmt_assigns s l_post in
              let assigns = NormAtLabels.preproc_assigns labels assigns in
              let a_desc = WpPropId.mk_stmt_assigns_desc s assigns in
              add_assigns acc Ahyp id a_desc
		
let add_call_assigns_any acc s =
  let asgn = WpPropId.mk_stmt_any_assigns_info s in
  {acc with info = { acc.info with a_call = asgn }}

let add_call_assigns_hyp acc kf_caller s ~called_kf l_post spec_opt =
  match spec_opt with
    | None ->
        let pid = WpPropId.mk_stmt_any_assigns_info s in
        add_assigns_any acc (AcallHyp called_kf) pid
    | Some spec ->
	match assigns_upper_bound spec with
          | None ->
              let asgn = WpPropId.mk_stmt_any_assigns_info s in
              add_assigns_any acc (AcallHyp called_kf) asgn
          | Some(bhv, assigns) ->
              let id = WpPropId.mk_stmt_assigns_id kf_caller s bhv assigns in
              match id with
                | None ->
                    let asgn = WpPropId.mk_stmt_any_assigns_info s in
                    add_assigns_any acc (AcallHyp called_kf) asgn
                | Some pid ->
                    let labels = NormAtLabels.labels_stmt_assigns s l_post in
                    let assigns = NormAtLabels.preproc_assigns labels assigns in
                    let a_desc = WpPropId.mk_stmt_assigns_desc s assigns in
                    add_assigns acc (AcallHyp called_kf) pid a_desc
		      
(* [VP 2011-01-28] following old behavior, not sure it is correct:
   why should we give to add_assigns the assigns with unnormalized labels?
   [AP 2011-03-11] to answer VP question, the source assigns are only used to
   build an identifier for the property which is use later to update its status
   and dependencies so we need to have the original one.
*)
let add_loop_assigns_hyp acc kf s asgn_opt = match asgn_opt with
  | None ->
      let asgn = WpPropId.mk_loop_any_assigns_info s in
        add_assigns_any acc Ahyp asgn
  | Some (ca, assigns) ->
      let id = WpPropId.mk_loop_assigns_id kf s ca assigns in
      match id with
          | None ->
            let asgn = WpPropId.mk_loop_any_assigns_info s in
            add_assigns_any acc Ahyp asgn
          | Some id ->
            let labels = NormAtLabels.labels_loop_assigns s in
            let assigns' = NormAtLabels.preproc_assigns labels assigns in
            let a_desc = WpPropId.mk_loop_assigns_desc s assigns' in
            add_assigns acc Ahyp id a_desc

let add_fct_bhv_assigns_hyp acc kf tkind b = match b.b_assigns with
  | WritesAny ->
      let id = WpPropId.mk_kf_any_assigns_info () in
      add_assigns_any acc Ahyp id
  | Writes assigns ->
      let id = WpPropId.mk_fct_assigns_id kf b tkind assigns in
      match id with
          | None ->
            let id = WpPropId.mk_kf_any_assigns_info () in
            add_assigns_any acc Ahyp id
          | Some id ->     
            let labels = NormAtLabels.labels_fct_assigns in
            let assigns' = NormAtLabels.preproc_assigns labels assigns in
            let a_desc = WpPropId.mk_kf_assigns_desc assigns' in
            add_assigns acc Ahyp id a_desc

(* --- Get annotations --- *)

let get_goal_only annots = annots.info.p_goal

let get_hyp_only annots = annots.info.p_hyp

let filter_both l =
  let add (h_acc, g_acc) (goal, p) =
    p::h_acc, if goal then p::g_acc else g_acc
  in List.fold_left add ([], []) l

let get_both_hyp_goals annots = filter_both annots.info.p_both

let get_call_hyp annots fct = 
  try ForCall.find fct annots.info.call_hyp
  with Not_found -> []

let get_call_pre annots fct = 
  try filter_both (ForCall.find fct annots.info.call_pre)
  with Not_found -> [],[]

let get_call_asgn annots = function
  | None -> annots.info.a_call
  | Some fct -> 
      try ForCall.find fct annots.info.call_asgn
      with Not_found -> WpPropId.empty_assigns_info

let get_cut annots = annots.info.p_cut

let get_asgn_hyp annots = annots.info.a_hyp

let get_asgn_goal annots = annots.info.a_goal

(* --- Print annotations --- *)

let pp_annots fmt acc =
  let acc = acc.info in
  let pp_pred k b p =
    Format.fprintf fmt "%s%s: %a@."
      k (if b then "" else " (h)") WpPropId.pp_pred_of_pred_info p
  in
  let pp_pred_list k l = List.iter (fun p -> pp_pred k true p) l in
  let pp_pred_b_list k l = List.iter (fun (b, p) -> pp_pred k b p) l in
  begin
     pp_pred_list "H" acc.p_hyp;
     pp_pred_list "G" acc.p_goal;
     pp_pred_b_list "H+G" acc.p_both;
     pp_pred_b_list "C" acc.p_cut;
     ForCall.iter
       (fun kf hs -> 
	  let name = "CallHyp:" ^ (Kernel_function.get_name kf) in
	  pp_pred_list name hs)
       acc.call_hyp;
     ForCall.iter
       (fun kf bhs ->
	  let name = "CallPre:" ^ (Kernel_function.get_name kf) in
	  pp_pred_b_list name bhs)
       acc.call_pre;
     ForCall.iter
       (fun kf asgn ->
	  let name = "CallAsgn:" ^ (Kernel_function.get_name kf) in
	  WpPropId.pp_assign_info name fmt asgn)
       acc.call_asgn;
     WpPropId.pp_assign_info "DC" fmt acc.a_call;
     WpPropId.pp_assign_info "HA" fmt acc.a_hyp;
     WpPropId.pp_assign_info "GA" fmt acc.a_goal;
  end

let merge_calls f call1 call2 =
  ForCall.merge
    (fun _fct a b -> match a,b with
       | None,c | c,None -> c
       | Some a,Some b -> Some (f a b)
    ) call1 call2

(* TODO: it should be possible to do without this, but needs a big refactoring*)
let merge_acc acc1 acc2 =
{
  p_hyp = acc1.p_hyp @ acc2.p_hyp;
  p_goal = acc1.p_goal @ acc2.p_goal;
  p_both = acc1.p_both @ acc2.p_both;
  p_cut = acc1.p_cut @ acc2.p_cut;
  call_hyp = merge_calls (@) acc1.call_hyp acc2.call_hyp;
  call_pre = merge_calls (@) acc1.call_pre acc2.call_pre;
  call_asgn = merge_calls WpPropId.merge_assign_info acc1.call_asgn acc2.call_asgn;
  a_goal = WpPropId.merge_assign_info acc1.a_goal acc2.a_goal;
  a_hyp = WpPropId.merge_assign_info acc1.a_hyp acc2.a_hyp;
  a_call = WpPropId.merge_assign_info acc1.a_call acc2.a_call;
}

(* -------------------------------------------------------------------------- *)
(* --- Annotation table                                                   --- *)
(* -------------------------------------------------------------------------- *)
   
(** This is an Hashtbl where some predicates are stored on CFG edges.
 * On each edge, we store hypotheses and goals.
 *)
module Hannots = Cil2cfg.HE (struct type t = annots end)

type annots_tbl = {
  tbl_annots : Hannots.t;
  mutable tbl_axioms :  WpPropId.axiom_info list;
  mutable tbl_has_prop_goal : bool;
  mutable tbl_has_asgn_goal : bool;
}

let create_tbl () = {
  tbl_annots = Hannots.create 7;
  tbl_axioms = [];
  tbl_has_prop_goal = false;
  tbl_has_asgn_goal = false;
}

let add_on_edges tbl new_acc edges =
  if new_acc.has_prop_goal then tbl.tbl_has_prop_goal <- true;
  if new_acc.has_asgn_goal then tbl.tbl_has_asgn_goal <- true;
  let add_on_edge e =
    let acc =
      try 
        let acc = Hannots.find tbl.tbl_annots e in 
          merge_acc new_acc.info acc
      with Not_found -> new_acc.info
    in Hannots.replace tbl.tbl_annots e acc;
  in List.iter add_on_edge edges

let add_node_annots tbl cfg v (before, (post, exits)) =
  debug "[add_node_annots] on %a@." Cil2cfg.pp_node v;
  add_on_edges tbl before (Cil2cfg.get_pre_edges cfg v);
  if post <> empty_acc then
    begin
      let edges_after = Cil2cfg.get_post_edges cfg v in
        if edges_after = []
        then Wp_parameters.warning ~once:true
               "Ignoring annotation rooted after statement with no succ"
        else add_on_edges tbl post edges_after
    end;
  if exits <> empty_acc then
    begin
      let edges_exits = Cil2cfg.get_exit_edges cfg v in
        if edges_exits = []
        then (* unreachable (see [process_unreached_annots]) *) ()
        else add_on_edges tbl exits edges_exits
    end

let add_loop_annots tbl cfg vloop ~entry ~back ~core =
  debug "[add_loop_annots] on %a@."Cil2cfg.pp_node vloop;
  let edges_to_head = Cil2cfg.succ_e cfg vloop in
    debug "[add_loop_annots] %d edges_to_head" (List.length edges_to_head);
  let edges_to_loop = Cil2cfg.pred_e cfg vloop in
    debug "[add_loop_annots] %d edges_to_loop" (List.length edges_to_loop);
  let back_edges, entry_edges =
    List.partition Cil2cfg.is_back_edge edges_to_loop
  in
    debug "[add_loop_annots] %d back_edges + %d entry_edges" 
      (List.length back_edges) (List.length entry_edges);
      add_on_edges tbl entry entry_edges;
  debug "[add_loop_annots on entry_edges ok]@.";
      add_on_edges tbl back back_edges;
  debug "[add_loop_annots on back_edges ok]@.";
      add_on_edges tbl core edges_to_head;
  debug "[add_loop_annots on edges_to_head ok]@."

let add_axiom tbl lemma =
  try
    (* Labels does not need normalization *)
    let axiom = WpPropId.mk_axiom_info lemma in
    debug "take %a@." WpPropId.pp_axiom_info axiom;
    tbl.tbl_axioms <- axiom::tbl.tbl_axioms 
  with e -> 
    NormAtLabels.catch_label_error e ("axiom "^lemma.lem_name) "axiom"

let add_all_axioms tbl =
  let rec do_g g =
      match g with
        | Daxiomatic (_ax_name, globs,_) -> do_globs globs
        | Dlemma (name,_,_,_,_,_) ->
	    let lem = LogicUsage.logic_lemma name in
	    add_axiom tbl lem
      | _ -> ()
  and do_globs globs = List.iter do_g globs in
  Annotations.iter_global (fun _ -> do_g)
  
let get_annots tbl e =
  try (* TODO clean : this is not very nice ! *)
    let info = Hannots.find tbl.tbl_annots e in { empty_acc with info = info}
  with Not_found -> empty_acc

(* -------------------------------------------------------------------------- *)
(* --- Strategy                                                           --- *)
(* -------------------------------------------------------------------------- *)

type strategy_for_froms = {
  get_pre : unit -> t_annots;
  more_vars : logic_var list
}

type strategy_kind =
  | SKannots (* normal mode for annotations *)
  | SKfroms of strategy_for_froms

(* an object of this type is the only access to annotations
 * from the rest of the application.
 * The idea is to be able to tune which properties to use for a computation. *)
type strategy = {
  desc : string ;
  cfg : Cil2cfg.t;
  behavior_name : string option ;

  new_loops : bool;

  strategy_kind : strategy_kind;
  annots : annots_tbl;
}


let get_kf s = Cil2cfg.cfg_kf s.cfg
let get_bhv s = s.behavior_name

let is_default_behavior s = 
  match s.behavior_name with None -> true | Some _ -> false

let mk_strategy desc cfg bhv_name new_loops kind tbl = {
  desc = desc; cfg = cfg; behavior_name = bhv_name; new_loops = new_loops; 
  strategy_kind = kind; annots = tbl;
}

let cfg_of_strategy strat = strat.cfg
let behavior_name_of_strategy strat = strat.behavior_name
let global_axioms strat = strat.annots.tbl_axioms
let strategy_kind strat = strat.strategy_kind
let strategy_has_prop_goal strat = strat.annots.tbl_has_prop_goal
let strategy_has_asgn_goal strat = strat.annots.tbl_has_asgn_goal
let get_annots strat = get_annots strat.annots
let new_loop_computation strat = strat.new_loops

let pp_info_of_strategy fmt strat = 
  Format.fprintf fmt "@[%s@]" strat.desc

(* -------------------------------------------------------------------------- *)
(* --- Helpers                                                            --- *)
(* -------------------------------------------------------------------------- *)

let is_main_init kf =
  if Kernel.LibEntry.get () then false
  else
    let is_main =
      try
        let main, _ = Globals.entry_point () in
        Kernel_function.equal kf main
      with Globals.No_such_entry_point _ -> false
    in
    debug "'%a' is %sthe main entry point@."
      Kernel_function.pretty kf (if is_main then "" else "NOT ");
    is_main

let mk_variant_properties kf s ca v =
  let vpos_id = WpPropId.mk_var_pos_id kf s ca in
  let vdecr_id = WpPropId.mk_var_decr_id kf s ca in
  let loc = v.term_loc in
  let lhead = Clabels.loop_head_label s in
  let vhead = Logic_const.tat ~loc (v, lhead) in
  let zero = Cil.lzero ~loc () in
  let vpos = Logic_const.prel ~loc (Rle, zero, vhead) in
  let vdecr = Logic_const.prel ~loc (Rlt, v, vhead) in
  (vpos_id, vpos), (vdecr_id, vdecr)

(* -------------------------------------------------------------------------- *)
