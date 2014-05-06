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

(** Wp computation using the CFG *)

open Cil_types
open Cil_datatype

module Cfg (W : Mcfg.S) = struct

  let dkey = Wp_parameters.register_category "calculus" (* Debugging key *)
  let debug fmt = Wp_parameters.debug ~dkey fmt

  (** Before storing something at a program point, we have to process the label
   * at that point. *)
  let do_labels wenv e obj =
    let do_lab o l =
      debug "[do_label] process %a@." Clabels.pretty l;
      W.label wenv l o
    in
    let obj = do_lab obj Clabels.Here in
    let labels = Cil2cfg.get_edge_labels e in
    let obj = List.fold_left do_lab obj labels in
    obj

  let add_hyp wenv obj h =
    debug "add hyp %a@." WpPropId.pp_pred_info h;
    W.add_hyp wenv h obj

  let open_scope wenv formals blocks =
    List.fold_right
      (fun b obj -> W.scope wenv b.blocals Mcfg.SC_Block_out obj)
      blocks 
      (W.scope wenv formals Mcfg.SC_Function_out W.empty)

  let add_goal wenv obj g =
    debug "add goal %a@." WpPropId.pp_pred_info g;
    W.add_goal wenv g obj
  (*[LC] Adding scopes for loop invariant preservation: WHY ???? *)
  (*[LC] Nevertheless, if required, this form should be used (BTS #1462) 
    match WpPropId.is_loop_preservation (fst g) with
      | None -> W.add_goal wenv g obj
      | Some stmt ->
    	debug "add scope for loop preservation %a@." WpPropId.pp_pred_info g ;
    	let blocks = Kernel_function.find_all_enclosing_blocks stmt in
    	let kf = Kernel_function.find_englobing_kf stmt in
    	let formals = Kernel_function.get_formals kf in
    	W.merge wenv (W.add_goal wenv g (open_scope wenv formals blocks)) obj
  *)

  let add_assigns_goal wenv obj g_assigns = match g_assigns with
    | WpPropId.AssignsAny _ | WpPropId.NoAssignsInfo -> obj
    | WpPropId.AssignsLocations a ->
        debug "add assign goal (@[%a@])@."
          WpPropId.pretty (WpPropId.assigns_info_id a);
        W.add_assigns wenv  a obj

  let add_assigns_hyp wenv obj h_assigns = match h_assigns with
    | WpPropId.AssignsLocations (h_id, a) ->
        let hid = Some h_id in
        let obj = W.use_assigns wenv a.WpPropId.a_stmt hid a obj in
        Some (Clabels.c_label a.WpPropId.a_label), obj
    | WpPropId.AssignsAny a ->
        Wp_parameters.warning ~current:true ~once:true 
          "Missing assigns clause (assigns 'everything' instead)" ;
        let obj = W.use_assigns wenv a.WpPropId.a_stmt None a obj in
        Some (Clabels.c_label a.WpPropId.a_label), obj
    | WpPropId.NoAssignsInfo -> None, obj

  (** detect if the computation of the result at [edge] is possible,
   * or if it will loop. If [strategy] are provide, 
   * cut are done on edges with cut properties, 
   * and if not, cut are done on loop node back edge if any.
   * TODO: maybe this should be done while building the strategy ?
   * *)
  exception Stop of Cil2cfg.edge
  let test_edge_loop_ok cfg strategy edge =
    debug "[test_edge_loop_ok] (%s strategy) for %a"
      (match strategy with None -> "without" | Some _ -> "with")
      Cil2cfg.pp_edge edge;
    let rec collect_edge_preds set e =
      let cut =
        match strategy with None ->  Cil2cfg.is_back_edge e
                          | Some strategy -> 
                              let e_annots = WpStrategy.get_annots strategy e in
                              (WpStrategy.get_cut e_annots <> [])
      in
      if cut then () (* normal loop cut *)
      else if Cil2cfg.Eset.mem e set 
      then (* e is already in set : loop without cut ! *)
        raise (Stop e)
      else (* add e to set and continue with its preds *)
        let set = Cil2cfg.Eset.add e set in
        let preds = Cil2cfg.pred_e cfg (Cil2cfg.edge_src e) in
        List.iter (collect_edge_preds set) preds
    in
    try
      let _ = collect_edge_preds Cil2cfg.Eset.empty edge in 
      debug "[test_edge_loop_ok] ok.";
      true
    with Stop e ->
      begin
        debug "[test_edge_loop_ok] loop without cut detected at %a"
          Cil2cfg.pp_edge e;
        false
      end

  (** to store the results of computations :
   * we store a result for each edge, and also a list of proof obligations.
   *
   * Be careful that there are two modes of computation :
   * the first one ([Pass1]) is used to prove the establishment of properties
   * while the second (after [change_mode_if_needed]) prove the preservation.
   * See {!R.set} for more details.
   * *)
  module R : sig
    type t
    val empty : Cil2cfg.t -> t
    val is_pass1 : t -> bool
    val change_mode_if_needed : t -> unit
    val find : t -> Cil2cfg.edge -> W.t_prop
    val set :  WpStrategy.strategy -> W.t_env ->
      t -> Cil2cfg.edge -> W.t_prop -> W.t_prop
    val add_oblig : t -> Clabels.c_label -> W.t_prop -> unit
    val add_memo : t -> Cil2cfg.edge -> W.t_prop -> unit
  end =
  struct
    type t_mode = Pass1 | Pass2

    module HE = Cil2cfg.HE (struct type t = W.t_prop option end)

    module LabObligs : sig
      type t
      val empty : t
      val is_empty : t -> bool
      val get_of_label : t -> Clabels.c_label -> W.t_prop list
      val get_of_edge : t -> Cil2cfg.edge -> W.t_prop list
      val add_to_label : t -> Clabels.c_label -> W.t_prop -> t
      val add_to_edge : t -> Cil2cfg.edge -> W.t_prop -> t
    end = struct

      type key = Olab of Clabels.c_label | Oedge of Cil2cfg.edge

      let cmp_key k1 k2 = match k1, k2 with
        | Olab l1, Olab l2 when l1 = l2 -> true
        | Oedge e1, Oedge e2 when Cil2cfg.same_edge e1 e2 -> true
        | _ -> false

      (* TODOopt: could have a sorted list... *)
      type t = (key * W.t_prop list) list

      let empty = []

      let is_empty obligs = (obligs = [])

      let add obligs k obj =
        let rec aux l_obligs = match l_obligs with
          | [] -> (k, [obj])::[]
          | (k', obligs)::tl when cmp_key k k' ->
              (k, obj::obligs)::tl
          | o::tl -> o::(aux tl)
        in aux obligs

      let add_to_label obligs label obj = add obligs (Olab label) obj

      let add_to_edge obligs e obj = add obligs (Oedge e) obj

      let get obligs k =
        let rec aux l_obligs = match l_obligs with
          | [] -> []
          | (k', obligs)::_ when cmp_key k k' -> obligs
          | _::tl -> aux tl
        in aux obligs

      let get_of_label obligs label = get obligs (Olab label)

      let get_of_edge obligs e = get obligs (Oedge e)

    end

    type t = {
      mutable mode : t_mode ;
      cfg: Cil2cfg.t;
      tbl : HE.t ;
      mutable memo : LabObligs.t;
      mutable obligs : LabObligs.t;
    }

    let empty cfg =
      debug "start computing (pass 1)@.";
      { mode = Pass1; cfg = cfg; tbl = HE.create 97 ;
        obligs = LabObligs.empty ; memo = LabObligs.empty ;}

    let is_pass1 res = (res.mode = Pass1)

    let add_oblig res label obj =
      debug "add proof obligation at label %a =@.  @[<hov2>  %a@]@."
        Clabels.pretty label W.pretty obj;
      res.obligs <- LabObligs.add_to_label (res.obligs) label obj

    let add_memo res e obj =
      debug "Memo goal for Pass2 at %a=@.  @[<hov2>  %a@]@."
        Cil2cfg.pp_edge e W.pretty obj;
      res.memo <- LabObligs.add_to_edge (res.memo) e obj

    let find res e =
      let obj = HE.find res.tbl e in
      match obj with None ->
        Wp_parameters.warning "find edge annot twice (%a) ?"
          Cil2cfg.pp_edge e;
        raise Not_found
                   | Some obj ->
                       if (res.mode = Pass2)
                       && (List.length
                             (Cil2cfg.pred_e res.cfg (Cil2cfg.edge_src e)) < 2) then
                         begin
                           (* it should be used once only : can free it *)
                           HE.replace res.tbl e None;
                           debug "clear edge %a@." Cil2cfg.pp_edge e
                         end;
                       obj

    (** If needed, clear wp table to compute Pass2.
     * If nothing has been stored in res.memo, there is nothing to do. *)
    let change_mode_if_needed res =
      if LabObligs.is_empty res.memo then ()
      else
        begin
          debug "change to Pass2 (clear wp table)@.";
          begin try
              let e_start = Cil2cfg.start_edge res.cfg in
              let start_goal = find res e_start in
              add_memo res e_start start_goal
            with Not_found -> ()
          end;
          HE.clear res.tbl;
          (* move memo obligs of Pass1 to obligs for Pass2 *)
          res.obligs <- res.memo;
          res.memo <- LabObligs.empty;
          res.mode <- Pass2
        end

    let collect_oblig wenv res e obj =
      let labels = Cil2cfg.get_edge_labels e in
      let add obj obligs =
        List.fold_left (fun obj o -> W.merge wenv o obj) obj obligs
      in
      let obj =
        try
          debug "get proof obligation at edge %a@." Cil2cfg.pp_edge e;
          let obligs = LabObligs.get_of_edge res.obligs e in
          add obj obligs
        with Not_found -> obj
      in
      let add_lab_oblig obj label =
        try
          debug "get proof obligation at label %a@." Clabels.pretty label;
          let obligs = LabObligs.get_of_label res.obligs label in
          add obj obligs
        with Not_found -> obj
      in
      let obj = List.fold_left add_lab_oblig obj labels in
      obj


    (** We have found some assigns hypothesis in the stategy :
     * it means that we skip the corresponding bloc, ie. we directly compute
     * the result before the block : (forall assigns. P),
     * and continue with empty. *)
    let use_assigns wenv res obj h_assigns =
      let lab, obj = add_assigns_hyp wenv obj h_assigns in
      match lab with 
      | None -> obj
      | Some label -> add_oblig res label obj; W.empty

    (** store the result p for the computation of the edge e.
     *
     * - In Compute mode :
        if we have some hyps H about this edge, store H => p
        if we have some goal G about this edge, store G /\ p
        if we have annotation B to be used as both H and G, store B /\ B=>P
        We also have to add H and G from HI (invariants computed in Pass1 mode)
        So finaly, we build : [ H => [ BG /\ (BH => (G /\ P)) ] ]
    *)
    let set strategy wenv res e obj =
      try
        match (HE.find res.tbl e) with
        | None -> raise Not_found
        | Some obj -> obj
      (* cannot warn here because it can happen with CUT properties.
       * We could check that obj is the same thing than the founded result *)
      (* Wp_parameters.fatal "strange loop at %a ?" Cil2cfg.pp_edge e *)
      with Not_found ->
        begin
          let e_annot = WpStrategy.get_annots strategy e in
          let h_prop = WpStrategy.get_hyp_only e_annot in
          let g_prop = WpStrategy.get_goal_only e_annot in
          let bh_prop, bg_prop = WpStrategy.get_both_hyp_goals e_annot in
          let h_assigns = WpStrategy.get_asgn_hyp e_annot in
          let g_assigns = WpStrategy.get_asgn_goal e_annot in
          (* get_cut is ignored : see get_wp_edge *)
          let obj = collect_oblig wenv res e obj in
          let is_loop_head =
            match Cil2cfg.node_type (Cil2cfg.edge_src e) with
            | Cil2cfg.Vloop (Some _, _) -> true
            | _ -> false
          in
          let compute ~goal obj =
            let local_add_goal obj g =
              if goal then add_goal wenv obj g else obj
            in
            let obj = List.fold_left (local_add_goal) obj g_prop in
            let obj = List.fold_left (add_hyp wenv) obj bh_prop in
            let obj =
              if goal then add_assigns_goal wenv obj g_assigns else obj
            in
            let obj = List.fold_left (local_add_goal) obj bg_prop in
            let obj = List.fold_left (add_hyp wenv) obj h_prop in
            obj
          in
          let obj = match res.mode with
            | Pass1 -> compute ~goal:true obj
            | Pass2 -> compute ~goal:false obj
          in
          let obj = do_labels wenv e obj in
          let obj =
            if is_loop_head then obj (* assigns used in [wp_loop] *)
            else use_assigns wenv res obj h_assigns
          in
          debug "[set_wp_edge] %a@." Cil2cfg.pp_edge e;
          debug " = @[<hov2>  %a@]@." W.pretty obj;
          Format.print_flush ();
          HE.replace res.tbl e (Some obj);
          find res e (* this should give back obj, but also do more things *)
        end

  end (* module R *)


  let use_loop_assigns strategy wenv e obj =
    let e_annot = WpStrategy.get_annots strategy e in
    let h_assigns = WpStrategy.get_asgn_hyp e_annot in
    let label, obj = add_assigns_hyp wenv obj h_assigns in
    match label with Some _ -> obj
                   | None -> assert false (* we should have assigns hyp for loops !*)

  let loop_with_cut cfg annots vloop =
    let to_loop_edges = Cil2cfg.pred_e cfg vloop in
    (*
     let back_edges =
       List.filter (Cil2cfg.is_back_edge) (Cil2cfg.pred_e cfg vloop)
     in *)
    List.for_all (test_edge_loop_ok cfg (Some annots)) to_loop_edges

  (** Compute the result for edge [e] which goes to the loop node [nloop].
   * So [e] can be either a back_edge or a loop entry edge.
   * Be very careful not to make an infinite loop by calling [get_loop_head]...
   * *)
  let wp_loop ((_, cfg, strategy, _, wenv)) res nloop e get_loop_head =
    let loop_with_cut_pass1 () =
      (* simply propagate both for [entry_edge] and [back_edge] *)
      debug "[wp_loop] propagate";
      let obj = get_loop_head nloop (* loop should be broken by a cut *) in
      let obj =
        if Cil2cfg.is_back_edge e then obj
        else W.loop_entry obj
      in obj
    in
    let loop_with_quantif () =
      if Cil2cfg.is_back_edge e then
        (* Be careful not to use get_only_succ here (infinite loop) *)
        (debug "[wp_loop] cut at back edge";
         W.empty)
      else (* edge going into the loop from outside : quantify *)
        begin
          debug "[wp_loop] quantify";
          let obj = get_loop_head nloop in
          let head = match Cil2cfg.succ_e cfg nloop with
            | [h] -> h
            | _ -> assert false (* already detected in [get_loop_head] *)
          in use_loop_assigns strategy wenv head obj
        end
    in
    if WpStrategy.new_loop_computation strategy
    && R.is_pass1 res
    && loop_with_cut cfg strategy nloop
    then
      loop_with_cut_pass1 ()
    else (* old mode or no inv or pass2 *)
      match Cil2cfg.node_type nloop with
      | Cil2cfg.Vloop (Some true, _) -> (* natural loop (has back edges) *)
          loop_with_quantif ()
      | _ -> (* TODO : print info about the loop *)
          Wp_error.unsupported
            "non-natural loop without invariant property."

  type callenv = {
    pre_annots : WpStrategy.t_annots ;
    post_annots : WpStrategy.t_annots ;
    exit_annots : WpStrategy.t_annots ;
  }

  let callenv cfg strategy v =
    let eb = match Cil2cfg.pred_e cfg v with e::_ -> e | _ -> assert false in
    let en, ee = Cil2cfg.get_call_out_edges cfg v in
    {
      pre_annots = WpStrategy.get_annots strategy eb ;
      post_annots = WpStrategy.get_annots strategy en ;
      exit_annots = WpStrategy.get_annots strategy ee ;
    }

  let wp_call_any wenv cenv ~p_post ~p_exit =
    let obj = W.merge wenv p_post p_exit in
    let call_asgn = WpStrategy.get_call_asgn cenv.post_annots None in
    let lab, obj = add_assigns_hyp wenv obj call_asgn in
    match lab with 
    | Some _ -> obj
    | None -> assert false

  let wp_call_kf wenv cenv stmt lval kf args precond ~p_post ~p_exit =
    let call_asgn = WpStrategy.get_call_asgn cenv.post_annots (Some kf) in
    let assigns = match call_asgn with
      | WpPropId.AssignsLocations (_, asgn_body) -> 
          asgn_body.WpPropId.a_assigns
      | WpPropId.AssignsAny _ -> WritesAny
      | WpPropId.NoAssignsInfo -> assert false (* see above *)
    in
    let pre_hyp, pre_goals = WpStrategy.get_call_pre cenv.pre_annots kf in
    let obj = W.call wenv stmt lval kf args
        ~pre:(pre_hyp)
        ~post:((WpStrategy.get_call_hyp cenv.post_annots kf))
        ~pexit:((WpStrategy.get_call_hyp cenv.exit_annots kf))
        ~assigns ~p_post ~p_exit in
    if precond 
    then W.call_goal_precond wenv stmt kf args ~pre:(pre_goals) obj
    else obj

  let wp_calls ((caller_kf, cfg, strategy, _, wenv)) res v stmt 
      lval call args p_post p_exit 
    =
    debug "[wp_calls] %a@." Cil2cfg.pp_call_type call;
    let cenv = callenv cfg strategy v in
    match call with
    | Cil2cfg.Static kf -> 
        let precond = 
          WpStrategy.is_default_behavior strategy && R.is_pass1 res 
        in
        wp_call_kf wenv cenv stmt lval kf args precond ~p_post ~p_exit
    | Cil2cfg.Dynamic fct ->
        let bhv = WpStrategy.behavior_name_of_strategy strategy in
        let calls = Dyncall.get ?bhv stmt in
        if calls = [] then
          wp_call_any wenv cenv ~p_post ~p_exit
        else
          let precond = R.is_pass1 res in
          let call kf = 
            let wp = wp_call_kf wenv cenv stmt lval 
                kf args precond ~p_post ~p_exit in
            kf , wp in
          let prop = Dyncall.property ~kf:caller_kf ?bhv ~stmt ~calls in
          let pid = WpPropId.mk_property prop in
          W.call_dynamic wenv stmt pid fct (List.map call calls)

  let wp_stmt wenv s obj = match s.skind with
    | Return (r, _) -> W.return wenv s r obj
    | Instr i ->
        begin match i with
          | (Set (lv, e, _)) -> W.assign wenv s lv e obj
          | (Call _) -> assert false
          | (Asm _) ->
              Wp_parameters.warning
                "Unsupported inline assembler. Assuming no effects.@.";
              obj
          | Skip _ | Code_annot _ -> obj
        end
    | Break _ | Continue _ | Goto _ -> obj
    | Loop _-> (* this is not a real loop (exit before looping)
                  just ignore it ! *) obj
    | If _ -> assert false
    | Switch _-> assert false
    | Block _-> assert false
    | UnspecifiedSequence _-> assert false
    | TryExcept _ | TryFinally _ -> assert false

  let wp_scope wenv vars scope obj =
    debug "[wp_scope] %s : %a@."
      (match scope with
       | Mcfg.SC_Global -> "global"
       | Mcfg.SC_Block_in -> "block in"
       | Mcfg.SC_Block_out -> "block out"
       | Mcfg.SC_Function_in -> "function in"
       | Mcfg.SC_Function_frame -> "function frame"
       | Mcfg.SC_Function_out -> "function out" )
      (Pretty_utils.pp_list  ~sep:", " Printer.pp_varinfo) vars;
    W.scope wenv vars scope obj


  (** @return the WP stored for edge [e]. Compute it if it is not already
   * there and store it. Also handle the Acut annotations. *)
  let rec get_wp_edge ((_kf, cfg, strategy, res, wenv) as env) e =
    !Db.progress ();
    let v = Cil2cfg.edge_dst e in
    debug "[get_wp_edge] get wp before %a@." Cil2cfg.pp_node v;
    try
      let res = R.find res e in
      debug "[get_wp_edge] %a already computed@." Cil2cfg.pp_node v;
      res
    with Not_found ->
      (* Notice that other hyp and goal are handled in R.set as usual *)
      let cutp =
        if R.is_pass1 res 
        then WpStrategy.get_cut (WpStrategy.get_annots strategy e)
        else []
      in
      match cutp with
      | [] ->
          let wp = compute_wp_edge env e in
          R.set strategy wenv res e wp
      | cutp ->
          debug "[get_wp_edge] cut at node %a@." Cil2cfg.pp_node v;
          let add_cut_goal (g,p) acc =
            if g then add_goal wenv acc p else acc
          in
          let edge_annot = List.fold_right add_cut_goal cutp W.empty in
          (* put cut goal properties as goals in e if any, else true *)
          let edge_annot = R.set strategy wenv res e edge_annot in
          let wp = compute_wp_edge env e in
          let add_cut_hyp (_,p) acc = add_hyp wenv acc p in
          let oblig = List.fold_right add_cut_hyp cutp wp in
          (* TODO : we could add hyp to the oblig if we have some in strategy *)
          let oblig = W.loop_step oblig in
          if test_edge_loop_ok cfg None e
          then R.add_memo res e oblig
          else R.add_oblig res Clabels.Pre (W.close wenv oblig);
          edge_annot

  and get_only_succ env cfg v = match Cil2cfg.succ_e cfg v with
    | [e'] -> get_wp_edge env e'
    | ls -> Wp_parameters.fatal "CFG node %a has %d successors instead of 1@."
              Cil2cfg.pp_node v (List.length ls)

  and compute_wp_edge ((kf, cfg, _annots, res, wenv) as env) e =
    let v = Cil2cfg.edge_dst e in
    debug "[compute_edge] before %a go...@." Cil2cfg.pp_node v;
    let old_loc = Cil.CurrentLoc.get () in
    let () = match Cil2cfg.node_stmt_opt v with
      | Some s -> Cil.CurrentLoc.set (Stmt.loc s)
      | None -> ()
    in
    let formals = Kernel_function.get_formals kf in
    let res = match Cil2cfg.node_type v with
      | Cil2cfg.Vstart ->
          Wp_parameters.fatal "No CFG edge can lead to Vstart"
      | Cil2cfg.VfctIn ->
          let obj = get_only_succ env cfg v in
          let obj = wp_scope wenv formals Mcfg.SC_Function_in obj in
          let obj = wp_scope wenv [] Mcfg.SC_Global obj in
          obj
      | Cil2cfg.VblkIn (Cil2cfg.Bfct, b) ->
          let obj = get_only_succ env cfg v in
          let obj = wp_scope wenv b.blocals Mcfg.SC_Block_in obj in
          wp_scope wenv formals Mcfg.SC_Function_frame obj
      | Cil2cfg.VblkIn (_, b) ->
          let obj = get_only_succ env cfg v in
          wp_scope wenv b.blocals Mcfg.SC_Block_in obj
      | Cil2cfg.VblkOut (_, _b) ->
          let obj = get_only_succ env cfg v in
          obj (* cf. blocks_closed_by_edge below *)
      | Cil2cfg.Vstmt s ->
          let obj = get_only_succ env cfg v in
          wp_stmt wenv s obj
      | Cil2cfg.Vcall (stmt, lval, fct, args) ->
          let en, ee = Cil2cfg.get_call_out_edges cfg v in
          let objn = get_wp_edge env en in
          let obje = get_wp_edge env ee in
          wp_calls env res v stmt lval fct args objn obje
      | Cil2cfg.Vtest (true, s, c) ->
          let et, ef = Cil2cfg.get_test_edges cfg v in
          let t_obj = get_wp_edge env et in
          let f_obj = get_wp_edge env ef in
          W.test wenv s c t_obj f_obj
      | Cil2cfg.Vtest (false, _, _) ->
          get_only_succ env cfg v
      | Cil2cfg.Vswitch (s, e) ->
          let cases, def_edge = Cil2cfg.get_switch_edges cfg v in
          let cases_obj = List.map (fun (c,e) -> c, get_wp_edge env e) cases in
          let def_obj = get_wp_edge env def_edge in
          W.switch wenv s e cases_obj def_obj
      | Cil2cfg.Vloop _ | Cil2cfg.Vloop2 _ ->
          let get_loop_head = fun n -> get_only_succ env cfg n in
          wp_loop env res v e get_loop_head
      | Cil2cfg.VfctOut
      | Cil2cfg.Vexit ->
          let obj = get_only_succ env cfg v (* exitpost / postcondition *) in
          wp_scope wenv formals Mcfg.SC_Function_out obj
      | Cil2cfg.Vend ->
          W.empty
          (* LC : unused entry point...
                    let obj = W.empty in
                    wp_scope wenv formals Mcfg.SC_Function_after_POST obj
          *)
    in
    let res =
      let blks = Cil2cfg.blocks_closed_by_edge cfg e in
      let free_locals res b = wp_scope wenv b.blocals Mcfg.SC_Block_out res in
      List.fold_left free_locals res blks
    in
    debug "[compute_edge] before %a done@." Cil2cfg.pp_node v;
    Cil.CurrentLoc.set old_loc;
    res

  (* Hypothesis for initialization of one global variable *)
  let rec init_global_variable wenv lv init obj =
    match init with

    | SingleInit exp ->
        W.init_value  wenv lv (Cil.typeOfLval lv)(Some exp) obj

    | CompoundInit ( ct , initl ) ->

        let len = List.length initl in
        let implicit_defaults =
          match ct with
          | TArray (ty,Some {enode = (Const CInt64 (size,_,_))},_,_)
            when Integer.lt (Integer.of_int len) size  ->

              W.init_range wenv lv ty
                (Int64.of_int len) (Integer.to_int64 size) obj

          | TComp (cp,_,_) when len < (List.length cp.cfields) ->

              List.fold_left
                (fun obj f ->
                   if List.exists
                       (function (Field(g,_),_) -> Fieldinfo.equal f g | _ -> false)
                       initl
                   then obj
                   else
                     W.init_value wenv
                       (Cil.addOffsetLval (Field(f, NoOffset)) lv)
                       f.ftype None obj)
                obj (List.rev cp.cfields)

          | _ -> obj
        in
        List.fold_left
          (fun obj (off,init) ->
             let lv = Cil.addOffsetLval off lv in
             init_global_variable wenv lv init obj)
          implicit_defaults (List.rev initl)


  (* WP of global initialisations. *)
  let process_global_init wenv kf obj =
    if WpStrategy.is_main_init kf then
      List.fold_left
        (fun obj global ->
           match global with
           | GVar (var, initinfo, loc) ->
               if var.vstorage = Extern then obj
               else
                 let old_loc = Cil.CurrentLoc.get () in
                 Cil.CurrentLoc.set loc ;
                 let obj =
                   match initinfo.init with
                   | None ->
                       W.init_value
                         wenv (Var var,NoOffset) var.vtype None obj
                   | Some init ->
                       let lv = Var var, NoOffset in
                       init_global_variable wenv lv init obj
                 in Cil.CurrentLoc.set old_loc ; obj
           | _ -> obj
        ) obj (Ast.get()).globals
    else
      obj

  let get_weakest_precondition cfg ((kf, _g, strategy, res, wenv) as env) =
    debug "[wp-cfg] start Pass1";
    Cil2cfg.iter_edges (fun e -> ignore (get_wp_edge env e)) cfg ;
    debug "[wp-cfg] end of Pass1";
    R.change_mode_if_needed res;
    (* Notice that [get_wp_edge] will start Pass2 if needed,
     * but if not, it will only fetch Pass1 result. *)
    let e_start = Cil2cfg.start_edge cfg in
    let obj = get_wp_edge env e_start in
    let obj = process_global_init wenv kf obj in
    let obj = match WpStrategy.strategy_kind strategy with
      | WpStrategy.SKannots -> obj
      | WpStrategy.SKfroms info ->
          let pre = info.WpStrategy.get_pre () in
          let pre = WpStrategy.get_hyp_only pre in
          W.build_prop_of_from wenv pre obj
    in
    debug "before close: %a@." W.pretty obj;
    W.close wenv obj

  let compute cfg strategy =
    debug "[wp-cfg] start computing with the strategy for %a"
      WpStrategy.pp_info_of_strategy strategy;
    if WpStrategy.strategy_has_prop_goal strategy 
    || WpStrategy.strategy_has_asgn_goal strategy then
      try
        let kf = Cil2cfg.cfg_kf cfg in

        if WpStrategy.new_loop_computation strategy then
          (match Cil2cfg.very_strange_loops cfg with [] -> ()
                                                   | _ -> (* TODO : print info about the loops *)
                                                       Wp_error.unsupported "strange loop(s).")
        else
          (match Cil2cfg.strange_loops cfg with [] -> ()
                                              | _ -> (* TODO : print info about the loops *)
                                                  Wp_error.unsupported
                                                    "non natural loop(s): try [-wp-invariants] option");

        let lvars = match WpStrategy.strategy_kind strategy with
          | WpStrategy.SKfroms info -> info.WpStrategy.more_vars 
          | _ -> []
        in
        let wenv = W.new_env ~lvars kf in
        let res = R.empty cfg in
        let env = (kf, cfg, strategy, res, wenv) in
        List.iter 
          (fun (pid,thm) -> W.add_axiom pid thm) 
          (WpStrategy.global_axioms strategy) ;
        let goal = get_weakest_precondition cfg env in
        debug "[get_weakest_precondition] %a@." W.pretty goal;
        let pp_cfg_edges_annot res fmt e =
          try W.pretty fmt (R.find res e)
          with Not_found -> Format.fprintf fmt "<released>"
        in
        let annot_cfg = pp_cfg_edges_annot res in
        debug "[wp-cfg] computing done.";
        [goal] , annot_cfg
      with Wp_error.Error (_, msg) ->
        Wp_parameters.warning "@[calculus failed on strategy@ @[for %a@]@ \
                               because@ %s (abort)@]" 
          WpStrategy.pp_info_of_strategy strategy
          msg;
        let annot_cfg fmt _e = Format.fprintf fmt "" in
        [], annot_cfg
    else
      begin
        debug "[wp-cfg] no goal in this strategy : ignore.";
        let annot_cfg fmt _e = Format.fprintf fmt "" in
        [], annot_cfg
      end

end
