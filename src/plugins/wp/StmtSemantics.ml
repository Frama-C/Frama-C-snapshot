(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Sigs
open Cil_types
open Cil_datatype
open Clabels

let not_yet = Wp_parameters.not_yet_implemented

module Make(Compiler:Sigs.Compiler) =
struct

  module Compiler = Compiler
  module Cfg = CfgCompiler.Cfg(Compiler.M.Sigma)
  module M = Compiler.M
  module Sigma = Compiler.M.Sigma
  module C  = Compiler.C
  module L = Compiler.L
  module A = Compiler.A

  type node = Cfg.node
  type goal = {
    goal_pred : Cfg.P.t;
    goal_prop : WpPropId.prop_id;
  }
  type cfg = Cfg.cfg
  type paths = {
    paths_cfg : cfg;
    paths_goals : goal Bag.t;
  }

  type env = {
    flow : node LabelMap.t ;
    kf : Kernel_function.t;
    result : Lang.F.var;
    return : typ ;
    (** used for substituting directly values without going through terms.
        Good for memory models, avoid unneeded conversions. *)
    subst_formals: (exp * node) Varinfo.Map.t;
    status : Lang.F.var;
  }

  exception LabelNotFound of c_label

  (* -------------------------------------------------------------------------- *)
  (* --- Env Utilities                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let result env = env.result

  let bind l n env =
    { env with flow = LabelMap.add l n env.flow }

  let (@^) cfg1 cfg2 = {
    paths_cfg = Cfg.concat cfg1.paths_cfg cfg2.paths_cfg;
    paths_goals = Bag.concat cfg1.paths_goals cfg2.paths_goals;
  }

  let (@*) env lns =
    let flow =
      List.fold_left (fun flow (l, n) -> LabelMap.add l n flow) env.flow lns
    in { env with flow }
  
  let (@:) env lbl =
    try
      LabelMap.find lbl env.flow
    with Not_found -> raise (LabelNotFound lbl)

  let (@-) env f =
    { env with flow = LabelMap.filter (fun lbl _ -> f lbl) env.flow }

  let empty_env kf  =
    let return = Kernel_function.get_return_type kf in
    let result = Lang.freshvar ~basename:"result" (Lang.tau_of_ctype return) in
    let status = Lang.freshvar ~basename:"status" Qed.Logic.Int in
    let env = {flow = LabelMap.empty; kf; result; status; return;
               subst_formals = Varinfo.Map.empty} in
    env @* [
      Clabels.init, Cfg.node ();
      Clabels.at_exit, Cfg.node();
    ]

  (* -------------------------------------------------------------------------- *)
  (* --- Paths & Cfg Utilities                                               --- *)
  (* -------------------------------------------------------------------------- *)
  
  let paths_of_cfg cfg = {
    paths_cfg = cfg;
    paths_goals = Bag.empty;
  }

  let nop = Cfg.nop |> paths_of_cfg
  let add_tmpnode n = Cfg.add_tmpnode n |> paths_of_cfg
  let goto n1 n2 = (Cfg.goto n1 n2) |> paths_of_cfg
  let meta ?stmt ?descr n = (Cfg.meta ?stmt ?descr n) |> paths_of_cfg
  let guard nc c nt = (Cfg.guard nc c nt) |> paths_of_cfg
  let guard' nc c nt = (Cfg.guard' nc c nt) |> paths_of_cfg
  let either n ns = (Cfg.either n ns) |> paths_of_cfg
  let implies n ns = (Cfg.implies n ns) |> paths_of_cfg
  let effect n1 e n2 = (Cfg.effect n1 e n2) |> paths_of_cfg
  let assume p = (Cfg.assume p) |> paths_of_cfg

  let current env sigma =
    Cfg.Node.(Map.add (env @: Clabels.here) sigma Map.empty)

  let goals_nodes goals =
    Bag.fold_left (fun acc g ->
        Cfg.Node.Map.fold
          (fun n _ acc -> Cfg.Node.Set.add n acc)
          (Cfg.P.reads g.goal_pred) acc
      )
      Cfg.Node.Set.empty
      goals

  (* -------------------------------------------------------------------------- *)
  (* --- Sequence & Parallel Compilation                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let rec sequence f env = function
    | [] -> goto (env @: Clabels.here) (env @: Clabels.next)
    | [ elt ] -> f env elt
    | stmt :: stmts ->
      let n = Cfg.node () in
      let paths = f (bind Clabels.next n env) stmt in
      paths @^ (sequence f (bind Clabels.here n env) stmts)

  let choice ?(pre=Clabels.here) ?(post=Clabels.next) f env =
    let pre_node = env @: pre in
    let apply f env elt =
      let n = Cfg.node () in
      n, f (bind pre n env) elt in
    let rec aux env ns = function
      | [] -> goto (env @: pre) (env @: post)
      | [ elt ] ->
        let n, paths = apply f env elt in
        paths @^ either pre_node (n :: ns)
      | elt :: elts ->
        let n, paths = apply f env elt in
        paths @^ (aux env (n :: ns) elts)
    in
    aux env []

  (** executed possibly at the same time *)
  let parallel ?(pre=Clabels.here) ?(post=Clabels.next) f env =
    let pre_node = env @: pre in
    let apply f env elt =
      let n = Cfg.node () in
      n, f (bind pre n env) elt in
    let rec aux env ns = function
      | [] -> goto (env @: pre) (env @: post)
      | [ elt ] ->
        let n, (c,paths) = apply f env elt in
        paths @^ implies pre_node ((c,n) :: ns)
      | elt :: elts ->
        let n, (c,paths) = apply f env elt in
        paths @^ (aux env ((c,n) :: ns) elts)
    in
    aux env []

  (* -------------------------------------------------------------------------- *)
  (* --- Compiler: Scope                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let scope env sc xs =
    let post = Sigma.create () in
    let pre = M.alloc post xs in
    let seq = { pre ; post } in
    let p = Lang.F.p_conj (M.scope seq sc xs) in
    let e = Cfg.E.create seq p in
    let descr =
      Format.asprintf "%s scope [%a]: @[%a@]"
        (match sc with Leave -> "Leaving" | Enter -> "Entering")
        (Pretty_utils.pp_iter ~sep:"; @" List.iter Varinfo.pretty) xs
        Cfg.E.pretty e
    in
    meta ~descr (env @: Clabels.here)
    @^ effect (env @: Clabels.here) e (env @: Clabels.next)

  (* -------------------------------------------------------------------------- *)
  (* --- Compiler: Assignment                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let set env lv exp =
    let here = Sigma.create () in
    let loc = C.lval here lv in
    let value = C.exp here exp in
    let obj = Ctypes.object_of (Cil.typeOfLval lv) in
    let next = Sigma.havoc here (M.domain obj loc) in
    let sequence = { pre=here ; post=next } in
    let ps =
      match value with
      | Loc ptr -> M.copied sequence obj loc ptr
      | Val term -> M.stored sequence obj loc term in
    let ps = List.map Cvalues.equation ps in
    let e = Cfg.E.create sequence (Lang.F.p_conj ps) in
    let descr = Format.asprintf "Set: @[%a = %a@]"
        Printer.pp_lval lv Printer.pp_exp exp in
    meta ~descr (env @: Clabels.here)
    @^ effect ( env @: Clabels.here ) e (env @: Clabels.next)

  (* -------------------------------------------------------------------------- *)
  (* --- Compiler: Return                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let return env e_opt =
    goto (env @: Clabels.here) (env @: Clabels.next)
    @^
    match e_opt with
    | None -> nop
    | Some exp ->
        let rtyp = env.return in
        let here = Sigma.create () in
        let value = C.return here rtyp exp in
        let p = Lang.F.p_equal (Lang.F.e_var env.result) value in
        assume (Cfg.P.create (current env here) p)

  (* -------------------------------------------------------------------------- *)
  (* --- Compiler: Assertion                                                --- *)
  (* -------------------------------------------------------------------------- *)

  let mk_frame ~descr env =
    let nsigmas =
      LabelMap.fold
        (fun _ (n : node) (nmap : M.sigma Cfg.Node.Map.t) ->
           if Cfg.Node.Map.mem n nmap then nmap
           else Cfg.Node.Map.add n (Sigma.create ()) nmap)
        env.flow
        Cfg.Node.Map.empty
    in
    let lsigmas =
      LabelMap.map
        (fun n ->
           try Cfg.Node.Map.find n nsigmas
           with Not_found -> assert false (* by nsigmas *))
        env.flow
    in
    let frame_formals = L.mk_frame
        ~kf:env.kf
        ~descr:"frame_formals"
        ~labels:LabelMap.empty
        ()
    in
    let formals = Varinfo.Map.map (fun (exp,n) ->
        try
          let here = Cfg.Node.Map.find n nsigmas in
          L.in_frame frame_formals (C.exp here) exp
        with Not_found -> Wp_parameters.fatal "node of formals not present in labels. normal?"
      ) env.subst_formals
    in
    let frame =
      L.mk_frame
        ~labels:lsigmas ~kf:env.kf
        ~result:(Sigs.R_var env.result)
        ~status:env.status
        ~formals ~descr ()
    in
    frame, nsigmas, lsigmas

  let pred
    : env -> Sigs.polarity -> predicate -> Interpreted_automata.guard_kind -> _
    = fun env polarity p truth ->
      (* Format.printf "env.flow: %a@." *)
      (*   (Pretty_utils.pp_iter2 LabelMap.iter Label.pretty Cfg.Node.pp) *)
      (*   env.flow; *)
      let frame, nsigmas, lsigmas = mk_frame ~descr:"pred" env in
      try
        let here = LabelMap.find Clabels.here lsigmas in
        let lenv = L.mk_env ~here () in
        let pred = L.in_frame frame (L.pred polarity lenv) p in
        let pred = if truth = Interpreted_automata.Then then pred else Lang.F.p_not pred in
        (** Remove the sigmas not used for the compilation, but here must stay *)
        let nsigmas = Cfg.Node.Map.filter (fun _ s ->
            s == here || not (Sigma.Chunk.Set.is_empty (Sigma.domain s))
          ) nsigmas
        in
        (Cfg.P.create nsigmas pred)
      with Not_found -> Wp_parameters.fatal "Error during compilation"

  let assert_ env p v prop_id =
    let pos = pred env `Positive p.ip_content v in
    let env' = env @* [Clabels.here, env @: Clabels.next ] in
    let neg = pred env' `Negative p.ip_content v in
    let goal = {
      goal_pred = pos;
      goal_prop = prop_id;
    } in
    {
      paths_goals = Bag.elt goal;
      paths_cfg = Cfg.goto (env @: Clabels.here) (env @: Clabels.next);
    } @^ assume neg


  let assume_
    : env -> Sigs.polarity -> predicate -> paths
    = fun env polarity p ->
    assume (pred env polarity p Interpreted_automata.Then)

  (* -------------------------------------------------------------------------- *)
  (* --- Compiler: Function Call                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let rec call_kf
    : env -> lval option -> kernel_function -> exp list -> paths
    = fun env lvr kf es ->
      let pre_node = Cfg.node () in
      let post_node = Cfg.node () in
      let return_node = Cfg.node () in
      let next_node = env @: Clabels.next in
      let exit_stop = Cfg.node () in

      (* Caller's context: sigma, frame and actuals evaluated to this sigma *)
      let cfg_enter_scope =
        scope
          (env @* [Clabels.next,pre_node]) (* Clabels.here is here *)
          Enter (Kernel_function.get_formals kf)
      in
      let cfg_leave_scope =
        scope
          (env @* [Clabels.here,post_node;Clabels.next,return_node])
          Leave (Kernel_function.get_formals kf)
      in

      let cfg_contract env =
        spec env (Annotations.funspec kf)
      in

      let result env = match lvr with
        | None -> goto (env @: Clabels.here) (env @: Clabels.next)
        | Some lv ->
            let pre = Sigma.create () in
            let tr = Cil.typeOfLval lv in
            let obj = Ctypes.object_of tr in
            let loc = C.lval pre lv in
            let post = Sigma.havoc pre (M.domain obj loc) in
            let vr = M.load post obj loc in
            let p =
              C.equal_typ tr vr
                (C.cast tr env.return (Val (Lang.F.e_var env.result)))
            in
            let e = Cfg.E.create { pre; post } p in
            effect (env @: Clabels.here) e (env @: Clabels.next)
      in

      let old_status = env.status in

      let exit_status (env:env) =
        let p = Lang.F.p_equal (Lang.F.e_var old_status) (Lang.F.e_var env.status) in
        let s = M.Sigma.create () in
        let e = Cfg.E.create {pre=s;post=s} p in
        effect (env @: Clabels.here) e (env @: Clabels.next)
      in

      let subst_formals = List.fold_left2
          (fun acc v e -> Varinfo.Map.add v (e,pre_node) acc)
          Varinfo.Map.empty (Kernel_function.get_formals kf) es
      in

      let env_call =
        { (empty_env kf) with subst_formals }
        @* [Clabels.init, env @: Clabels.init;
            Clabels.pre, pre_node; Clabels.here, pre_node;
            Clabels.next, post_node; Clabels.post, post_node;
            Clabels.at_exit, env @: Clabels.at_exit]
      in

      (* TODO: Call inlining. *)
      nop
      @^ cfg_enter_scope
      @^ cfg_contract env_call
      @^ cfg_leave_scope
      @^ result (env_call @* [(Clabels.here, return_node);
                              (Clabels.next, next_node)])
      @^ exit_status (env_call @* [(Clabels.here, exit_stop);
                                   (Clabels.next, env @: Clabels.at_exit)])

  and call
    : env -> lval option -> exp -> exp list -> paths
    = fun env lv e es ->
      match Kernel_function.get_called e with
      | Some kf -> call_kf env lv kf es
      | None -> not_yet "[StmtSemantics] Call through a function pointer."

  (* -------------------------------------------------------------------------- *)
  (* --- Compiler: Instruction                                              --- *)
  (* -------------------------------------------------------------------------- *)

  and instr : env -> instr -> paths = fun env -> function
    | Set (lv, e, _) -> set env lv e
    | Call (lv, e, es, _) -> call env lv e es
    | Asm _ -> not_yet "[StmtSemantics] Inline Asm."
    | Local_init (v, ConsInit(f, args, kind), loc) ->
        Cil.treat_constructor_as_func
          (fun lv e es _ -> call env lv e es)
          v f args kind loc
    | Local_init (vi, AssignInit init, _) ->
        let here = Sigma.create () in
        let next = Sigma.create () in
        (*TODO: make something of warnings *)
        let hyp = Lang.F.p_all snd (C.init ~sigma:next vi (Some init)) in
        effect (env @: Clabels.here) (Cfg.E.create {pre=here; post=next} hyp) (env @: Clabels.next)
    | Skip _ | Code_annot _ -> goto (env @: Clabels.here) (env @: Clabels.next)

  (* -------------------------------------------------------------------------- *)
  (* --- Compiler: Annotations                                              --- *)
  (* -------------------------------------------------------------------------- *)

  and spec : env -> spec -> paths = fun env spec ->
    let pre_cond env p prop_id =
      assert_ env p Interpreted_automata.Then prop_id
    in
    let post_cond termination_kind env (tk, ip) =
      if tk = termination_kind then
        assume_ env `Positive ip.ip_content
      else nop
    in
    let behavior env b =
      let nrequires = Cfg.node () in
      let nassigns = Cfg.node () in
      let assume =
        let p = pred (env @* [Clabels.here, env @: Clabels.pre])
            `Negative (Ast_info.behavior_assumes b) Interpreted_automata.Then in
        match Cfg.P.to_condition p with
        | Some (c,None) -> c
        | Some (c,Some n) when Cfg.Node.equal n (env @: Clabels.pre) -> c
        | _ -> not_yet "assume of behaviors with labels: %a" Cfg.P.pretty p
      in
      let post_normal_behavior = Cfg.node () in
      let post_normal_env = env @* [Clabels.here, nassigns;
                                    Clabels.post, post_normal_behavior] in
      let post_at_exit_behavior = Cfg.node () in
      let post_at_exit_env = env @* [Clabels.here, nassigns;
                                     Clabels.post, post_at_exit_behavior] in
      assume,
      sequence
        (fun env ip ->
           (** TODO: Kglobal is it always Kglobal ? *)
           let prop_id = WpPropId.mk_pre_id env.kf Kglobal b ip in
           pre_cond env ip prop_id)
         (env @* [Clabels.next, nrequires]) b.b_requires
      @^ assigns (env @* [Clabels.here, nrequires; Clabels.next, nassigns]) b.b_assigns
      @^ either nassigns [post_normal_behavior;post_at_exit_behavior]
      @^ List.fold_left
        (fun acc post -> acc @^ post_cond Normal post_normal_env post)
        nop b.b_post_cond
      @^ List.fold_left
        (fun acc post -> acc @^ post_cond Exits post_at_exit_env post)
        nop b.b_post_cond
      @^ goto post_normal_behavior  (env @: Clabels.post)
      @^ goto post_at_exit_behavior (env @: Clabels.at_exit)
    in
    let env = env @* [Clabels.here, env @: Clabels.pre; Clabels.next, env @: Clabels.post] in
    parallel behavior env spec.spec_behavior

  and assigns : env -> assigns -> paths = fun env a ->
    let frame, _,  _ = mk_frame "assigns" env in
    let lenv = L.mk_env () in (* TODO: lenv for ghost code. *)
    let here = Sigma.create () in
    let authorized_region = L.in_frame frame
        (L.assigned_of_assigns ~unfold:false lenv) a in
    match authorized_region with
    | None -> goto (env @: Clabels.here) (env @: Clabels.next)
    | Some region ->
        let domain = A.domain region in
        let next = M.Sigma.havoc here domain in
        let seq = { pre = here; post = next } in
        let preds = A.apply_assigns seq region in
        effect (env @: Clabels.here) (Cfg.E.create seq (Lang.F.p_conj preds)) (env @: Clabels.next)

  and froms : env -> from list -> paths = fun env froms ->
    assigns env (Writes froms)

  (* -------------------------------------------------------------------------- *)
  (* --- Automaton                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let pref v1 v2 =
    let open Interpreted_automata in
    match v1.vertex_info, v2.vertex_info with
    | NoneInfo, NoneInfo -> 0
    | NoneInfo, _ -> -1
    | _ , NoneInfo -> 1
    | LoopHead i, LoopHead j -> Pervasives.compare j i

  module Automata = Interpreted_automata.UnrollUnnatural.Version
  type nodes = {
    global: node Automata.Hashtbl.t;
    local: node Automata.Map.t;
  }

  let get_node nodes v =
    try Automata.Map.find v nodes.local
    with Not_found ->
      Automata.Hashtbl.memo nodes.global v (fun _ -> Cfg.node ())

  let add_local nodes v n =
    {nodes with local = Automata.Map.add v n nodes.local}

  let transition
    : env -> nodes -> Automata.t Interpreted_automata.transition -> paths
    = fun env nodes tr ->
    let open Interpreted_automata in
    match tr with
    | Skip | Enter { blocals = [] } | Leave { blocals = [] } ->
        goto (env @: Clabels.here) (env @: Clabels.next)
    | Enter {blocals} -> scope env Sigs.Enter blocals
    | Leave {blocals} -> scope env Sigs.Leave blocals
    | Return (r,_) -> return env r
    | Prop ((Assert|Invariant),p,v,l,ip) ->
        let env = Logic_label.Map.fold
            (fun logic_label vertex acc ->
               let c_label = Clabels.of_logic logic_label in
               let node = get_node nodes vertex in
               bind c_label node acc
            ) l env in
        assert_ env p v (WpPropId.mk_property ip)
    | Prop (Assume,p,v,l,_) ->
        let env = Logic_label.Map.fold
            (fun logic_label vertex acc ->
               let c_label = Clabels.of_logic logic_label in
               let node = get_node nodes vertex in
               bind c_label node acc
            ) l env in
        assume (pred env `Negative p.ip_content v) @^
        goto (env @: Clabels.here) (env @: Clabels.next)
    | Prop (_,_,_,_,_) ->
      not_yet "[StmtSemantics] Annots other than 'assert'"
    | Guard (exp,b,_) ->
        let here = Sigma.create () in
        let cond = C.cond here exp in
        let condition = Cfg.C.create here cond in
        (if b = Then then guard else guard')
          (env @: Clabels.here) condition (env @: Clabels.next)
    | Instr (i,_) -> instr env i

  let rec get_invariants g n (l:Automata.t Wto.partition) =
    let open Interpreted_automata in
    let open Interpreted_automata.UnrollUnnatural in
    match l, G.succ_e g n with
    | (Wto.Node a)::l,
      [(_,{edge_transition =
             (Prop((Assert|Invariant|Assume|Check),_,_,_,_) | Skip) as t},b)]
      when Automata.equal a b ->
        let invs,l = get_invariants g b l in
        (t,a)::invs,l
    | _ -> [],(Wto.Node n)::l

  let as_assumes l =
    let open Interpreted_automata in
    List.map (function
        | (Prop(Assume,_,_,_,_),_) as t -> t
        | (Prop((Assert|Invariant),a1,a2,a3,a4),b) -> (Prop(Assume,a1,a2,a3,a4),b)
        | (Prop(Check,_,_,_,_),b) -> (Skip,b)
        | (Skip,_) as t -> t
        | _ -> assert false
      ) l

  let automaton : env -> Interpreted_automata.automaton -> paths = fun env a ->
    let open Interpreted_automata in
    let wto = WTO.partition ~pref ~init:a.entry_point ~succs:(G.succ a.graph) in
    let index = Compute.build_wto_index_table wto in

    (*
    let cout = open_out "/tmp/automata.dot" in
    Interpreted_automata.output_to_dot cout ~wto ~number:`Vertex a;
    close_out cout;
    *)

    let open UnrollUnnatural in
    let g = unroll_unnatural_loop a wto index in

    let here = (a.entry_point,Vertex.Set.empty) in
    let next = (a.return_point,Vertex.Set.empty) in
    let wto =
      WTO.partition
        ?pref:None (* natural loop keep the heads *)
        ~succs:(UnrollUnnatural.G.succ g)
        ~init:here in

    let do_node nodes v paths =
      let n = get_node nodes v in
      let l,paths = G.fold_succ_e (fun (_,e,v2) (l,paths) ->
          let n2' = Cfg.node () in
          let n2 = get_node nodes v2 in
          (n2'::l, transition (env @* [Clabels.here,n2';Clabels.next,n2]) nodes e.edge_transition
                   @^ add_tmpnode n2'
                   @^ paths)
        ) g v ([],paths) in
      (either n l) @^ paths
    in
    let rec do_list ~fresh_nodes paths nodes n1 = function
      | [] -> (n1,paths)
      | (t,b)::l ->
          let n2, nodes =
            if fresh_nodes then
              let n2 = Cfg.node () in
              let nodes = add_local nodes b n2 in
              n2, nodes
            else (get_node nodes b), nodes
          in
          let paths = paths @^ transition (env @* [Clabels.here,n1;Clabels.next,n2]) nodes t in
          do_list ~fresh_nodes paths nodes n2 l
    in
    let rec component nodes paths = function
      | Wto.Node v -> do_node nodes v paths
      | Wto.Component (v,l) ->
          assert (not (Automata.Map.mem v nodes.local));
          let invariants,l = get_invariants g v l in
          let n = get_node {nodes with local = Automata.Map.empty} v in
          (* initialization *)
          let n,paths = do_list ~fresh_nodes:true paths nodes n invariants in
          (* preservation *)
          let n_loop = Cfg.node () in
          let _,paths = do_list ~fresh_nodes:true paths nodes n_loop invariants in
          (* arbitrary number of loop *)
          let n_havoc = Cfg.node () in
          let havoc = Cfg.havoc n ~effects:{pre=n_havoc;post=n_loop} n_havoc in
          let paths = (havoc |> paths_of_cfg) @^ paths in
          (* body *)
          let invariants_as_assumes = as_assumes invariants in
          let _,paths =
            do_list ~fresh_nodes:false paths (add_local nodes v n_havoc)
              n_havoc invariants_as_assumes in
          partition (add_local nodes v n_loop) paths l
    and partition nodes paths l =
      List.fold_left (component nodes) paths l
    in
    let nodes = {
      global = Automata.Hashtbl.create 10;
      local = Automata.Map.empty
    } in
    Automata.Hashtbl.add nodes.global here (env @: Clabels.here);
    Automata.Hashtbl.add nodes.global next (env @: Clabels.next);
    partition nodes nop wto

  (** connect init to here. [is_pre_main] indicate if here is the
     pre-state of main. *)
  let init ~is_pre_main env =
    let ninit = (env @: Clabels.init) in
    let sinit = Sigma.create () in
    (** todo WpStrategy.is_main_init, need to test that seq.pre is the
        start of the function *)
    (** todo warning *)
    let cfg_init = Globals.Vars.fold_in_file_order
        (fun var initinfo cfg ->
           if var.vstorage = Extern then cfg else
             let h = Lang.F.p_all snd (C.init ~sigma:sinit var initinfo.init) in
             let h = Cfg.P.create
                 (Cfg.Node.Map.add ninit sinit Cfg.Node.Map.empty)
                 h
             in
             assume h
        ) nop
    in
    if is_pre_main
    then cfg_init @^ goto ninit (env @: Clabels.here)
    else
      let nconst = Cfg.Node.create () in
      let sconst = Sigma.havoc_any ~call:false sinit in
      let havoc = Cfg.E.create {pre=sinit; post=sconst} Lang.F.p_true in
      let consts =
        if WpStrategy.isInitConst () then
          Globals.Vars.fold_in_file_order
            (fun var _ cfg ->
               if WpStrategy.isGlobalInitConst var
               then
                 let h = (C.unchanged sconst sinit var) in
                 let h = Cfg.P.create
                     (Cfg.Node.Map.add ninit sinit
                        (Cfg.Node.Map.add nconst sconst
                           Cfg.Node.Map.empty))
                     h
                 in
                 cfg @^ assume h
               else cfg
            ) nop
        else nop
    in
    cfg_init @^ effect ninit havoc nconst @^ consts @^ goto nconst (env @: Clabels.here)

  let pre_spec env spec =
    let pre_cond polarity env p =
      assume_ (env @* [Clabels.here, env @: Clabels.pre]) polarity p
    in
    let behavior env b =
      let assume =
        let p = pred env `Negative (Ast_info.behavior_assumes b) Interpreted_automata.Then in
        match Cfg.P.to_condition p with
        | Some (c,None) -> c
        | Some (c,Some n) when Cfg.Node.equal n (env @: Clabels.here) -> c
        | _ -> not_yet "assume of behaviors with labels: %a" Cfg.P.pretty p
      in
      assume,
      List.fold_left
        (fun acc ip -> acc @^ pre_cond `Negative env ip.ip_content)
        nop b.b_requires
      @^ goto (env @: Clabels.here) (env @: Clabels.next)
    in
    parallel behavior env spec.spec_behavior

  let post_normal_spec env spec =
    let post_cond termination_kind env (tk, ip) propid =
      if tk = termination_kind then
        assert_ env ip Interpreted_automata.Then propid
      else nop
    in
    let behavior env b =
      let assume =
        let p = pred (env @* [Clabels.here, env @: Clabels.pre])
            `Negative (Ast_info.behavior_assumes b)
            Interpreted_automata.Then
        in
        match Cfg.P.to_condition p with
        | Some (c,None) -> c
        | Some (c,Some n) when Cfg.Node.equal n (env @: Clabels.pre) -> c
        | _ -> not_yet "assume of behaviors with labels: %a" Cfg.P.pretty p
      in
      assume,
      sequence
        (fun env post ->
           let propid = WpPropId.mk_fct_post_id env.kf b post in
           post_cond Normal env post propid)
        env b.b_post_cond
    in
    let env = env in
    parallel behavior env spec.spec_behavior


  let compute_kf kf =
    let autom = Interpreted_automata.Compute.get_automaton ~annotations:true kf in
    (* let cout = open_out (Format.sprintf "/tmp/cfg_automata_%s.dot" (Kernel_function.get_name kf)) in
     * Interpreted_automata.Compute.output_to_dot cout autom;
     * close_out cout; *)
    let nprepre = Cfg.node () in
    let npre = Cfg.node () in
    let npost = Cfg.node () in
    let npostpost = Cfg.node () in
    let env = empty_env kf  in
    let env = env @* [Clabels.pre,npre;Clabels.post,npost] in
    let init = init ~is_pre_main:(WpStrategy.is_main_init kf)
        (env @* [Clabels.here,nprepre]) in
    let kf_spec = Annotations.funspec kf in
    let pre = pre_spec (env @* [Clabels.here,nprepre;Clabels.next,npre]) kf_spec in
    let paths = automaton (env @* [Clabels.here,npre;Clabels.next,npost]) autom in
    let post = post_normal_spec (env @* [Clabels.here,npost;Clabels.next,npostpost]) kf_spec in
    init @^ pre @^ paths @^ post, env @: Clabels.init
end
