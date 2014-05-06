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

(* -------------------------------------------------------------------------- *)
(* --- WP Calculus                                                        --- *)
(* -------------------------------------------------------------------------- *)

open LogicUsage
open Cil_types
open Cil_datatype
open WpPropId
open Clabels
open Qed
open Lang
open Lang.F
open Memory
open Wpo

module WpLog = Wp_parameters

module VC( M : Memory.Model ) =
struct

  open M
  module V = Vars
  module P = WpPropId.PropId
  module C = CodeSemantics.Make(M)
  module L = LogicSemantics.Make(M)
  module A = LogicAssigns.Make(M)(C)(L)

  type target =
    | Gprop of P.t
    | Geffect of P.t * Stmt.t * effect_source
    | Gposteffect of P.t

  module TARGET = 
  struct
    type t = target
    let hsrc = function 
      | FromCode -> 1 | FromCall -> 2 | FromReturn -> 3
    let hash = function
      | Gprop p | Gposteffect p -> P.hash p
      | Geffect(p,s,e) -> P.hash p * 37 + 41 * Stmt.hash s + hsrc e
    let compare g1 g2 =
      if g1 == g2 then 0 else
        match g1,g2 with
        | Gprop p1 , Gprop p2 -> P.compare p1 p2
        | Gprop _ , _ -> (-1)
        | _ , Gprop _ -> 1
        | Geffect(p1,s1,e1) , Geffect(p2,s2,e2) ->
            let c = P.compare p1 p2 in
            if c <> 0 then c else
              let c = Stmt.compare s1 s2 in
              if c <> 0 then c else
                hsrc e1 - hsrc e2
        | Geffect _ , _ -> (-1)
        | _ , Geffect _ -> 1
        | Gposteffect p1 , Gposteffect p2 -> P.compare p1 p2
    let equal g1 g2 = (compare g1 g2 = 0)
    let prop_id = function Gprop p | Gposteffect p | Geffect(p,_,_) -> p
    let source = function Gprop _ | Gposteffect _ -> None | Geffect(_,s,e) -> Some(s,e)
    let pretty fmt = function
      | Gprop p -> WpPropId.pretty fmt p
      | Geffect(p,s,FromCode) -> Format.fprintf fmt "%a at sid:%d" WpPropId.pretty p s.sid
      | Geffect(p,s,FromCall) -> Format.fprintf fmt "Call %a at sid:%d" WpPropId.pretty p s.sid
      | Geffect(p,s,FromReturn) -> Format.fprintf fmt "Return %a at sid:%d" WpPropId.pretty p s.sid
      | Gposteffect p -> Format.fprintf fmt "%a post-effect" WpPropId.pretty p
  end

  type effect = {
    e_pid : P.t ; (* Assign Property *)
    e_kind : a_kind ; (* Requires post effects (in case of loop-assigns) *)
    e_label : c_label ; (* scope for collection *)
    e_valid : L.sigma ; (* sigma where locations are filtered for validity *)
    e_region : A.region ; (* expected from spec *)
    e_warn : Warning.Set.t ; (* from translation *)
  }

  module EFFECT = 
  struct
    type t = effect
    let compare e1 e2 = P.compare e1.e_pid e2.e_pid
  end

  module G = Qed.Collection.Make(TARGET)
  module W = Warning.Set
  module D = Property.Set
  module S = Stmt.Set
  module Eset = FCSet.Make(EFFECT)
  module Gset = G.Set
  module Gmap = G.Map

  type vc = {
    hyps : Conditions.bundle ;
    goal : F.pred ;
    vars : Vars.t ; (* the variables of effects/goal to collect *)
    warn : W.t ;
    deps : D.t ;
    path : S.t ;
  }

  (* -------------------------------------------------------------------------- *)
  (* --- MCFG Interface                                                     --- *)
  (* -------------------------------------------------------------------------- *)

  type t_env = {
    frame : L.frame ;
    main : L.env ;
  }

  type t_prop = { 
    sigma : L.sigma option ;
    effects : Eset.t ;
    vcs : vc Splitter.t Gmap.t ;
  }

  (* -------------------------------------------------------------------------- *)
  (* --- MCFG Pretty                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let pp_vc fmt vc = 
    Format.fprintf fmt "%a@ @[<hov 2>Prove %a@]" 
      Conditions.dump vc.hyps 
      F.pp_pred vc.goal

  let pp_vcs fmt vcs =
    let k = ref 0 in
    Splitter.iter
      (fun tags vc -> 
         incr k ;
         begin
           match tags with
           | [] -> ()
           | t::ts ->
               Format.fprintf fmt " (%a" Splitter.pretty t ;
               List.iter (fun t -> Format.fprintf fmt ",%a" Splitter.pretty t) ts ;
               Format.fprintf fmt ")@\n" ;
         end ;
         Format.fprintf fmt "@[<hov 5> (%d) %a@]@\n" !k pp_vc vc)
      vcs

  let pp_gvcs fmt gvcs = 
    Gmap.iter_sorted
      (fun goal vcs ->
         let n = Splitter.length vcs in
         Format.fprintf fmt "Goal %a: (%d)@\n" TARGET.pretty goal n ;
         pp_vcs fmt vcs ;
      ) gvcs

  let pretty fmt wp =
    begin
      (match wp.sigma with None -> () | Some s ->
        Format.fprintf fmt "Sigma:@[<hov 2>%a@]@\n" Sigma.pretty s) ;
      pp_gvcs fmt wp.vcs ;
    end

  (* -------------------------------------------------------------------------- *)
  (* --- Utilities                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let empty_vc = { 
    hyps = Conditions.empty ;
    goal = p_true ;
    vars = V.empty ;
    warn = W.empty ; 
    deps = D.empty ;
    path = S.empty ;
  }

  let sigma_opt = function None -> Sigma.create () | Some s -> s
  let sigma_at w = sigma_opt w.sigma
  let sigma_any ~call w = match w.sigma with
    | None -> Sigma.create ()
    | Some s -> Sigma.havoc_any ~call s
  let sigma_union s1 s2 =
    match s1 , s2 with
    | None , s | s , None -> sigma_opt s , Passive.empty , Passive.empty
    | Some s1 , Some s2 -> Sigma.merge s1 s2
  let merge_sigma s1 s2 =
    match s1 , s2 with
    | None , s | s , None -> s , Passive.empty , Passive.empty
    | Some s1 , Some s2 -> let s,p1,p2 = Sigma.merge s1 s2 in Some s,p1,p2

  let join_with s = function None -> Passive.empty | Some s' -> Sigma.join s s'

  let occurs_vc vc x = 
    Vars.mem x vc.vars || Conditions.occurs x vc.hyps

  let intersect_vc vc p = 
    Vars.intersect (F.varsp p) vc.vars || Conditions.intersect p vc.hyps

  let assume_vc ~descr ?hpid ?stmt ?warn hs vc =
    if hs = [] && warn = None then vc 
    else
      let path = match stmt with
        | None -> vc.path
        | Some s -> S.add s vc.path in
      let deps = match hpid with 
        | None -> [] | Some p -> [WpPropId.property_of_id p] in
      let dset = List.fold_right D.add deps vc.deps in
      let wrns = match warn with
        | None -> vc.warn
        | Some w -> Warning.Set.union w vc.warn in
      { 
        hyps = Conditions.assume ~descr ?stmt ?warn ~deps (F.p_conj hs) vc.hyps ;
        goal = vc.goal ;
        vars = vc.vars ;
        warn = wrns ;
        deps = dset ; 
        path = path ;
      }

  let passify_vc pa vc =
    let hs = Passive.conditions pa (occurs_vc vc) in
    assume_vc ~descr:"Control Flow" hs vc

  (* -------------------------------------------------------------------------- *)
  (* --- Branching                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let branch_vc ~stmt cond vc1 vc2 =
    let hyps , goal = 
      if F.eqp vc1.goal vc2.goal then
        begin
          Conditions.branch ~descr:"Conditional" ~stmt cond vc1.hyps vc2.hyps ,
          vc1.goal
        end
      else
        let k = F.e_var (Lang.freshvar ~basename:"K" Logic.Bool) in
        let p = F.p_equal k F.e_true in
        let q = F.p_equal k F.e_false in
        let h1 = Conditions.assume ~descr:"Then Case" p vc1.hyps in
        let h2 = Conditions.assume ~descr:"Else Case" q vc2.hyps in
        (Conditions.branch ~descr:"Conditional"
           ~stmt cond h1 h2 , F.p_if p vc1.goal vc2.goal)
    in
    {
      hyps = hyps ;
      goal = goal ;
      vars = V.union vc1.vars vc2.vars ;
      deps = D.union vc1.deps vc2.deps ;
      warn = W.union vc1.warn vc2.warn ;
      path = S.union vc1.path vc2.path ;
    }

  (* -------------------------------------------------------------------------- *)
  (* --- Merging                                                            --- *)
  (* -------------------------------------------------------------------------- *)

  let merge_vc vc1 vc2 =
    let hyps , goal =
      if F.eqp vc1.goal vc2.goal then
        Conditions.merge [vc1.hyps;vc2.hyps] , vc1.goal
      else
        let k = F.e_var (Lang.freshvar ~basename:"K" Logic.Bool) in
        let p = F.p_equal k F.e_true in
        let q = F.p_equal k F.e_false in
        let h1 = Conditions.assume ~descr:"Case A" p vc1.hyps in
        let h2 = Conditions.assume ~descr:"Case B" q vc2.hyps in
        (Conditions.merge [h1 ; h2] , F.p_if p vc1.goal vc2.goal)
    in
    {
      hyps = hyps ;
      goal = goal ;
      vars = V.union vc1.vars vc2.vars ;
      deps = D.union vc1.deps vc2.deps ;
      warn = W.union vc1.warn vc2.warn ;
      path = S.union vc1.path vc2.path ;
    }

  let merge_vcs = function
    | [] -> empty_vc
    | [vc] -> vc
    | vcs ->
        let hyps = Conditions.merge (List.map (fun vc -> vc.hyps) vcs) in
        let goal = p_all (fun vc -> vc.goal) vcs in
        let vars = List.fold_left (fun d vc -> V.union d vc.vars) V.empty vcs in
        let deps = List.fold_left (fun d vc -> D.union d vc.deps) D.empty vcs in
        let warn = List.fold_left (fun d vc -> W.union d vc.warn) W.empty vcs in
        let path = List.fold_left (fun d vc -> S.union d vc.path) S.empty vcs in
        { hyps = hyps ;
          goal = goal ; 
          vars = vars ; 
          deps = deps ; 
          warn = warn ; 
          path = path }

  (* -------------------------------------------------------------------------- *)
  (* --- Merging and Branching with Splitters                               --- *)
  (* -------------------------------------------------------------------------- *)

  let gmerge = Gmap.union (fun _gid -> Splitter.union merge_vc)

  let gmap phi vcs = 
    Gmap.map (Splitter.map phi) vcs

  let gbranch ~left ~both ~right vcs1 vcs2 = 
    Gmap.merge
      (fun _g w1 w2 ->
         match w1 , w2 with
         | None , None -> None
         | Some vcs1 , None -> 
             Some (Splitter.map left vcs1)
         | None , Some vcs2 -> 
             Some (Splitter.map right vcs2)
         | Some vcs1 , Some vcs2 ->
             Some (Splitter.merge ~left ~both ~right vcs1 vcs2)
      ) vcs1 vcs2

  let merge_all_vcs : vc Splitter.t Gmap.t list -> vc Splitter.t Gmap.t =
    fun cases ->
      let targets = List.fold_left
          (fun goals vcs -> Gset.union goals (Gmap.domain vcs))
          Gset.empty cases in
      let goal g vcs = try Gmap.find g vcs with Not_found -> Splitter.empty in
      Gset.mapping
        (fun g -> Splitter.merge_all merge_vcs (List.map (goal g) cases))
        targets

  (* -------------------------------------------------------------------------- *)
  (* --- Merge for Calculus                                                 --- *)
  (* -------------------------------------------------------------------------- *)

  let empty = { 
    sigma = None ;
    effects = Eset.empty ;
    vcs = Gmap.empty ;
  }

  let merge wenv wp1 wp2 =
    L.in_frame wenv.frame
      (fun () ->
         let sigma,pa1,pa2 = merge_sigma wp1.sigma wp2.sigma in
         let effects = Eset.union wp1.effects wp2.effects in
         let vcs1 = gmap (passify_vc pa1) wp1.vcs in
         let vcs2 = gmap (passify_vc pa2) wp2.vcs in
         let vcs = gmerge vcs1 vcs2 in
         { sigma = sigma ; vcs = vcs ; effects = effects }
      ) ()

  (* -------------------------------------------------------------------------- *)
  (* --- Environment                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let new_env ?(lvars=[]) kf =
    let frame = L.frame kf in
    let env = L.in_frame frame L.new_env lvars in
    { frame = frame ; main = env }

  let in_wenv 
      (wenv:t_env) (wp:t_prop)
      (phi:L.env -> t_prop -> 'a) : 'a =
    L.in_frame wenv.frame 
      (fun wp -> 
         match wp.sigma with
         | None -> 
             let s = sigma_at wp in
             phi (L.move wenv.main s) { wp with sigma = Some s } 
         | Some s ->
             phi (L.move wenv.main s) wp) wp

  (* -------------------------------------------------------------------------- *)
  (* --- Compilation of Goals                                               --- *)
  (* -------------------------------------------------------------------------- *)

  let rec intros hs p = 
    match F.pred p with
    | Logic.Bind(Logic.Forall,_,p) -> intros hs p
    | Logic.Imply(hs2,p) -> intros (hs @ hs2) p
    | _ -> hs , p

  let introduction pred =
    let hs , goal = intros [] pred in
    let xs = List.fold_left
        (fun xs h -> Vars.union xs (F.varsp h))
        (F.varsp goal) hs
    in xs , hs , goal

  let add_vc target ?(warn=Warning.Set.empty) pred vcs =
    let xs , hs , goal = introduction pred in
    let hyps = Conditions.intros hs Conditions.empty in
    let vc = { empty_vc with goal=goal ; vars=xs ; hyps=hyps ; warn=warn } in
    Gmap.add target (Splitter.singleton vc) vcs

  (* ------------------------------------------------------------------------ *)
  (* --- Compilation of Effects                                           --- *)
  (* ------------------------------------------------------------------------ *)

  let cc_effect env pid (ainfo:WpPropId.assigns_desc) : effect option =
    let from = Clabels.c_label ainfo.WpPropId.a_label in
    let sigma = L.mem_frame from in
    let region = 
      L.assigns 
        (match ainfo.a_kind with
         | StmtAssigns -> L.move env sigma
         | LoopAssigns -> env)
        ainfo.a_assigns 
    in match region with
    | None -> None
    | Some r -> Some {
        e_pid = pid ;
        e_kind = ainfo.a_kind ;
        e_label = from ;
        e_valid = sigma ;
        e_region = r ;
        e_warn = Warning.Set.empty ;
      }

  let cc_posteffect e vcs = 
    match e.e_kind with
    | StmtAssigns -> vcs
    | LoopAssigns ->
        let vc = { empty_vc with vars = A.vars e.e_region } in
        Gmap.add (Gposteffect e.e_pid) (Splitter.singleton vc) vcs

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULES : adding axioms, hypotheses and goals                     --- *)
  (* -------------------------------------------------------------------------- *)

  let add_axiom _id _l = ()

  let add_hyp wenv (hpid,predicate) wp = in_wenv wenv wp
      (fun env wp ->
         let descr = Pretty_utils.to_string WpPropId.pretty hpid in
         let outcome = Warning.catch 
             ~severe:false ~effect:"Skip hypothesis"
             (L.pred ~positive:false env) predicate in
         let warn,hs = match outcome with
           | Warning.Result(warn,p) -> warn , [p]
           | Warning.Failed warn -> warn , [] 
         in
         let vcs = gmap (assume_vc ~hpid ~descr ~warn hs) wp.vcs in
         { wp with vcs = vcs })

  let add_goal wenv (gpid,predicate) wp = in_wenv wenv wp
      (fun env wp ->
         let outcome = Warning.catch
             ~severe:true ~effect:"Degenerated goal"
             (L.pred ~positive:true env) predicate in
         let warn,goal = match outcome with
           | Warning.Result(warn,goal) -> warn,goal
           | Warning.Failed warn -> warn,F.p_false
         in
         let vcs = add_vc (Gprop gpid) ~warn goal wp.vcs in
         { wp with vcs = vcs })

  let add_assigns wenv (gpid,ainfo) wp = in_wenv wenv wp 
      begin fun env wp ->
        let outcome = Warning.catch
            ~severe:true ~effect:"Degenerated goal"
            (cc_effect env gpid) ainfo
        in match outcome with
        | Warning.Result (_,None) -> wp
        | Warning.Result (warn,Some e) ->
            let e = { e with e_warn = warn } in
            let effects = Eset.add e wp.effects in
            let vcs = cc_posteffect e wp.vcs in
            { wp with effects = effects ; vcs = vcs }
        | Warning.Failed warn ->
            let vcs = add_vc (Gprop gpid) ~warn p_false wp.vcs in
            { wp with vcs = vcs }
      end

  let add_warnings wrns vcs = 
    gmap (fun vc -> { vc with warn = W.union wrns vc.warn }) vcs

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : use assigns clause                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let assigns_condition (region : A.region) (e:effect)
    : F.pred =
    p_all 
      (fun (obj1,r1) ->
         p_imply 
           (L.valid e.e_valid RD obj1 r1)
           (p_any 
              (fun (obj2,r2) -> L.included obj1 r1 obj2 r2)
              e.e_region))
      region

  exception COLLECTED

  let is_collected vcs p =
    try
      Gmap.iter 
        (fun target vcs ->
           let q = TARGET.prop_id target in
           if P.equal p q && Splitter.length vcs > 0 then raise COLLECTED
        ) vcs ; 
      false
    with COLLECTED -> true

  let check_nothing effects vcs =
    Eset.fold
      (fun e vcs ->
         if is_collected vcs e.e_pid then vcs else
           Gmap.add (Gprop e.e_pid) (Splitter.singleton empty_vc) vcs
      ) effects vcs

  let check_assigns sloc source ?(warn=Warning.Set.empty) region effects vcs =
    Eset.fold
      (fun e vcs ->
         let xs,hs,goal = introduction (assigns_condition region e) in
         let warn = Warning.Set.union warn e.e_warn in
         let setup vc = 
           { vc with 
             warn = warn ; 
             hyps = Conditions.intros hs vc.hyps ;
             goal = goal ; 
             vars = xs } 
         in
         let group =
           match e.e_kind with
           | StmtAssigns -> 
               Splitter.singleton (setup empty_vc)
           | LoopAssigns ->
               try Splitter.map setup (Gmap.find (Gposteffect e.e_pid) vcs)
               with Not_found -> 
                 Wp_parameters.fatal "Missing post-effect for %a" 
                   WpPropId.pretty e.e_pid
         in
         let target = match sloc with 
           | None -> Gprop e.e_pid 
           | Some stmt -> Geffect(e.e_pid,stmt,source)
         in
         Gmap.add target group vcs
      ) effects vcs

  let do_assigns ~descr ?stmt ~source ?hpid ?warn sequence region effects vcs =
    let vcs = check_assigns stmt source ?warn region effects vcs in
    let eqmem = A.assigned sequence region in
    gmap (assume_vc ~descr ?hpid ?stmt ?warn eqmem) vcs

  let do_assigns_everything ?stmt ?warn effects vcs =
    Eset.fold
      (fun e vcs ->
         let target = match stmt with 
           | None -> Gprop e.e_pid 
           | Some s -> Geffect(e.e_pid,s,FromCode)
         in
         add_vc target ?warn F.p_false vcs)
      effects vcs

  let cc_assigned env kind froms =
    let dummy = Sigma.create () in
    let r0 = A.domain (L.assigns_from (L.move env dummy) froms) in
    let s1 = L.sigma env in
    let s0 = Sigma.havoc s1 r0 in
    let sref = match kind with
      | StmtAssigns -> s0
      | LoopAssigns -> s1 
    in
    let assigned = L.assigns_from (L.move env sref) froms in
    let sequence = { pre=s0 ; post=s1 } in
    sequence , assigned

  let use_assigns wenv stmt hpid ainfo wp = in_wenv wenv wp 
      begin fun env wp ->
        match ainfo.a_assigns with
        
        | WritesAny ->
            let sigma = Sigma.havoc_any ~call:false (L.sigma env) in
            let vcs = do_assigns_everything ?stmt wp.effects wp.vcs in
            { sigma = Some sigma ; vcs=vcs ; effects = wp.effects }

        | Writes froms ->
            let kind = ainfo.WpPropId.a_kind in
            let outcome = 
              Warning.catch ~severe:true ~effect:"Assigns everything"
                (cc_assigned env kind) froms 
            in
            match outcome with
            | Warning.Result(warn,(sequence,assigned)) ->
                let vcs = 
                  do_assigns ~source:FromCode ~descr:"Assigns"
                    ?hpid ?stmt ~warn sequence assigned wp.effects wp.vcs in
                { sigma = Some sequence.pre ; vcs=vcs ; effects = wp.effects }
            | Warning.Failed warn ->
                let sigma = Sigma.havoc_any ~call:false (L.sigma env) in
                let vcs = do_assigns_everything ?stmt ~warn wp.effects wp.vcs in
                { sigma = Some sigma ; vcs=vcs ; effects = wp.effects }
      end

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : label                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let is_stopeffect l e = Clabels.equal l e.e_label
  let not_posteffect es target _vcs = match target with
    | Gposteffect p -> not (Eset.exists (fun e -> P.equal p e.e_pid) es)
    | _ -> true

  let label wenv labl wp = 
    match labl , wp.sigma with
    | Clabels.Here , _ | _ , None -> wp
    | _ -> in_wenv wenv wp
             (fun env wp ->
                let s_here = L.sigma env in
                let s_labl = L.mem_frame labl in
                let pa = Sigma.join s_here s_labl in
                let stop,effects = Eset.partition (is_stopeffect labl) wp.effects in
                let vcs = Gmap.filter (not_posteffect stop) wp.vcs in
                let vcs = gmap (passify_vc pa) vcs in 
                let vcs = check_nothing stop vcs in
                { sigma = Some s_here ; vcs=vcs ; effects=effects })

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : assignation                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let cc_lval env lv =
    let obj = Ctypes.object_of (Cil.typeOfLval lv) in
    let dummy = Sigma.create () in
    let l0 = C.lval dummy lv in
    let s2 = L.sigma env in
    let domain = M.domain obj l0 in
    let s1 = Sigma.havoc s2 domain in
    let loc = C.lval s1 lv in
    let seq = { pre=s1 ; post=s2 } in
    obj , domain , seq , loc

  let cc_stored seq loc obj expr =
    match expr.enode with
    | Lval lv -> M.copied seq obj loc (C.lval seq.pre lv)
    | _ -> M.stored seq obj loc (C.val_of_exp seq.pre expr)

  let assign wenv stmt lv expr wp = in_wenv wenv wp
      begin fun env wp ->
        let outcome = Warning.catch 
            ~severe:true ~effect:"Assigns everything (unknown l-value)"
            (cc_lval env) lv in
        match outcome with
        | Warning.Failed warn ->
            (* L-Value is unknown *)
            let sigma = Sigma.havoc_any ~call:false (L.sigma env) in
            let vcs = do_assigns_everything ~stmt ~warn wp.effects wp.vcs in
            { sigma = Some sigma ; vcs=vcs ; effects = wp.effects }
        | Warning.Result(l_warn,(obj,dom,seq,loc)) ->
            (* L-Value has been translated *)
            let region = [obj,[Sloc loc]] in
            let outcome = Warning.catch
                ~severe:false ~effect:"Havoc l-value (unknown r-value)"
                (cc_stored seq loc obj) expr in
            match outcome with
            | Warning.Failed r_warn ->
                (* R-Value is unknown *)
                let warn = Warning.Set.union l_warn r_warn in
                let vcs = do_assigns ~descr:"Assignment" ~source:FromCode
                    ~stmt ~warn seq region wp.effects wp.vcs in
                { sigma = Some seq.pre ; vcs=vcs ; effects = wp.effects }
            | Warning.Result(r_warn,stored) ->
                (* R-Value and effects has been translated *)
                let warn = Warning.Set.union l_warn r_warn in
                let ft = M.Heap.Set.fold_sorted 
                    (fun chunk ft -> M.Sigma.get seq.post chunk :: ft) dom [] 
                in
                let update vc = 
                  if List.exists (occurs_vc vc) ft
                  then assume_vc ~descr:"Assignment" ~stmt ~warn stored vc
                  else vc in
                let vcs = gmap update wp.vcs in
                let vcs = 
                  check_assigns (Some stmt) FromCode region wp.effects vcs in
                { sigma = Some seq.pre ; vcs=vcs ; effects = wp.effects }
      end

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : return statement                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let return wenv stmt result wp = 
    match result with
    | None -> wp
    | Some exp -> in_wenv wenv wp
                    (fun env wp ->
                       let xr = L.result () in
                       let tr = L.return () in
                       let sigma = L.sigma env in
                       let returned = p_equal (e_var xr) (C.return sigma tr exp) in
                       let vcs = gmap (assume_vc ~descr:"Return" ~stmt [returned]) wp.vcs in
                       { wp with vcs = vcs })

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : conditional                                              --- *)
  (* -------------------------------------------------------------------------- *)

  let condition ~descr ?stmt ?warn pa h vc = 
    passify_vc pa (assume_vc ?stmt ?warn ~descr h vc)

  let mark m = function 
    | None -> Splitter.empty
    | Some s -> Splitter.group m merge_vcs s

  let random () =
    let v = Lang.freshvar ~basename:"cond" Logic.Bool in
    F.p_bool (F.e_var v)

  let pp_opt pp fmt = function None -> Format.fprintf fmt "none" | Some e -> pp fmt e

  let test wenv stmt exp wp1 wp2 = L.in_frame wenv.frame
      (fun () ->
         let sigma,pa1,pa2 = sigma_union wp1.sigma wp2.sigma in
         let warn,cond = 
           match Warning.catch ~source:"Condition" 
                   ~severe:false ~effect:"Skip condition value" (C.cond sigma) exp 
           with
           | Warning.Result(warn,cond) -> warn,cond
           | Warning.Failed(warn) -> warn , random ()
         in
         let effects = Eset.union wp1.effects wp2.effects in
         let vcs = 
           if Wp_parameters.Split.get () then
             let cneg = p_not cond in
             let vcs1 = gmap (condition pa1 ~stmt ~warn ~descr:"Then" [cond]) wp1.vcs in
             let vcs2 = gmap (condition pa2 ~stmt ~warn ~descr:"Else" [cneg]) wp2.vcs in
             Gmap.merge
               (fun _g w1 w2 ->
                  let s1 = mark (Splitter.if_then stmt) w1 in
                  let s2 = mark (Splitter.if_else stmt) w2 in
                  Some (Splitter.union (merge_vc) s1 s2)
               ) vcs1 vcs2
           else
             let vcs1 = gmap (passify_vc pa1) wp1.vcs in
             let vcs2 = gmap (passify_vc pa2) wp2.vcs in
             gbranch
               ~left:(assume_vc ~descr:"Then" ~stmt [cond])
               ~right:(assume_vc ~descr:"Else" ~stmt [p_not cond])
               ~both:(branch_vc ~stmt cond)
               vcs1 vcs2 
         in
         { sigma = Some sigma ; vcs=vcs ; effects=effects }) ()

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : switch                                                   --- *)
  (* -------------------------------------------------------------------------- *)

  let rec cc_case_values ks vs sigma = function
    | [] -> ks , vs
    | e::es ->
        match Ctypes.get_int e with
        | Some k -> 
            cc_case_values (k::ks) (F.e_int64 k::vs) sigma es
        | None ->
            cc_case_values ks (C.val_of_exp sigma e::vs) sigma es

  let cc_group_case stmt warn descr tag pa cond vcs : vc Splitter.t Gmap.t =
    Gmap.map 
      (fun s ->
         Splitter.map 
           (condition ~descr ~warn ~stmt pa cond) 
           (Splitter.group tag merge_vcs s)
      ) vcs

  let cc_case stmt warn sigma v (es,wp) = 
    let ks,vs = cc_case_values [] [] sigma es in
    let pa = join_with sigma wp.sigma in
    let eq = p_any (p_equal v) vs in
    vs , cc_group_case stmt warn "Case" 
      (Splitter.switch_cases stmt ks) pa [eq] wp.vcs

  let cc_default stmt sigma neq default =
    let pa = join_with sigma default.sigma in
    cc_group_case stmt W.empty "Default" 
      (Splitter.switch_default stmt) pa neq default.vcs

  let switch wenv stmt exp cases default = L.in_frame wenv.frame
      (fun () ->
         let sigma = Sigma.create () in
         let warn,value = 
           match Warning.catch ~source:"Switch" 
                   ~severe:false ~effect:"Skip switched value"
                   (C.val_of_exp sigma) exp
           with
           | Warning.Result(warn,value) -> warn,value
           | Warning.Failed(warn) -> 
               let tau = Lang.tau_of_ctype (Cil.typeOf exp) in
               warn,e_var (Lang.freshvar tau)
         in
         let vcs_cases = List.map (cc_case stmt warn sigma value) cases in
         let neq = List.map (fun (vs,_) -> p_all (p_neq value) vs) vcs_cases in
         let vcs_default = cc_default stmt sigma neq default in
         let vcs = merge_all_vcs ( vcs_default :: List.map snd vcs_cases ) in
         let effects = List.fold_left
             (fun es (_,wp) -> Eset.union es wp.effects)
             default.effects cases in
         { sigma = Some sigma ; effects = effects ; vcs = vcs }) ()

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULES : initial values                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let init_value wenv lv typ init wp = in_wenv wenv wp
      (fun env wp ->
         let sigma = L.sigma env in
         let obj = Ctypes.object_of typ in
         let outcome = Warning.catch 
             ~severe:false ~effect:"Skip initializer"
             (fun () ->
                let l = C.lval sigma lv in
                match init with
                | Some e -> 
                    let v = M.load sigma obj l in
                    p_equal (C.val_of_exp sigma e) (C.cval v)
                | None -> C.is_zero sigma obj l 
             ) () in
         let warn,hyp = match outcome with
           | Warning.Failed warn -> warn , F.p_true
           | Warning.Result(warn , hyp) -> warn , hyp in
         let vcs = gmap (assume_vc ~descr:"Initializer" ~warn [hyp]) wp.vcs in
         { wp with vcs = vcs })

  let init_range wenv lv typ a b wp = in_wenv wenv wp
      (fun env wp ->
         let sigma = L.sigma env in
         let obj = Ctypes.object_of typ in
         let outcome = Warning.catch
             ~severe:false ~effect:"Skip initializer"
             (fun () ->
                let l = C.lval sigma lv in
                C.is_zero_range sigma l obj (e_int64 a) (e_int64 b)
             ) () in
         let warn,hyp = match outcome with
           | Warning.Failed warn -> warn , F.p_true
           | Warning.Result(warn , hyp) -> warn , hyp in
         let vcs = gmap (assume_vc ~descr:"Initializer" ~warn [hyp]) wp.vcs in
         { wp with vcs = vcs })

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : tag                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let loop_step wp = wp
  let loop_entry wp = wp

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : call dynamic                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let call_instances sigma gpid fct calls vcs =
    let outcome = Warning.catch
        ~severe:true ~effect:"Degenerated goal"
        (C.call sigma) fct in
    let warn,goal = match outcome with
      | Warning.Failed warn -> warn,F.p_false
      | Warning.Result(warn,floc) -> 
          warn , F.p_any (C.instance_of floc) calls
    in add_vc (Gprop gpid) ~warn goal vcs

  let call_contract stmt sigma fptr (kf,wp) : vc Splitter.t Gmap.t =
    let pa = join_with sigma wp.sigma in
    let tag = Splitter.call stmt kf in
    let descr = Printf.sprintf "Instance of '%s'" (Kernel_function.get_name kf) in
    let instance_vc pa fptr vc =
      passify_vc pa 
        begin match fptr with
          | None -> vc 
          | Some(warn,floc) -> 
              let hyp = C.instance_of floc kf in
              assume_vc ~stmt ~warn ~descr [hyp] vc
        end
    in
    Gmap.map
      (fun s ->
         Splitter.map 
           (instance_vc pa fptr)
           (Splitter.group tag merge_vcs s)
      ) wp.vcs

  let call_dynamic wenv stmt gpid fct calls = L.in_frame wenv.frame
      begin fun () ->
        let sigma = Sigma.create () in
        let outcome = Warning.catch
            ~severe:false ~effect:"Ignored function pointer value"
            (C.call sigma) fct in
        let fptr = match outcome with
          | Warning.Failed _warn -> None
          | Warning.Result(warn,floc) -> Some(warn,floc) in
        let vcs_calls = List.map (call_contract stmt sigma fptr) calls in
        let vcs = merge_all_vcs vcs_calls in
        let vcs = call_instances sigma gpid fct (List.map fst calls) vcs in
        let effects = List.fold_left 
            (fun es (_,wp) -> Eset.union es wp.effects) Eset.empty calls in
        { sigma = Some sigma ; vcs = vcs ; effects = effects }
      end ()

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : call precondition                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let call_goal_precond wenv _stmt kf es ~pre wp = in_wenv wenv wp
      (fun env wp ->
         let sigma = L.sigma env in
         let outcome = Warning.catch 
             ~severe:true ~effect:"Can not prove call preconditions"
             (List.map (C.exp sigma)) es in
         match outcome with
         | Warning.Failed warn ->
             let vcs = List.fold_left
                 (fun vcs (gid,_) -> add_vc (Gprop gid) ~warn p_false vcs)
                 wp.vcs pre
             in { wp with vcs = vcs }
         | Warning.Result(warn,vs) ->
             let call_e = L.call sigma in
             let call_f = L.call_pre kf vs sigma in
             let vcs = List.fold_left
                 (fun vcs (gid,p) ->
                    let outcome = Warning.catch
                        ~severe:true ~effect:"Can not prove call precondition"
                        (L.in_frame call_f (L.pred ~positive:false call_e)) p in
                    match outcome with
                    | Warning.Result(warn2,goal) ->
                        let warn = W.union warn warn2 in
                        add_vc (Gprop gid) ~warn goal vcs
                    | Warning.Failed warn2 ->
                        let warn = W.union warn warn2 in
                        add_vc (Gprop gid) ~warn p_false vcs
                 ) wp.vcs pre
             in { wp with vcs = vcs })

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : call postcondition                                       --- *)
  (* -------------------------------------------------------------------------- *)

  type callenv = {
    sigma_pre : sigma ;
    seq_post : sigma sequence ;
    seq_exit : sigma sequence ;
    seq_result : sigma sequence ;
    loc_result : (typ * Ctypes.c_object * loc) option ;
    frame_pre : L.frame ;
    frame_post : L.frame ;
    frame_exit : L.frame ;
  }

  (* --- Computing Call Memory States --- *)

  let cc_result_domain = function
    | Some lv ->
        let dummy = Sigma.create () in
        let tr = Cil.typeOfLval lv in
        let lr = C.lval dummy lv in
        Some (M.domain (Ctypes.object_of tr) lr)
    | None -> Some (M.Heap.Set.empty)

  let cc_call_domain env0 kf es = function
    | WritesAny -> None
    | Writes froms ->
        let dummy = Sigma.create () in
        let vs = List.map (C.exp dummy) es in
        let env = L.move env0 dummy in
        let frame = L.call_pre kf vs dummy in
        Some (A.domain (L.in_frame frame (L.assigns_from env) froms))

  let cc_havoc d s = match d with
    | None -> { pre = Sigma.havoc_any ~call:true s ; post = s }
    | Some domain -> { pre = Sigma.havoc s domain ; post = s }

  let cc_callenv env0 lvr kf es assigns wpost wexit =
    let dom_call = cc_call_domain env0 kf es assigns in
    let dom_vret = cc_result_domain lvr in
    (* Sequences to be considered *)
    let seq_result = cc_havoc dom_vret (sigma_at wpost) in
    let seq_post = cc_havoc dom_call seq_result.pre in
    let seq_exit = cc_havoc dom_call (sigma_at wexit) in
    (* Pre-State *)
    let sigma_pre, _, _ = Sigma.merge seq_post.pre seq_exit.pre in
    let formals = List.map (C.exp sigma_pre) es in
    let result = match lvr with
      | None -> None
      | Some lv -> 
          let tr = Cil.typeOfLval lv in
          let obj = Ctypes.object_of tr in
          let loc = C.lval sigma_pre lv in
          Some (tr,obj,loc)
    in
    {
      sigma_pre = sigma_pre ;
      seq_post = seq_post ;
      seq_exit = seq_exit ;
      seq_result = seq_result ;
      loc_result = result ;
      frame_pre = L.call_pre kf formals sigma_pre ;
      frame_post = L.call_post kf formals seq_post ;
      frame_exit = L.call_post kf formals seq_exit ;
    }

  type call_vcs = {
    vcs_post : vc Splitter.t Gmap.t ;
    vcs_exit : vc Splitter.t Gmap.t ;
  }

  let cc_call_effects stmt cenv env0 assigns wpost wexit =
    match assigns with
    | WritesAny -> 
        {
          vcs_post = do_assigns_everything ~stmt wpost.effects wpost.vcs ;
          vcs_exit = do_assigns_everything ~stmt wexit.effects wexit.vcs ;
        }
    | Writes froms ->
        let env = L.move env0 cenv.sigma_pre in
        let call_region = L.in_frame cenv.frame_pre (L.assigns_from env) froms in
        let vcs_post = do_assigns ~descr:"Call Effects" ~source:FromCall
            ~stmt cenv.seq_post call_region wpost.effects wpost.vcs in
        let vcs_exit = do_assigns ~descr:"Exit Effects" ~source:FromCall
            ~stmt cenv.seq_exit call_region wexit.effects wexit.vcs in
        let vcs_result = 
          match cenv.loc_result with
          | None -> vcs_post (* no result *)
          | Some(_,obj,loc) -> 
              let res_region = [obj,[Sloc loc]] in
              do_assigns ~descr:"Return Effects" ~source:FromReturn
                ~stmt cenv.seq_result res_region wpost.effects vcs_post 
        in
        { vcs_post = vcs_result ; vcs_exit = vcs_exit }

  (* --- Compiling Contracts --- *)

  let cc_contract_hyp frame env contract = 
    L.in_frame frame 
      (List.map (fun (_,p) -> L.pred ~positive:false env p)) contract

  (* --- Binding Result --- *)

  let cc_result call = match call.loc_result with
    | None -> []
    | Some(tr,obj,loc) ->
        (* [LC,VP] : the C left unspecified where to compute the lv *)
        (* [LC,BY] : lv computed before, like in Value Analysis *)
        let vr = M.load call.seq_result.post obj loc in
        let re = L.in_frame call.frame_post L.result () in
        let te = L.in_frame call.frame_post L.return () in
        [ C.equal_typ tr vr (C.cast tr te (Val (e_var re))) ]

  let cc_status f_caller f_callee =
    p_equal 
      (e_var (L.in_frame f_caller L.status ()))
      (e_var (L.in_frame f_callee L.status ()))

  (* --- Call Rule --- *)

  let call_proper wenv stmt lvr kf es ~pre ~post ~pexit ~assigns ~p_post ~p_exit () =
    let call = cc_callenv wenv.main lvr kf es assigns p_post p_exit in
    let env_pre = L.move wenv.main call.sigma_pre in
    let env_post = L.move wenv.main call.seq_post.post in
    let env_exit = L.move wenv.main call.seq_exit.post in

    (* Compiling specifications *)
    let hs_pre  = cc_contract_hyp call.frame_pre env_pre pre in
    let hs_post = cc_contract_hyp call.frame_post env_post post in
    let hs_exit = cc_contract_hyp call.frame_exit env_exit pexit in

    (* Binding result/status *)
    let hs_post = cc_result call @ hs_post in
    let hs_exit = cc_status wenv.frame call.frame_exit :: hs_exit in

    (* Checking effects (assigns and result) *)
    let ceff = cc_call_effects stmt call wenv.main assigns p_post p_exit in

    (* Applying specifications *)
    let fname = Kernel_function.get_name kf in
    let apply outcome pa hs vcs = 
      let descr = Printf.sprintf "%s '%s'" outcome fname in
      gmap (condition ~descr ~stmt pa hs) vcs in

    let pa_post = Sigma.join call.sigma_pre call.seq_post.pre in
    let pa_exit = Sigma.join call.sigma_pre call.seq_exit.pre in

    let cond_post = apply "Call" pa_post (hs_pre @ hs_post) ceff.vcs_post in
    let cond_exit = apply "Exit" pa_exit (hs_pre @ hs_exit) ceff.vcs_exit in

    (* Final vcs *)
    let vcs = gmerge cond_post cond_exit in
    let effects = Eset.union p_post.effects p_exit.effects in
    { sigma = Some call.sigma_pre ; effects=effects ; vcs=vcs }

  let call wenv stmt lvr kf es ~pre ~post ~pexit ~assigns ~p_post ~p_exit 
    = L.in_frame wenv.frame
      (fun () ->
         let outcome = Warning.catch 
             ~severe:true ~effect:"Call assigns everything"
             (call_proper wenv stmt lvr kf es 
                ~pre ~post ~pexit ~assigns ~p_post ~p_exit) () in
         match outcome with
         | Warning.Result(warn , wp) -> { wp with vcs = add_warnings warn wp.vcs }
         | Warning.Failed warn ->
             let v_post = do_assigns_everything ~stmt ~warn p_post.effects p_exit.vcs in
             let v_exit = do_assigns_everything ~stmt ~warn p_exit.effects p_exit.vcs in
             let effects = Eset.union p_post.effects p_exit.effects in
             let vcs = gmerge v_post v_exit in
             let sigma = Sigma.create () in
             { sigma = Some sigma ; vcs = vcs ; effects = effects }
      ) ()

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : scope                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let scope wenv xs sc wp = in_wenv wenv wp 
      (fun env wp ->
         let sigma,hs = M.scope (L.sigma env) sc xs in
         let descr = match sc with
           | Mcfg.SC_Global -> "Heap"
           | Mcfg.SC_Function_frame -> "Function Frame"
           | Mcfg.SC_Function_in -> "Function Entry"
           | Mcfg.SC_Function_out -> "Function Exit"
           | Mcfg.SC_Block_in -> "Block In"
           | Mcfg.SC_Block_out -> "Block Out"
         in
         let vcs = gmap (assume_vc ~descr hs) wp.vcs in
         { wp with sigma = Some sigma ; vcs = vcs })

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : close                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let close wenv wp = 
    let guards = L.guards wenv.frame in
    let vcs = gmap 
        (fun vc ->
           let gdom = List.filter (intersect_vc vc) guards in
           let hyps = Conditions.domain gdom vc.hyps in
           { vc with hyps = hyps ; vars = Vars.empty }
        ) wp.vcs
    in
    { wp with vcs = vcs }

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : froms                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let cc_from deps hs vc =
    let guards = Lang.get_hypotheses () in
    let hyps = Conditions.assume ~descr:"Bisimulation" (p_conj guards) vc.hyps in
    let p = F.p_hyps (Conditions.extract hyps) vc.goal in
    let alpha = Alpha.create () in
    let a_hs = List.map (Alpha.convertp alpha) hs in
    let a_p = Alpha.convertp alpha p in
    let p = p_hyps a_hs a_p in
    { vc with
      goal = p ; vars = F.varsp p ;
      hyps = Conditions.empty ;
      deps = D.union deps vc.deps ;
    }

  let build_prop_of_from wenv preconds wp = in_wenv wenv wp
      (fun env wp ->
         let sigma = L.mem_frame Pre in
         let env_pre = L.move env sigma in
         let hs = List.map 
             (fun (_,p) -> L.pred ~positive:false env_pre p) 
             preconds in
         let ds = List.fold_left
             (fun ds (pid,_) -> D.add (WpPropId.property_of_id pid) ds)
             D.empty preconds in
         let vcs = gmap (cc_from ds hs) wp.vcs in
         { sigma = Some sigma ; effects = Eset.empty ; vcs=vcs })

  (* -------------------------------------------------------------------------- *)
  (* --- WPO Builder                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let is_trivial vc = F.eqp vc.goal F.p_true

  let is_empty vc = 
    is_trivial vc && 
    D.is_empty vc.deps &&
    S.is_empty vc.path &&
    W.is_empty vc.warn

  let make_vcqs target tags vc =
    let vcq = {
      VC_Annot.effect = TARGET.source target ;
      VC_Annot.goal = GOAL.dummy ;
      VC_Annot.tags = tags ;
      VC_Annot.deps = vc.deps ;
      VC_Annot.path = vc.path ;
      VC_Annot.warn = W.elements vc.warn ;
    } in
    let hyps = Conditions.hypotheses vc.hyps in
    let goal g = { vcq with VC_Annot.goal = GOAL.make (hyps,g) } in
    match F.pred vc.goal with
    | Logic.And gs when Wp_parameters.Split.get () -> Bag.list (List.map goal gs)
    | _ -> Bag.elt (goal vc.goal)

  let make_trivial vc =
    {
      VC_Annot.effect = None ;
      VC_Annot.goal = GOAL.trivial ;
      VC_Annot.tags = [] ;
      VC_Annot.deps = vc.deps ;
      VC_Annot.path = vc.path ;
      VC_Annot.warn = W.elements vc.warn ;
    }

  let make_oblig index pid emitter vcq = 
    {
      po_model = Model.get_model () ;
      po_pid = pid ;
      po_gid = "" ; 
      po_name = "" ;
      po_idx = index ;
      po_updater = emitter ;
      po_formula = GoalAnnot vcq ;
    }

  (* -------------------------------------------------------------------------- *)
  (* --- WPO Grouper                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  module PMAP = FCMap.Make(P)

  type group = {
    mutable verifs : VC_Annot.t Bag.t ;
    mutable trivial : vc ;
  }

  let group_vc groups target tags vc =
    let pid = TARGET.prop_id target in
    let group = 
      try PMAP.find pid !groups 
      with Not_found ->
        let g = { verifs = Bag.empty ; trivial = empty_vc } in
        groups := PMAP.add pid g !groups ; g
    in
    if is_trivial vc
    then
      group.trivial <- merge_vc group.trivial vc
    else
      group.verifs <- Bag.concat group.verifs (make_vcqs target tags vc)

  let compile collection index (wp : t_prop) =
    let groups = ref PMAP.empty in
    Gmap.iter_sorted
      (fun target -> Splitter.iter (group_vc groups target)) 
      wp.vcs ;
    let model = Model.get_model () in
    let emitter = Model.get_emitter model in
    PMAP.iter 
      begin fun pid group ->
        let trivial_wpo =
          let vcq = make_trivial group.trivial in
          Bag.elt (make_oblig index pid emitter vcq)
        in
        let provers_wpo = 
          Bag.map (make_oblig index pid emitter) group.verifs
        in
        let mid = Model.get_id model in
        let group = 
          if is_empty group.trivial then 
            if Bag.is_empty provers_wpo 
            then trivial_wpo 
            else provers_wpo
          else
            Bag.concat trivial_wpo provers_wpo
        in
        WpAnnot.split
          begin fun pid wpo -> 
            let gid = Wpo.gid ~propid:pid ~model:mid in
            let name = Pretty_utils.to_string WpPropId.pretty pid in
            let wpo = 
              { wpo with po_pid = pid ; po_gid = gid ; po_name = name } 
            in
            Wpo.add wpo ;
            collection := Bag.append !collection wpo ;
          end
          pid group

      end !groups

  (* -------------------------------------------------------------------------- *)
  (* --- WPO Lemmas                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let compile_lemma l = ignore (L.lemma l)

  let prove_lemma collection l =
    if not l.lem_axiom then
      begin
        let id = WpPropId.mk_lemma_id l in
        let def = L.lemma l in
        let model = Model.get_model () in
        let vca = {
          Wpo.VC_Lemma.depends = l.lem_depends ;
          Wpo.VC_Lemma.lemma = def ;
        } in
        let index = match LogicUsage.section_of_lemma l.lem_name with
          | LogicUsage.Toplevel _ -> Wpo.Axiomatic None
          | LogicUsage.Axiomatic a -> Wpo.Axiomatic (Some a.ax_name) in
        let wpo = {
          Wpo.po_model = model ;
          Wpo.po_gid = Wpo.gid ~model:(Model.get_id model) ~propid:id ;
          Wpo.po_name = Printf.sprintf "Lemma '%s'" l.lem_name ;
          Wpo.po_idx = index ;
          Wpo.po_pid = id ;
          Wpo.po_updater = Model.get_emitter model ;
          Wpo.po_formula = Wpo.GoalLemma vca ;
        } in
        Wpo.add wpo ;
        collection := Bag.append !collection wpo ;
      end

end 

(* -------------------------------------------------------------------------- *)
(* --- Qed Checks                                                         --- *)
(* -------------------------------------------------------------------------- *)

let add_qed_check collection model ~qed ~raw ~goal =
  let id = Printf.sprintf "Qed-%s-%d-%d" 
      (Lang.F.head raw) (Lang.F.id qed) (Lang.F.id raw) in
  let pip = Property.ip_other id None Kglobal in
  let pid = WpPropId.mk_check pip in
  let vck = let open VC_Check in { raw ; qed ; goal } in
  let w = let open Wpo in {
      po_gid = id ;
      po_name = id ;
      po_idx = Axiomatic None ;
      po_model = model ;
      po_pid = pid ;
      po_updater = Model.get_emitter model ;
      po_formula = GoalCheck vck ;
    } in
  Wpo.add w ; collection := Bag.append !collection w

(* -------------------------------------------------------------------------- *)
(* --- WPO Computer                                                       --- *)
(* -------------------------------------------------------------------------- *)

module Computer(M : Memory.Model) =
struct

  module VCG = VC(M)
  module WP = Calculus.Cfg(VCG)

  class thecomputer (m:Model.t) =
    object
      val mutable lemmas : LogicUsage.logic_lemma Bag.t = Bag.empty
      val mutable annots : WpStrategy.strategy Bag.t = Bag.empty
      method lemma = true
      method add_lemma lemma = lemmas <- Bag.append lemmas lemma
      method add_strategy strategy = annots <- Bag.append annots strategy
      method compute : Wpo.t Bag.t =
        begin
          let collection = ref Bag.empty in
          Lang.F.release () ;
          Lang.F.do_checks := Wp_parameters.QedChecks.get () ;
          Model.on_model m 
            begin fun () ->
              LogicUsage.iter_lemmas VCG.compile_lemma ;
              Bag.iter (VCG.prove_lemma collection) lemmas ;
              Bag.iter
                (fun strategy ->
                   let cfg = WpStrategy.cfg_of_strategy strategy in
                   let kf = WpStrategy.get_kf strategy in
                   let names = WpAnnot.missing_rte kf in
                   let bhv = WpStrategy.get_bhv strategy in
                   let index = Wpo.Function( kf , bhv ) in
                   if names <> [] then
                     Wp_parameters.warning ~current:false ~once:true
                       "Missing RTE guards" ;
                   try
                     let (results,_) = WP.compute cfg strategy in
                     List.iter (VCG.compile collection index) results
                   with Warning.Error(source,reason) ->
                     Wp_parameters.failure ~current:false "From %s: %s" source reason
                ) annots ;
              if !Lang.F.do_checks then
                begin
                  Wp_parameters.feedback "Collecting checks" ;
                  Bag.iter (fun w -> ignore (Wpo.resolve w)) !collection ;
                  Lang.F.iter_checks (add_qed_check collection m) ;
                end
            end ;
          lemmas <- Bag.empty ;
          annots <- Bag.empty ;
          Lang.F.do_checks := false ;
          Lang.F.release () ;
          !collection
        end
    end

  let create m = (new thecomputer m :> Generator.computer)

end
