(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
    | Geffect of P.t * Stmt.t
    | Gposteffect of P.t

  module TARGET = 
  struct
    type t = target
    let hash = function
      | Gprop p | Gposteffect p -> P.hash p
      | Geffect(p,s) -> P.hash p * 37 + 41 * Stmt.hash s
    let compare g1 g2 =
      if g1 == g2 then 0 else
	match g1,g2 with
	  | Gprop p1 , Gprop p2 -> P.compare p1 p2
	  | Gprop _ , _ -> (-1)
	  | _ , Gprop _ -> 1
	  | Geffect(p1,s1) , Geffect(p2,s2) ->
	      let c = P.compare p1 p2 in
	      if c = 0 then Stmt.compare s1 s2 else c
	  | Geffect _ , _ -> (-1)
	  | _ , Geffect _ -> 1
	  | Gposteffect p1 , Gposteffect p2 -> P.compare p1 p2
    let prop_id = function Gprop p | Gposteffect p | Geffect(p,_) -> p
    let stmt = function Gprop _ | Gposteffect _ -> None | Geffect(_,s) -> Some s
    let pretty fmt = function
      | Gprop p -> WpPropId.pretty fmt p
      | Geffect(p,s) -> Format.fprintf fmt "%a at sid:%d" WpPropId.pretty p s.sid
      | Gposteffect p -> Format.fprintf fmt "%a post-effect" WpPropId.pretty p
  end

  type effect = {
    e_pid : P.t ; (* Assign Property *)
    e_kind : a_kind ; (* Requires post effects (in case of loop-assigns) *)
    e_label : c_label ; (* scope for collection *)
    e_valid : L.sigma ; (* sigma where locations are filtered for validity *)
    e_region : A.region ; (* expected from spec *)
    e_wrn : Warning.Set.t ; (* from translation *)
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
  module Eset = Set.Make(EFFECT)
  module Gset = G.Set
  module Gmap = G.Map

  type vc = {
    hyps : Hypotheses.t ;
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
      Hypotheses.pretty vc.hyps F.pp_pred vc.goal

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

(*  let pp_sigma_opt fmt = function
    | None -> Format.fprintf fmt "none"
    | Some s -> Sigma.pretty fmt s
  *)  
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
    hyps = Hypotheses.empty ;
    goal = p_true ;
    vars = V.empty ;
    warn = W.empty ; 
    deps = D.empty ;
    path = S.empty ;
  }

  let sigma_opt = function None -> Sigma.create () | Some s -> s
  let sigma_at w = sigma_opt w.sigma
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
    Vars.mem x vc.vars || Hypotheses.occurs x vc.hyps

  let _intersect_vc vc h = 
    Vars.intersect vc.vars (F.varsp h) || Hypotheses.intersect h vc.hyps

  let assume_vc ?hpid ?descr ?stmt ?wrn hs vc =
    if hs = [] && wrn = None then vc 
    else
      let path = match stmt with
	| None -> vc.path
	| Some s -> S.add s vc.path in
      let deps = match hpid with
	| None -> vc.deps
	| Some pid -> D.add (WpPropId.property_of_id pid) vc.deps in
      let wrns = match wrn with
	| None -> vc.warn
	| Some w -> Warning.Set.union w vc.warn in
      let hs = Hypotheses.assume vc.hyps hs in
      { 
	hyps = Hypotheses.mark ?descr ?stmt hs ;
	goal = vc.goal ;
	vars = vc.vars ;
	warn = wrns ;
	deps = deps ; 
	path = path ;
      }

  let passify_vc pa vc =
    let hs = Passive.conditions pa (occurs_vc vc) in
    assume_vc hs vc

  (* -------------------------------------------------------------------------- *)
  (* --- Branching                                                          --- *)
  (* -------------------------------------------------------------------------- *)

  let branch_vc ~stmt cond vc1 vc2 =
    let hyps , goal =
      if F.eqp vc1.goal vc2.goal then
	Hypotheses.m_if cond vc1.hyps vc2.hyps , vc1.goal
      else
	let h1,hyps,h2 = Hypotheses.factorize vc1.hyps vc2.hyps in
	hyps , p_and 
	  (p_hyps (cond::h1) vc1.goal) 
	  (p_hyps (p_not cond::h2) vc2.goal) 
    in
    {
      hyps = Hypotheses.mark ~descr:"Conditional" ~stmt hyps ;
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
	Hypotheses.m_or vc1.hyps vc2.hyps , vc1.goal
      else
	let h1,hyps,h2 = Hypotheses.factorize vc1.hyps vc2.hyps in
	hyps , p_and (p_hyps h1 vc1.goal) (p_hyps h2 vc2.goal)
    in
    {
      hyps = hyps ;
      goal = goal ;
      vars = V.union vc1.vars vc2.vars ;
      deps = D.union vc1.deps vc2.deps ;
      warn = W.union vc1.warn vc2.warn ;
      path = S.union vc1.path vc2.path ;
    }

  let merge_vcs vcs = 
    let hyps = 
      match Hypotheses.m_either (List.map (fun vc -> vc.hyps) vcs) with
	| None -> Hypotheses.empty
	| Some hs -> hs in
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
    let env = L.in_frame frame L.env lvars in
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

  let add_vc target ?(wrn=Warning.Set.empty) pred vcs =
    let hs , p = intros [] pred in
    let hyps =
      if hs <> [] then
	Hypotheses.mark ~descr:"Goal introduction" 
	  (Hypotheses.assume Hypotheses.empty hs)
      else
	Hypotheses.empty
    in
    let vc = 
      { empty_vc with 
	  warn = wrn ; 
	  hyps = hyps ; 
	  goal = p ; 
	  vars = F.varsp p ;
      } in
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
	  e_wrn = Warning.Set.empty ;
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
       let wrn,hs = match outcome with
	 | Warning.Result(wrn,p) -> wrn , [p]
	 | Warning.Failed wrn -> wrn , [] 
       in
       let vcs = gmap (assume_vc ~hpid ~descr ~wrn hs) wp.vcs in
       { wp with vcs = vcs })
    
  let add_goal wenv (gpid,predicate) wp = in_wenv wenv wp
    (fun env wp ->
       let outcome = Warning.catch
	 ~severe:true ~effect:"Degenerated goal"
	 (L.pred ~positive:true env) predicate in
       let wrn,goal = match outcome with
	 | Warning.Result(wrn,goal) -> wrn,goal
	 | Warning.Failed wrn -> wrn,F.p_false
       in
       let vcs = add_vc (Gprop gpid) ~wrn goal wp.vcs in
       { wp with vcs = vcs })
      
  let add_assigns wenv (gpid,ainfo) wp = in_wenv wenv wp 
    begin fun env wp ->
      let outcome = Warning.catch
	~severe:true ~effect:"Degenerated goal"
	(cc_effect env gpid) ainfo
      in match outcome with
	| Warning.Result (_,None) -> wp
	| Warning.Result (wrn,Some e) ->
	    let e = { e with e_wrn = wrn } in
	    let effects = Eset.add e wp.effects in
	    let vcs = cc_posteffect e wp.vcs in
	    { wp with effects = effects ; vcs = vcs }
	| Warning.Failed wrn ->
	    let vcs = add_vc (Gprop gpid) ~wrn p_false wp.vcs in
	    { wp with vcs = vcs }
    end

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : use assigns clause                                       --- *)
  (* -------------------------------------------------------------------------- *)

  let assigns_condition (region : A.region) (e:effect)
      : F.pred =
    p_all 
      (fun (obj1,r1) ->
	 p_imply 
	   (L.valid e.e_valid obj1 r1)
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

  let check_assigns ?stmt ?(wrn=Warning.Set.empty) region effects vcs =
    Eset.fold
      (fun e vcs ->
	 let goal = assigns_condition region e in
	 let vars = F.varsp goal in
	 let warn = Warning.Set.union wrn e.e_wrn in
	 let setup vc = { vc with warn = warn ; goal = goal ; vars = vars } in
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
	 let target = match stmt with 
	   | None -> Gprop e.e_pid 
	   | Some s -> Geffect(e.e_pid,s)
	 in
	 Gmap.add target group vcs
      ) effects vcs
      
  let do_assigns ?hpid ?descr ?stmt ?wrn 
      sequence region effects vcs =
    let vcs = check_assigns ?stmt ?wrn region effects vcs in
    let eqmem = A.assigned sequence region in
    gmap (assume_vc ?hpid ?descr ?stmt ?wrn eqmem) vcs

  let do_assigns_everything ?stmt ?wrn effects vcs =
    Eset.fold
      (fun e vcs ->
	 let target = match stmt with 
	   | None -> Gprop e.e_pid 
	   | Some s -> Geffect(e.e_pid,s)
	 in
	 add_vc target ?wrn F.p_false vcs)
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
	    let sigma = Sigma.havoc_any (L.sigma env) in
	    let vcs = do_assigns_everything ?stmt wp.effects wp.vcs in
	    { sigma = Some sigma ; vcs=vcs ; effects = wp.effects }

	| Writes froms ->
	    let kind = ainfo.WpPropId.a_kind in
	    let outcome = 
	      Warning.catch ~severe:true ~effect:"Assigns everything"
		(cc_assigned env kind) froms 
	    in
	    match outcome with
	      | Warning.Result(wrn,(sequence,assigned)) ->
		  let vcs = 
		    do_assigns ?hpid ?stmt ~wrn sequence assigned 
		      wp.effects wp.vcs in
		  { sigma = Some sequence.pre ; vcs=vcs ; effects = wp.effects }
	      | Warning.Failed wrn ->
		  let sigma = Sigma.havoc_any (L.sigma env) in
		  let vcs = do_assigns_everything ?stmt ~wrn wp.effects wp.vcs in
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
	| Warning.Failed wrn ->
	    (* L-Value is unknown *)
	    let sigma = Sigma.havoc_any (L.sigma env) in
	    let vcs = do_assigns_everything ~stmt ~wrn wp.effects wp.vcs in
	    { sigma = Some sigma ; vcs=vcs ; effects = wp.effects }
	| Warning.Result(l_wrn,(obj,dom,seq,loc)) ->
	    (* L-Value has been translated *)
	    let region = [obj,[Sloc loc]] in
	    let outcome = Warning.catch
	      ~severe:false ~effect:"Havoc l-value (unknown r-value)"
	      (cc_stored seq loc obj) expr in
	    match outcome with
	      | Warning.Failed r_wrn ->
		  (* R-Value is unknown *)
		  let wrn = Warning.Set.union l_wrn r_wrn in
		  let vcs = do_assigns ~stmt ~wrn seq region wp.effects wp.vcs in
		  { sigma = Some seq.pre ; vcs=vcs ; effects = wp.effects }
	      | Warning.Result(r_wrn,stored) ->
		  (* R-Value and effects has been translated *)
		  let wrn = Warning.Set.union l_wrn r_wrn in
		  let ft = M.Heap.Set.fold 
		    (fun chunk ft -> M.Sigma.get seq.post chunk :: ft) dom [] 
		  in
		  let update vc = 
		    if List.exists (occurs_vc vc) ft
		    then assume_vc ~descr:"Assignment" ~stmt ~wrn stored vc
		    else vc in
		  let vcs = gmap update wp.vcs in
		  let vcs = check_assigns ~stmt region wp.effects vcs in
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

  let condition ?stmt ?descr pa h vc = 
    passify_vc pa (assume_vc ?stmt ?descr h vc)

  let simplified phi vc =
    let vc = phi vc in
    if Wp_parameters.Qed.get () then 
      let hyps = Hypotheses.simplify_hyps vc.hyps in
      { vc with hyps = hyps }
    else
      vc

  let mark m = function 
    | None -> Splitter.empty
    | Some s -> Splitter.group m merge_vcs s

  let test wenv stmt exp wp1 wp2 = L.in_frame wenv.frame
    (fun () ->
       let sigma,pa1,pa2 = sigma_union wp1.sigma wp2.sigma in
       let cond = C.cond sigma exp in
       let effects = Eset.union wp1.effects wp2.effects in
       let vcs = 
	 if Wp_parameters.Split.get () then
	   let cneg = p_not cond in
	   let vcs1 = gmap 
	     (simplified (condition pa1 ~stmt ~descr:"Then" [cond])) wp1.vcs in
	   let vcs2 = gmap 
	     (simplified (condition pa2 ~stmt ~descr:"Else" [cneg])) wp2.vcs in
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

  let cc_group_case stmt descr tag pa cond vcs : vc Splitter.t Gmap.t =
    Gmap.map 
      (fun s ->
	 Splitter.map 
	   (condition ~descr ~stmt pa cond) 
	   (Splitter.group tag merge_vcs s)
      ) vcs

  let cc_case stmt sigma v (es,wp) = 
    let ks,vs = cc_case_values [] [] sigma es in
    let pa = join_with sigma wp.sigma in
    let eq = p_any (p_equal v) vs in
    vs , cc_group_case stmt "Case" 
      (Splitter.switch_cases stmt ks) pa [eq] wp.vcs

  let cc_default stmt sigma neq default =
    let pa = join_with sigma default.sigma in
    cc_group_case stmt "Default" 
      (Splitter.switch_default stmt) pa neq default.vcs

  let get_vcs goal vcs = try Gmap.find goal vcs with Not_found -> Splitter.empty

  let switch wenv stmt exp cases default = L.in_frame wenv.frame
    (fun () ->
       let sigma = Sigma.create () in
       let value = C.val_of_exp sigma exp in
       let vcs_cases = List.map (cc_case stmt sigma value) cases in
       let neq = List.map (fun (vs,_) -> p_all (p_neq value) vs) vcs_cases in
       let vcs_default = cc_default stmt sigma neq default in
       let targets = List.fold_left
	 (fun ds (_,vcs) -> Gset.union ds (Gmap.domain vcs))
	 (Gmap.domain vcs_default) vcs_cases in
       let vcs = Gset.mapping
	 (fun goal ->
	    Splitter.merge_all merge_vcs
	      (get_vcs goal vcs_default :: 
		 List.map (fun (_,vcs) -> get_vcs goal vcs) vcs_cases))
	 targets in
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
       let l = C.lval sigma lv in
       let h = match init with
	 | Some e -> 
	     let v = M.load sigma obj l in
	     p_equal (C.val_of_exp sigma e) (C.cval v)
	 | None -> C.is_zero sigma obj l in
       let vcs = gmap (assume_vc ~descr:"Initializer" [h]) wp.vcs in
       { wp with vcs = vcs })

  let init_range wenv lv typ a b wp = in_wenv wenv wp
    (fun env wp ->
       let sigma = L.sigma env in
       let obj = Ctypes.object_of typ in
       let l = C.lval sigma lv in
       let h = C.is_zero_range sigma l obj (e_int64 a) (e_int64 b) in
       let vcs = gmap (assume_vc ~descr:"Initializer" [h]) wp.vcs in
       { wp with vcs = vcs })

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : tag                                                      --- *)
  (* -------------------------------------------------------------------------- *)

  let loop_step wp = wp
  let loop_entry wp = wp

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : call precondition                                        --- *)
  (* -------------------------------------------------------------------------- *)
      
  let call_goal_precond wenv _stmt kf es ~pre wp = in_wenv wenv wp
    (fun env wp ->
       let sigma = L.sigma env in
       let vs = List.map (C.exp sigma) es in
       let call_e = L.call sigma in
       let call_f = L.call_pre kf vs sigma in
       let vcs = List.fold_left
	 (fun vcs (gid,p) ->
	    let precond = L.pred ~positive:false call_e in
	    add_vc (Gprop gid) (L.in_frame call_f precond p) vcs
	 ) wp.vcs pre 
       in 
       { wp with vcs = vcs })

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : call postcondition                                       --- *)
  (* -------------------------------------------------------------------------- *)

  type callenv = {
    pa_post : Passive.t ;
    pa_exit : Passive.t ;
    sigma_pre : sigma ;
    sigma_post : sigma ;
    sigma_exit : sigma ;
    frame_pre : L.frame ;
    frame_post : L.frame ;
    frame_exit : L.frame ;
    wpost : vc Splitter.t Gmap.t ;
    wexit : vc Splitter.t Gmap.t ;
  }

  (* --- Computing Call Memory States --- *)

  let cc_call_domain env0 kf es froms =
    let dummy = Sigma.create () in
    let vs = List.map (C.exp dummy) es in
    let env = L.move env0 dummy in
    let frame = L.call_pre kf vs dummy in
    A.domain (L.in_frame frame (L.assigns_from env) froms)

  let cc_callenv stmt env0 kf es assigns wpost wexit =
    let sigma_post = sigma_at wpost in
    let sigma_exit = sigma_at wexit in
    let transfert = match assigns with
      | WritesAny -> Sigma.havoc_any
      | Writes froms -> 
	  let domain = cc_call_domain env0 kf es froms in
	  fun s -> Sigma.havoc s domain in
    let spost = { pre = transfert sigma_post ; post = sigma_post } in
    let sexit = { pre = transfert sigma_exit ; post = sigma_exit } in
    let sigma_pre, pa_post, pa_exit = Sigma.merge spost.pre sexit.pre in
    let formals = List.map (C.exp sigma_pre) es in
    let frame_pre = L.call_pre kf formals sigma_pre in
    let frame_post = L.call_post kf formals spost in
    let frame_exit = L.call_post kf formals sexit in
    let callenv = {
      pa_post = pa_post ;
      pa_exit = pa_exit ;
      sigma_pre = sigma_pre ;
      sigma_post = sigma_post ;
      sigma_exit = sigma_exit ;
      frame_pre = frame_pre ;
      frame_post = frame_post ;
      frame_exit = frame_exit ;
      wpost = wpost.vcs ;
      wexit = wexit.vcs ;
    } in
    match assigns with
      | WritesAny ->
	  { callenv with
	      wpost = do_assigns_everything ~stmt wpost.effects wpost.vcs ;
	      wexit = do_assigns_everything ~stmt wexit.effects wexit.vcs ;
	  }
      | Writes froms ->
	  let env_pre = L.move env0 sigma_pre in
	  let region = L.in_frame frame_pre (L.assigns_from env_pre) froms in
	  { callenv with
	      wpost = do_assigns ~stmt spost region wpost.effects wpost.vcs ;
	      wexit = do_assigns ~stmt sexit region wexit.effects wexit.vcs ;
    	  }
	    
  (* --- Compiling Contracts --- *)
	    
  let cc_contract_hyp frame env contract = 
    L.in_frame frame 
      (List.map (fun (_,p) -> L.pred ~positive:false env p)) contract

  (* --- Binding Result --- *)

  let cc_result frame seq = function
    | None -> []
    | Some lv ->
	(* [LC,VP] : the C left unspecified where to compute the lv *)
	(* [LC,BY] : lv computed before, like in Value Analysis *)
	let tr = Cil.typeOfLval lv in
	let lr = C.lval seq.pre lv in
	let vr = M.load seq.post (Ctypes.object_of tr) lr in
	let re = L.in_frame frame L.result () in
	let te = L.in_frame frame L.return () in
	[ C.equal_typ tr vr (C.cast tr te (Val (e_var re))) ]

  let cc_status f_caller f_callee =
    p_equal 
      (e_var (L.in_frame f_caller L.status ()))
      (e_var (L.in_frame f_callee L.status ()))

  (* --- Call Rule --- *)

  let call wenv stmt lvr kf es ~pre ~post ~pexit ~assigns ~p_post ~p_exit 
      = L.in_frame wenv.frame
    (fun () ->
       let call = cc_callenv stmt wenv.main kf es assigns p_post p_exit in
       let env_pre = L.move wenv.main call.sigma_pre in
       let env_post = L.move wenv.main call.sigma_post in
       let env_exit = L.move wenv.main call.sigma_exit in
       let hs_pre  = cc_contract_hyp call.frame_pre env_pre pre in
       let hs_post = cc_contract_hyp call.frame_post env_post post in
       let seq = { pre = call.sigma_pre ; post = call.sigma_post } in
       let hs_post = cc_result call.frame_post seq lvr @ hs_post in
       let hs_exit = cc_contract_hyp call.frame_exit env_exit pexit in
       let hs_exit = cc_status wenv.frame call.frame_exit :: hs_exit in
       let fname = Kernel_function.get_name kf in
       let apply outcome pa hs vcs = 
	 let descr = Printf.sprintf "%s '%s'" outcome fname in
	 gmap (condition ~descr ~stmt pa hs) vcs in
       let cond_post = apply "Call" call.pa_post (hs_pre @ hs_post) call.wpost in
       let cond_exit = apply "Exit" call.pa_exit (hs_pre @ hs_exit) call.wexit in
       let vcs = gmerge cond_post cond_exit in
       let effects = Eset.union p_post.effects p_exit.effects in
       { sigma = Some call.sigma_pre ; effects=effects ; vcs=vcs }
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

  let _size vcs = Gmap.fold (fun _ s n -> Splitter.length s + n) vcs 0

  let close wenv wp = 
    let guards = L.guards wenv.frame in
    let vcs = gmap 
      (fun vc ->
	 let hyps = Hypotheses.add_guards vc.hyps vc.goal guards in
	 { vc with hyps = hyps ; vars = Vars.empty }
      ) wp.vcs
    in
    { wp with vcs = vcs }

  (* -------------------------------------------------------------------------- *)
  (* --- WP RULE : froms                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let cc_from deps hs vc =
    let guards = Lang.get_hypotheses () in
    let hyps = Hypotheses.add_guards vc.hyps vc.goal guards in
    let p = F.p_hyps (Hypotheses.conditions hyps) vc.goal in
    let alpha = Alpha.create () in
    let a_hs = List.map (Alpha.convertp alpha) hs in
    let a_p = Alpha.convertp alpha p in
    let p = p_hyps a_hs a_p in
    { vc with
	goal = p ; vars = F.varsp p ;
	hyps = Hypotheses.empty ;
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

  open Wpo

  let is_trivial vc = F.eqp vc.goal F.p_true

  let is_empty vc = 
    is_trivial vc && 
    D.is_empty vc.deps &&
    S.is_empty vc.path &&
    W.is_empty vc.warn

  let make_vcqs target tags vc =
    let vcq = {
      VC_Annot.model = Model.get_model () ;
      VC_Annot.effect = TARGET.stmt target ;
      VC_Annot.goal = GOAL.make Hypotheses.empty p_false ;
      VC_Annot.tags = tags ;
      VC_Annot.deps = vc.deps ;
      VC_Annot.path = vc.path ;
      VC_Annot.warn = W.elements vc.warn ;
    } in
    match F.pred vc.goal with
      | Logic.And gs when Wp_parameters.Split.get () -> 
	  Bag.map 
	    (fun g -> { vcq with VC_Annot.goal = GOAL.make vc.hyps g }) 
	    (Bag.list gs)
      | _ -> 
	  Bag.elt { vcq with VC_Annot.goal = GOAL.make vc.hyps vc.goal }

  let make_trivial model vc =
    {
      VC_Annot.model = model ;
      VC_Annot.effect = None ;
      VC_Annot.goal = GOAL.make Hypotheses.empty p_true ;
      VC_Annot.tags = [] ;
      VC_Annot.deps = vc.deps ;
      VC_Annot.path = vc.path ;
      VC_Annot.warn = W.elements vc.warn ;
    }

  let make_oblig index pid emitter vcq = 
    {
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

  module PMAP = Map.Make(P)
  
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
	  let vcq = make_trivial model group.trivial in
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
	  Wpo.VC_Lemma.model = model ;
	  Wpo.VC_Lemma.depends = l.lem_depends ;
	  Wpo.VC_Lemma.lemma = def ;
	} in
	let wpo = {
	  Wpo.po_gid = Wpo.gid ~model:(Model.get_id model) ~propid:id ;
	  Wpo.po_name = Printf.sprintf "Lemma '%s'" l.lem_name ;
	  Wpo.po_idx = Wpo.Lemma l.lem_name ;
	  Wpo.po_pid = id ;
	  Wpo.po_updater = Model.get_emitter model ;
	  Wpo.po_formula = Wpo.GoalLemma vca ;
	} in
	Wpo.add wpo ;
	collection := Bag.append !collection wpo ;
      end
	
end 

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
	Model.on_model m 
	  (fun () ->
	     LogicUsage.iter_lemmas VCG.compile_lemma ;
	     Bag.iter (VCG.prove_lemma collection) lemmas ;
	     Bag.iter
	       (fun strategy ->
		  let cfg = WpStrategy.cfg_of_strategy strategy in
		  let kf = WpStrategy.get_kf strategy in
		  let bhv = WpStrategy.get_bhv strategy in
		  let index = Wpo.Function( kf , bhv ) in
		  try
		    let (results,_) = WP.compute cfg strategy in
		    List.iter (VCG.compile collection index) results
		  with Warning.Error(source,reason) ->
		    Wp_parameters.failure ~current:false "From %s: %s" source reason
	       ) annots) ;
	lemmas <- Bag.empty ;
	annots <- Bag.empty ;
	!collection
      end
  end

  let create m = (new thecomputer m :> Generator.computer)

end
