(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

let dkey = "annot" (* debugging key *)
let debug fmt = Wp_parameters.debug ~dkey fmt

(* This file groups functions that extract some annotations
 * and associates them with CFG edges. *)

open Cil_types
open Cil_datatype

(* -------------------------------------------------------------------------- *)
(* --- Global Status                                                      --- *)
(* -------------------------------------------------------------------------- *)

let rte_find rte_st kf = let (_,_,status,_) = !rte_st () in status kf
let rte_precond_status    = rte_find Db.RteGen.get_precond_status
let rte_signedOv_status   = rte_find Db.RteGen.get_signedOv_status
let rte_divMod_status     = rte_find Db.RteGen.get_divMod_status
let rte_downCast_status   = rte_find Db.RteGen.get_downCast_status
let rte_memAccess_status  = rte_find Db.RteGen.get_memAccess_status
let rte_unsignedOv_status = rte_find Db.RteGen.get_unsignedOv_status

let rte_wp =
  [
    "valid pointer dereferencing" , rte_memAccess_status , "-rte-mem" ;
    "division by zero" , rte_divMod_status , "-rte-div" ;
    "signed overflow" , rte_signedOv_status , "-rte-signed" ;
    "unsigned overflow" , rte_unsignedOv_status , "-rte-unsigned-ov" ;
  ]

let rte_generated kf =
  List.for_all (fun (_,rte,_) -> rte kf) rte_wp

let missing_rte kf =
  List.map
    (fun (name,_,_) -> name)
    (List.filter (fun (_,rte,_) -> not (rte kf)) rte_wp)

let compute_rte_for kf =
  begin
    Dynamic.Parameter.Bool.set "-rte" true ;
    List.iter (fun (_,_,opt) -> Dynamic.Parameter.Bool.set opt true) rte_wp ;
    !Db.RteGen.annotate_kf kf ;
  end

let ip_complete f = Property.ip_complete_of_spec f Kglobal f.spec

let ip_disjoint f = Property.ip_disjoint_of_spec f Kglobal f.spec

(*
(* -------------------------------------------------------------------------- *)
(* --- Contract for functions                                             --- *)
(* -------------------------------------------------------------------------- *)

let wp_contract = 
  Emitter.create "WP Function" ~correctness:[] ~tuning:[]

let wp_external = 
  Emitter.create "WP External Function" ~correctness:[] ~tuning:[]

let ip_contract f =
  let ip_rte =
    if not (rte_generated f) then
      Wp_parameters.warning ~current:false ~once:true
        "Missing RTE guards" ;
    [ (*TODO: these dependencies should be put on the emitter at Wpo level *) ]
  in
  ip_complete f @ ip_disjoint f @ ip_rte

let ip_external f =
  ip_complete f @ ip_disjoint f

module FunctionContracts = Wprop.Indexed2(Kernel_function)(Datatype.String)
  (struct
     let name = "WP Function Contracts"
     type key = kernel_function * string
     let size = 81
     let kind = `Correctness
     let dependencies = [ Kernel_function.self ]
     let property (kf,model) =
       let name = Printf.sprintf
         "Function '%s' is consistent with %s"
         (Kernel_function.get_name kf) model
       in
       let ip = Property.ip_other name (Some kf) Kglobal in
       match kf.fundec with
         | Definition _ -> 
	     Wprop.Proxy( ip , wp_contract , ip_contract kf )
	 | Declaration _ -> 
	     Wprop.Proxy( ip , wp_external , ip_external kf )
   end)
*)

(* -------------------------------------------------------------------------- *)
(* --- Local Preconditions for functions                                  --- *)
(* -------------------------------------------------------------------------- *)

let wp_preconditions = 
  Emitter.create "WP Call Preconditions" ~correctness:[] ~tuning:[]

module PreCondAt = Wprop.Indexed2(Property)(Cil_datatype.Stmt)
  (struct
     let name = "WP Called Preconditions"
     type key = Property.t * stmt
     let size = 81
     let kind = `Correctness
     let dependencies = [ Kernel_function.self ]
     let property (pid,stmt) =
       let kf = Kernel_function.find_englobing_kf stmt in
       let name = Pretty_utils.to_string (Description.pp_localized ~kf:`Always ~ki:false) pid in
       Wprop.Later (Property.ip_other name (Some kf) (Kstmt stmt))
   end)

module PreCondProxyGenerated =
  State_builder.Hashtbl(Property.Hashtbl)(Datatype.Unit)
    (struct
       let name = "WP Call Preconditions Generated"
       let dependencies = [Ast.self]
       let kind = `Internal
       let size = 97
     end)

let pre_cond_id = function
  | Property.IPCodeAnnot(_,_,p) -> p.annot_id
  | Property.IPPredicate(_,_,_,p) -> p.ip_id
  | property -> 
    Wp_parameters.fatal "No precondition id for @[%a@]" Property.pretty property

let setup_precondition_proxy called_kf precondition =
  if not (PreCondProxyGenerated.mem precondition) then
    begin
      let called_preconditions =
        List.map
          (fun (_,stmt) -> PreCondAt.property precondition stmt)
          (Kernel_function.find_syntactic_callsites called_kf)
      in
      Property_status.emit
	wp_preconditions ~hyps:called_preconditions precondition 
	Property_status.True ;
      PreCondProxyGenerated.add precondition ()
    end

(* Properties for kf-preconditions at call-site stmt, IF CREATED only *)
let lookup_called_preconditions_at kf stmt =
  let spec = Kernel_function.get_spec kf in
  List.fold_left
    (fun properties bhv ->
       List.fold_left
         (fun properties precond ->
            let pid_spec = Property.ip_of_requires kf Kglobal bhv precond in
	    if PreCondAt.mem pid_spec stmt then
              let pid_call = PreCondAt.property pid_spec stmt in
              pid_call :: properties
	    else 
	      properties
         ) properties bhv.b_requires
    ) [] spec.spec_behavior

(* Properties for kf-preconditions at call-site stmt *)
let get_called_preconditions_at kf stmt =
  let spec = Kernel_function.get_spec kf in
  List.fold_left
    (fun properties bhv ->
       List.fold_left
         (fun properties precond ->
            let pid_spec = Property.ip_of_requires kf Kglobal bhv precond in
            let pid_call = PreCondAt.property pid_spec stmt in
            pid_call :: properties
         ) properties bhv.b_requires
    ) [] spec.spec_behavior

(* Properties for kf-conditions of termination-kind 'tkind' *)
let get_called_postconds (tkind:termination_kind) kf =
  let spec = Kernel_function.get_spec kf in
  List.fold_left
    (fun properties bhv ->
       List.fold_left
	 (fun properties postcond ->
	    if tkind = fst postcond then
	      let pid_spec = Property.ip_of_ensures kf Kglobal bhv postcond in
	      pid_spec :: properties
	    else properties
	 ) properties bhv.b_post_cond
    ) [] spec.spec_behavior

let get_called_post_conditions = get_called_postconds Cil_types.Normal
let get_called_exit_conditions = get_called_postconds Cil_types.Exits

(** Properties for assigns of kf *)
let get_called_assigns kf =
  let spec = Kernel_function.get_spec kf in
  List.fold_left
    (fun properties bhv ->
       if Cil.is_default_behavior bhv then
	 match Property.ip_assigns_of_behavior kf Kglobal bhv with
	   | None -> properties
	   | Some ip -> ip :: properties
       else properties
    ) [] spec.spec_behavior
  
(* -------------------------------------------------------------------------- *)
(* --- Status of Unreachable Annotations                                  --- *)
(* -------------------------------------------------------------------------- *)

let wp_unreachable = 
  Emitter.create
    "Unreachable Annotations"
    ~correctness:[] (* TBC *)
    ~tuning:[] (* TBC *)

let set_unreachable pid =
  debug
    "unreachable annotation %a@." WpPropId.pp_id_name pid;
  Property_status.emit 
    wp_unreachable ~hyps:[] (WpPropId.property_of_id pid) Property_status.True

(*----------------------------------------------------------------------------*)
(* Proofs                                                                     *)
(*----------------------------------------------------------------------------*)

type proof = {
  target : Property.t ;
  proved : proofpart array ;
  mutable dependencies : Property.Set.t ;
} and proofpart =
  | Noproof
  | Complete
  | Parts of Bitvector.t

let target p = p.target
let dependencies p = Property.Set.elements (Property.Set.remove p.target p.dependencies)

let create_proof p =
  let n = WpPropId.subproofs p in
  {
    target = WpPropId.property_of_id p ;
    proved = Array.create n Noproof ;
    dependencies = Property.Set.empty ;
  }

let add_proof pf p hs =
  begin
    if not (Property.equal (WpPropId.property_of_id p) pf.target)
    then Wp_parameters.fatal "Partial proof inconsistency" ;
    List.iter
      (fun iph ->
         if not (WpPropId.is_requires iph) then
           pf.dependencies <- Property.Set.add iph pf.dependencies
      ) hs ;
    let k = WpPropId.subproof_idx p in
    match WpPropId.parts_of_id p with
      | None -> pf.proved.(k) <- Complete
      | Some(p,n) ->
          match pf.proved.(k) with
            | Complete -> ()
            | Noproof ->
                let bv = Bitvector.create n in
                Bitvector.set_range bv 0 (p-1) ;
                Bitvector.set_range bv (p+1) (n-1) ;
                pf.proved.(k) <- Parts bv
            | Parts bv ->
                Bitvector.clear bv p ;
                if Bitvector.is_empty bv
                then pf.proved.(k) <- Complete
  end

let is_composed pf =
  Array.length pf.proved > 1

let is_proved pf =
  try Array.iter (fun r -> if r<>Complete then raise Exit) pf.proved ; true
  with Exit -> false

(* -------------------------------------------------------------------------- *)
(* --- PID for Functions                                                  --- *)
(* -------------------------------------------------------------------------- *)

let mk_call_pre_id called_kf bhv s_call called_pre =
  (* TODOclean : quite dirty here ! *)
  let id = WpPropId.mk_pre_id called_kf Kglobal bhv called_pre in
  let called_pre = WpPropId.property_of_id id in
  setup_precondition_proxy called_kf called_pre ;
  let called_pre_p = PreCondAt.property called_pre s_call in
  WpPropId.mk_call_pre_id called_kf s_call called_pre called_pre_p

(* -------------------------------------------------------------------------- *)
(* --- Prop Splitter                                                      --- *)
(* -------------------------------------------------------------------------- *)

(* prop-id splitter *)

let split job pid goals =
  let n = Bag.length goals in
  if n <= 1 then Bag.iter (job pid) goals else
    let k = ref 0 in
    Bag.iter
      (fun g ->
         let pid_k = WpPropId.mk_part pid (!k,n) in
         incr k ; job pid_k g)
      goals

(*----------------------------------------------------------------------------*)
(* Strategy and annotations                                                   *)
(*----------------------------------------------------------------------------*)

(* This is to code what kind of properties we want to process. *)
type asked_assigns = NoAssigns | OnlyAssigns | WithAssigns

(* This is to code which behavior the computed strategy refers to. *)
type asked_bhv =
  | FunBhv of funbehavior option (* None means default behavior 
              when the function has no spec. This is useful to process internal
              properties even if the function has no default behavior *)
  | StmtBhv of Cil2cfg.node * stmt * funbehavior

let name_of_asked_bhv = function
  | FunBhv (Some bhv) -> bhv.b_name
  | FunBhv None -> Cil.default_behavior_name
  | StmtBhv (_, _, bhv) -> bhv.b_name

(* This is to code what properties the user asked for in a given behavior. *)
type asked_prop =
  | AllProps
  | OnlyPreconds
  | NamedProp of string
  | IdProp of Property.t
  | CallPre of stmt * Property.t option (** No specified property means all *)

(* a table to keep the information about the statement default specification
 * associated with each edge in order to know in which strategy we should put a
 * default annotation on this edge. When an edge has no information in the table,
 * it means that the edge annotations belong to the [FunBhv] default behavior;
 * and when we find a statement [s], it means that they belong to the [StmtBhv s]
 * default behavior. The [int] information is only useful to build the table :
 * when an edge is included in 2 different [StmtBhv] we only keep the one that
 * has the fewer internal edges because it is necessarily included in the other.
 *)
module HdefAnnotBhv = Cil2cfg.HE (struct type t = (stmt * int) end)

(* Finally, a configuration is associated to a strategy computation to
 * summarize what is to be computed. *)
type strategy_info = {
  kf : Kernel_function.t;
  cfg : Cil2cfg.t;
  cur_bhv : asked_bhv;
  asked_bhvs : asked_bhv list;
  asked_prop : asked_prop;
  assigns_filter : asked_assigns;
  def_annots_info : HdefAnnotBhv.t;
}

(*----------------------------------------------------------------------------*)
(* Adding things in the stategy                                               *)
(*----------------------------------------------------------------------------*)

(* Select annotations to take as Hyp/Goal/... *)

let pp_assigns_mode fmt config =
  let str = match config.assigns_filter with
  | NoAssigns -> "without assigns"
  | OnlyAssigns -> "only with assigns"
  | WithAssigns -> "both assigns or not"
  in Format.fprintf fmt "%s" str

let pp_asked_prop fmt config = match config.asked_prop  with
  | AllProps -> Format.fprintf fmt "all properties"
  | OnlyPreconds -> Format.fprintf fmt "main preconditions"
  | NamedProp s ->  Format.fprintf fmt "properties %s" s
  | IdProp p -> Format.fprintf fmt "property %s" (WpPropId.id_prop_txt p)
  | CallPre (s, Some p) -> Format.fprintf fmt "pre %s at stmt %a"
                             (WpPropId.id_prop_txt p) Stmt.pretty_sid s
  | CallPre (s, None) -> Format.fprintf fmt "all call preconditions at stmt %a"
                             Stmt.pretty_sid s

let pp_strategy_info fmt config =
  Format.fprintf fmt "'%a', " Kernel_function.pretty config.kf;
  let _ = match config.cur_bhv with
    | FunBhv _bhv ->
        Format.fprintf fmt "behavior '%s'" (name_of_asked_bhv config.cur_bhv)
    | StmtBhv (_, s, bhv) ->
        Format.fprintf fmt "behavior '%s' of statement %d" bhv.b_name s.sid
  in Format.fprintf fmt ", %a, %a"
       pp_asked_prop config pp_assigns_mode config

let cur_fct_default_bhv config = match config.cur_bhv with
  | FunBhv None -> true
  | FunBhv (Some bhv) -> bhv.b_name = Cil.default_behavior_name
  | _ -> false

let filter_status id =
  Wp_parameters.StatusAll.get () ||
    match Property_status.get (WpPropId.property_of_id id) with
      | Property_status.Best(Property_status.True, _) -> 
	  Wp_parameters.StatusTrue.get ()
      | Property_status.Best(Property_status.Dont_know, _) -> 
	  Wp_parameters.StatusMaybe.get ()
      | Property_status.Best((Property_status.False_if_reachable
			     | Property_status.False_and_reachable), _) -> 
	  Wp_parameters.StatusFalse.get ()
      | Property_status.Never_tried -> true
      | Property_status.Inconsistent _ -> false
	
let filter_precond pid =
  match WpPropId.property_of_id pid with
    | Property.IPPredicate( Property.PKRequires _ , _ , Kglobal , _ ) -> true
    | _ -> false

let goal_to_select config pid =
  let asked, take_it =
    match config.assigns_filter, WpPropId.property_of_id pid with
      | NoAssigns, Property.IPAssigns _ ->
          "no assigns", false
      | (OnlyAssigns | WithAssigns), Property.IPAssigns _ ->
          "", true
      | OnlyAssigns, _ -> "only assigns", false
      | (NoAssigns | WithAssigns), _ -> "", true
  in
  let asked, (take_it, msg) =
    if take_it then begin
      match config.asked_prop with
      | AllProps ->
        "all",
        if not (filter_status pid)
        then false, " (skipped w.r.t status)"
        else true, " (selected)"
      | OnlyPreconds ->
        "main precondition",
        if not (filter_status pid) || not (filter_precond pid)
        then false, " (main precondition only)"
        else true, " (selected)"
      | IdProp idp ->
        (* Notice that if the user has explicitly selected a property,
         * we consider it as a goal even if it has been proved already *)
        (WpPropId.id_prop_txt idp),
	(Property.equal (WpPropId.property_of_id pid) idp, "")
      | CallPre (s_call, asked_pre) ->
        let take_it, msg = WpPropId.select_call_pre s_call asked_pre pid in 
        let pre_txt = match asked_pre with None -> "all pre "
          | Some pre -> WpPropId.id_prop_txt pre
        in
	pre_txt ^ " at stmt " ^ (string_of_int s_call.sid),
        (take_it, msg)
    | NamedProp str -> str, WpPropId.select_by_name str pid
    end
    else asked, (take_it, "")
  in
    debug "[goal_to_select] %s vs %a -> %s%s@."
        asked WpPropId.pp_id_name pid (if take_it then "select" else "ignore") msg;
    take_it

(*----------------------------------------------------------------------------*)
(* Add properties *)

(* TODO: still have to remove these fonctions... *)

let kind_to_select config kind id = match kind with
    | WpStrategy.Agoal ->
        if goal_to_select config id then Some WpStrategy.Agoal else None
    | WpStrategy.Aboth goal ->
        let goal = goal && goal_to_select config id in
          Some (WpStrategy.Aboth goal)
    | WpStrategy.AcutB goal ->
        let goal = goal && goal_to_select config id in
          Some (WpStrategy.AcutB goal)
    | WpStrategy.AcallPre goal ->
        let goal = goal && goal_to_select config id in
          Some (WpStrategy.AcallPre goal)
    | WpStrategy.Ahyp | WpStrategy.AcallHyp -> Some kind

let add_prop_inv_establish config acc kind s ca p =
  let id = WpPropId.mk_establish_id config.kf s ca in
  match kind_to_select config kind id with None -> acc
    | Some kind -> WpStrategy.add_prop_loop_inv acc kind s id p

let add_prop_inv_preserve config acc kind s ca p =
  let id = WpPropId.mk_preserve_id config.kf s ca in
  match kind_to_select config kind id with None -> acc
    | Some kind -> WpStrategy.add_prop_loop_inv acc kind s id p

let add_prop_inv_fixpoint config acc kind s ca p =
  let id = WpPropId.mk_inv_hyp_id config.kf s ca in
  match kind_to_select config kind id with None -> acc
    | Some kind -> WpStrategy.add_prop_loop_inv acc kind s id p

(*----------------------------------------------------------------------------*)
(* Add Assigns *)

let add_loop_assigns_goal config s (ca, assigns) acc =
  let id = WpPropId.mk_loop_assigns_id config.kf s ca assigns in
  match id with
      None -> acc
    | Some id ->
      if goal_to_select config id then
        let labels = NormAtLabels.labels_loop_assigns s in
        let assigns' = NormAtLabels.preproc_assigns labels assigns in
        let a_desc = WpPropId.mk_loop_assigns_desc s assigns' in
        WpStrategy.add_assigns acc WpStrategy.Agoal id a_desc
      else acc

let add_stmt_assigns_goal config s acc b l_post = match b.b_assigns with
  | WritesAny -> acc
  | Writes assigns ->
      let id = WpPropId.mk_stmt_assigns_id config.kf s b assigns in
      match id with
        | None -> acc
        | Some id ->
          if goal_to_select config id then
            let labels = NormAtLabels.labels_stmt_assigns s l_post in
            let assigns = NormAtLabels.preproc_assigns labels assigns in
            let a_desc = WpPropId.mk_stmt_assigns_desc s assigns in
            WpStrategy.add_assigns acc WpStrategy.Agoal id a_desc
          else acc

let add_fct_assigns_goal config acc tkind b = match b.b_assigns with
  | WritesAny -> acc
  | Writes assigns ->
      let id = WpPropId.mk_fct_assigns_id config.kf b tkind assigns in
      match id with
        | None -> acc
        | Some id -> 
          if goal_to_select config id then
            let labels = NormAtLabels.labels_fct_assigns in
            let assigns' = NormAtLabels.preproc_assigns labels assigns in
            let a_desc = WpPropId.mk_kf_assigns_desc assigns' in
            WpStrategy.add_assigns acc WpStrategy.Agoal id a_desc
          else acc

(* ------------------------------------------------------------------------ *)
(* --- Get annotations according to the behavior                        --- *)
(* ------------------------------------------------------------------------ *)

(** find the behavior named [name] in the list *)
let get_named_bhv name bhv_list =
  try Some (List.find (fun b -> b.b_name = name) bhv_list)
  with Not_found -> None

(** Select in [bhv_list] the behavior that has to be processed
 * according to [config] and [ki] current statement. *)
let get_behav config ki bh_list = match config.cur_bhv, ki with
  | FunBhv _, Kglobal -> 
      get_named_bhv (name_of_asked_bhv config.cur_bhv) bh_list
  | StmtBhv (_, s1, b), Kstmt s2 when s1.sid = s2.sid ->
      get_named_bhv b.b_name bh_list
  | _ -> None

(** Tells weather the property belonging to the behaviors in [bhv_name_list]
 * has to be considered according to [config]. *)
type test_behav_res =
  | TBRno   (* [cur_bhv] is not concerned *)
  | TBRhyp  (* the property belongs to [default_behavior],
               but not to [cur_bhv] : it doesn't have to be a Goal
               but can be considered as an hypothesis. *)
  | TBRpart (* the property has to be taken as a Goal, but even if it is
               proved for every [asked_bhvs], it will still be a partial proof.
               TODO: use this to generate PKPartial ! *)
  | TBRok   (* Select as a Goal *)

(** (see [test_behav_res] above).
 * If the annotation doesn't have "for" names, it is a bit complicated because
 * we have to know if the statement [s] is inside a stmt behavior or not. *)
let is_annot_for_config config node s_annot bhv_name_list =
  let edges_before = Cil2cfg.pred_e config.cfg node in
  debug "[is_annot_for_config] at sid:%d for %a ? @."
    s_annot.sid (Wp_error.pp_string_list ~sep:" " ~empty:"<default>")
    bhv_name_list;
  let hyp_but_not_at_post n = (* don't take assert at post pgpt (see #564) *)
    let s_post = match Cil2cfg.get_post_edges config.cfg n with
      | [] -> None
      | e::_ -> Cil2cfg.get_edge_next_stmt config.cfg e
    in match s_post with
      | Some s_post when s_post.sid = s_annot.sid ->  TBRno
      | _ -> TBRhyp
  in
  let res = match bhv_name_list with
    | [] -> (* no spec 'for' in the property *)
        begin
          let e = match edges_before with
            | e::_ -> e
            | _ -> Wp_parameters.fatal "annot with no edge ?"
          in
          match config.cur_bhv with
            | FunBhv _ when cur_fct_default_bhv config ->
                begin
                  try
                    let _ = HdefAnnotBhv.find config.def_annots_info e in
                      TBRhyp
                  with Not_found -> TBRok
                end
            | StmtBhv (n, sb, b) when b.b_name = Cil.default_behavior_name ->
                begin
                  try
                    let s,_ = HdefAnnotBhv.find config.def_annots_info e in
                      if s.sid = sb.sid then TBRok
                      else raise Not_found
                  with Not_found -> hyp_but_not_at_post n
                end
            | FunBhv _ -> TBRhyp
            | StmtBhv (n,_,_) -> hyp_but_not_at_post n
        end
    | bhvs -> (* TODOopt : there is surely a better way to do this : *)
      let asked_bhv = name_of_asked_bhv config.cur_bhv in
      let goal = List.exists (fun bl -> bl = asked_bhv) bhvs in
          if goal then
            let full = (* TODO *) true
              (* List.for_all (fun bl -> is_in bl config.asked_bhvs) bhvs *)
            in (if full then TBRok else TBRpart)
          else TBRno
  in debug "[is_annot_for_config] -> %s@."
    (match res with TBRok -> "ok" | TBRhyp -> "hyp" | TBRno -> "no" 
                  | TBRpart -> "part");
     res

let get_bhv_assumes spec l =
  let rec get_assumes bhv_names = match bhv_names with [] -> []
    | bhv::tl ->
        let l = match get_named_bhv bhv spec.spec_behavior with
          | None -> Wp_parameters.warning "no %s behavior !?!?" bhv;
                    get_assumes tl
          | Some b ->
                (Ast_info.behavior_assumes b)::(get_assumes tl)
        in l
  in get_assumes l

let add_fct_pre config acc spec =
  let kf = config.kf in
  let add_bhv_pre_hyp b acc =
    let impl_assumes = false in
    let kind = WpStrategy.Ahyp in
      WpStrategy.add_prop_fct_bhv_pre acc kind kf b ~impl_assumes
  in
  let add_def_pre_hyp acc =
    match Cil.find_default_behavior spec with None -> acc
      | Some bdef -> add_bhv_pre_hyp bdef acc
  in
  let acc = match get_behav config Kglobal spec.spec_behavior with
    | None -> add_def_pre_hyp acc
    | Some b ->
        let acc =
          if not (Cil.is_default_behavior b) then add_def_pre_hyp acc else acc
        in
        let acc = 
          if WpStrategy.is_main_init kf then
            let add_both acc p = 
              let id = WpPropId.mk_pre_id kf Kglobal b p in
              let goal = goal_to_select config id in 
              let kind = WpStrategy.Aboth goal in
                WpStrategy.add_prop_fct_pre acc kind kf b ~assumes:None p 
            in
            let acc = List.fold_left add_both acc b.b_requires in
            let add_hyp acc p =
              let kind = WpStrategy.Ahyp in
              WpStrategy.add_prop_fct_pre acc kind kf b ~assumes:None p
            in List.fold_left add_hyp acc b.b_assumes
          else add_bhv_pre_hyp b acc
        in acc
  in acc


let add_variant acc spec = (* TODO *)
  let _ = match spec.spec_variant with None -> ()
    | Some v ->
        Wp_parameters.warning "Ignored 'decrease' specification:@, %a@."
                   Cil.d_decreases v
  in acc

let add_terminates acc spec = (* TODO *)
  let _ = match spec.spec_terminates with None -> ()
    | Some p ->  Wp_parameters.warning
                   "Ignored 'terminates' specification:@, %a@."
                   !Ast_printer.d_predicate_named
                   (Logic_const.pred_of_id_pred p)
  in acc

let add_disjoint_behaviors_props config ki spec acc =
  match spec.spec_disjoint_behaviors with [] -> acc
    | l ->
        let add_disj acc bhv_names =
          let id = WpPropId.mk_disj_bhv_id (config.kf, ki, bhv_names) in
            if goal_to_select config id then
              begin
                let prop = Ast_info.disjoint_behaviors spec bhv_names in
                let labels = match ki with
                  | Kglobal -> NormAtLabels.labels_fct_pre
                  | Kstmt s -> NormAtLabels.labels_stmt_pre s
                in WpStrategy.add_prop acc WpStrategy.Agoal labels id prop
              end
            else acc
        in List.fold_left add_disj acc l

let add_complete_behaviors_props config ki spec acc =
  match spec.spec_complete_behaviors with [] -> acc
    | l ->
        let mk_prop acc bhv_names =
          let id = WpPropId.mk_compl_bhv_id (config.kf, ki, bhv_names) in
            if goal_to_select config id then
                let prop = Ast_info.complete_behaviors spec bhv_names in
                let labels = match ki with
                  | Kglobal -> NormAtLabels.labels_fct_pre
                  | Kstmt s -> NormAtLabels.labels_stmt_pre s
                in WpStrategy.add_prop acc WpStrategy.Agoal labels id prop
            else acc
        in List.fold_left mk_prop acc l

let add_behaviors_props config ki spec acc =
  let add = match config.cur_bhv, ki with
    | FunBhv _, Kglobal when cur_fct_default_bhv config -> true
    | StmtBhv (_, cur_s,  b), Kstmt s
        when (s.sid = cur_s.sid && b.b_name = Cil.default_behavior_name) -> true
    | _ -> false
  in
    if add then
      let acc = add_complete_behaviors_props config ki spec acc in
      let acc = add_disjoint_behaviors_props config ki spec acc in
        acc
    else acc

(** Add the post condition of the whole spec as hypothesis.
* Add [old(assumes) => ensures] for all the behaviors,
* and also add an upper approximation of the merged assigns information. *)
let add_stmt_spec_post_as_hyp config v s spec acc =
  let l_post = Cil2cfg.get_post_logic_label config.cfg v in
  let add_bhv_post acc b =
    let assumes = Some (Ast_info.behavior_assumes b) in
    let add tk acc p =
      WpStrategy.add_prop_stmt_post acc WpStrategy.Ahyp config.kf 
        s b tk l_post ~assumes p
    in
    let p_acc, e_acc =
      WpStrategy.fold_bhv_post_cond ~warn:false (add Normal) (add Exits) acc b
    in let p_acc =
      WpStrategy.add_stmt_spec_assigns_hyp p_acc config.kf s l_post spec in
      (* let e_acc =  TODO, but crach at the moment... why ?
      * add_spec_assigns_hyp config ki l_post e_acc spec in *)
      p_acc, e_acc
  in List.fold_left add_bhv_post acc spec.spec_behavior

(** we want to prove this behavior:
* - add the requires as preconditions to both prove and use as hyp,
* - add the assumes as hypotheses,
* - add the postconditions as goals.
*)
let add_stmt_bhv_as_goal config v s b (b_acc, (p_acc, e_acc)) =
  let l_post = Cil2cfg.get_post_logic_label config.cfg v in
  let assumes = None in (* [assumes] are used as separate hypotheses *)
  let add_pre_hyp acc p =
    WpStrategy.add_prop_stmt_pre acc WpStrategy.Ahyp config.kf s b ~assumes p
  in
  let add_pre_goal acc p = 
    let id = WpPropId.mk_pre_id config.kf (Kstmt s) b p in
    let goal = goal_to_select config id in
    let kind = WpStrategy.Aboth goal in
      WpStrategy.add_prop_stmt_pre acc kind config.kf s b ~assumes p
  in
  let add_post tk acc p =
    let id = WpPropId.mk_stmt_post_id config.kf s b (tk, p) in
    let goal = goal_to_select config id in
    let kind = WpStrategy.Aboth goal in
      WpStrategy.add_prop_stmt_post acc kind config.kf s b tk l_post ~assumes p
  in

  let b_acc = List.fold_left add_pre_goal b_acc b.b_requires in
  let b_acc =  List.fold_left add_pre_hyp b_acc b.b_assumes in

  let p_acc, e_acc = WpStrategy.fold_bhv_post_cond ~warn:true 
                       (add_post Normal) (add_post Exits) (p_acc, e_acc) b 
  in
  let p_acc = add_stmt_assigns_goal config s p_acc b l_post in
    (*let e_acc = TODO, but crach at the moment... why ?
                          add_stmt_assigns config s e_acc b l_post in *)
    b_acc, (p_acc, e_acc)

let add_stmt_spec_annots config v s spec ((b_acc, (p_acc, e_acc)) as acc) =
  let acc = add_variant acc spec in
  let acc = add_terminates acc spec in
  match config.cur_bhv with
    | StmtBhv (_n, cur_s, b) when s.sid = cur_s.sid ->
        (*
        begin match get_behav config (Kstmt s) spec.spec_behavior with
          | None -> (* in some cases, it seems that we can have several spec
                       for the same statement -> not an error *) acc
          | Some b ->
              *)
              let b_acc, a_acc = add_stmt_bhv_as_goal config v s b acc in
              let b_acc = add_behaviors_props config (Kstmt s) spec b_acc in
                b_acc, a_acc
    | _ -> (* in all other cases, use the specification as hypothesis *)
        let kind = WpStrategy.Aboth false in
        let b_acc = 
          WpStrategy.add_prop_stmt_spec_pre b_acc kind config.kf s spec 
        in
        let p_acc, e_acc = 
          add_stmt_spec_post_as_hyp config v s spec (p_acc, e_acc) 
        in b_acc, (p_acc, e_acc)

(*----------------------------------------------------------------------------*)
(* Call annotations                                                           *)
(*----------------------------------------------------------------------------*)

let add_called_pre config called_kf s spec =
  debug "[add_called_pre] for %a@."
    Kernel_function.pretty  called_kf;
  let add_behav acc b = (* pre for behavior is [assumes => requires] *)
    let assumes = (Ast_info.behavior_assumes b) in
    let add_pre acc pre =
      let id = mk_call_pre_id called_kf b s pre in
      let kind = WpStrategy.AcallPre (goal_to_select config id) in
        WpStrategy.add_prop_call_pre acc kind id ~assumes pre
    in List.fold_left add_pre acc b.b_requires
  in
  let acc =
    List.fold_left add_behav WpStrategy.empty_acc spec.spec_behavior 
  in
    if acc = WpStrategy.empty_acc then
      debug "no called precond for %a@."
        Kernel_function.pretty  called_kf;
    acc

let add_called_post called_kf termination_kind =
  let spec = Kernel_function.get_spec called_kf in
  debug "[add_called_post] '%s' for %a@."
    (WpPropId.string_of_termination_kind termination_kind)
    Kernel_function.pretty  called_kf;
  let add_behav acc b =
    (* post for behavior is [\old(assumes) => ensures] *)
    let kind = WpStrategy.AcallHyp in
    let assumes = (Ast_info.behavior_assumes b) in
    let add_post acc (tk, p) = 
      if tk = termination_kind 
      then WpStrategy.add_prop_call_post acc kind called_kf b tk ~assumes p
      else acc
    in List.fold_left add_post acc b.b_post_cond
  in 
  let acc = List.fold_left add_behav WpStrategy.empty_acc spec.spec_behavior in
    if acc = WpStrategy.empty_acc then
      debug "no called %s postcondition for %a@."
        (WpPropId.string_of_termination_kind termination_kind)
        Kernel_function.pretty  called_kf;
    acc

let get_call_annots config v s fct =
  let l_post = Cil2cfg.get_post_logic_label config.cfg v in
  match WpStrategy.get_called_kf fct with
    | Some kf ->
        let spec = Kernel_function.get_spec kf in
        let before_annots =
          if rte_precond_status config.kf then WpStrategy.empty_acc
          else add_called_pre config kf s spec
        in
        let post_annots = add_called_post kf Normal in
        let post_annots =
          WpStrategy.add_call_assigns_hyp post_annots config.kf s 
            l_post (Some spec)
        in
        let exits_annots = add_called_post kf Exits in
        before_annots, (post_annots, exits_annots)

    | None ->
        Wp_parameters.warning
          "call through function pointer not implemented yet: \
                                   ignore called function properties.";
        let assigns_annots = 
          WpStrategy.add_call_assigns_hyp WpStrategy.empty_acc config.kf s 
            l_post None
        in WpStrategy.empty_acc, (assigns_annots, assigns_annots)

(*----------------------------------------------------------------------------*)
let add_variant_annot config s ca var_exp loop_entry loop_back =
  let (vpos_id, vpos), (vdecr_id, vdecr) = 
    WpStrategy.mk_variant_properties config.kf s ca var_exp
  in
  let add acc kind id p =
    WpStrategy.add_prop_loop_inv acc kind s id p 
  in
  let add_hyp acc =
    let acc = add acc WpStrategy.Ahyp vdecr_id vdecr in
      add acc WpStrategy.Ahyp vpos_id vpos
  in
  let add_goal acc =
    let acc = 
      if goal_to_select config vdecr_id then
        add acc WpStrategy.Agoal vdecr_id vdecr
      else acc
    in if goal_to_select config vpos_id then
      add acc WpStrategy.Agoal vpos_id vpos
    else acc
  in
  let loop_back =
    if cur_fct_default_bhv config then add_goal loop_back else add_hyp loop_back
  (*TODO: what about variant establishment ??? It seems that [0<v)] is not
  *       proved by induction anymore. Why ? *)
  in loop_entry, loop_back

let add_loop_invariant_annot config vloop s ca b_list inv acc =
  let assigns, loop_entry, loop_back , loop_core = acc in
    (* we have to prove that inv is true for each edge that goes
    * in the loop, so we can assume that inv is true for each edge
    * starting from this point. *)
    match is_annot_for_config config vloop s b_list with
      | TBRok
      | TBRpart (* TODO: PKPartial *)
        ->
          if Wp_parameters.Invariants.get() then begin
            let loop_core = add_prop_inv_fixpoint config loop_core 
                              (WpStrategy.AcutB true) s ca inv
            in assigns, loop_entry , loop_back , loop_core
          end
          else begin
            let loop_entry = add_prop_inv_establish config loop_entry 
                               WpStrategy.Agoal s ca inv in
            let loop_back = add_prop_inv_preserve config loop_back 
                              WpStrategy.Agoal s ca inv in
            let loop_core = add_prop_inv_fixpoint config loop_core 
                              WpStrategy.Ahyp s ca inv in
              assigns, loop_entry , loop_back , loop_core
          end
      | TBRhyp -> (* TODO : add more inv hyp ? *)
          let kind =
            if Wp_parameters.Invariants.get() 
            then (WpStrategy.AcutB false) else WpStrategy.Ahyp
          in
          let loop_core =
            add_prop_inv_fixpoint config loop_core kind s ca inv
          in assigns, loop_entry , loop_back , loop_core
      | TBRno -> acc

let add_stmt_invariant_annot config v s ca b_list inv ((b_acc, a_acc) as acc) =
  let add_to_acc k =
    let b_acc = add_prop_inv_fixpoint config b_acc k s ca inv in
      (b_acc, a_acc)
  in
  let acc =
    match is_annot_for_config config v s b_list with
      | TBRok | TBRpart -> add_to_acc (WpStrategy.AcutB true)
      | TBRhyp -> add_to_acc (WpStrategy.AcutB false)
      | TBRno -> acc
  in acc


(** Returns the annotations for the three edges of the loop node:
 * - loop_entry : goals for the edge entering in the loop
 * - loop_back  : goals for the edge looping to the entry point
 * - loop_core  : fix-point hypothesis for the edge starting the loop core
 *)
let get_loop_annots config vloop s =
  let do_annot a (assigns, loop_entry, loop_back , loop_core as acc) =
    let ca = match a with User ca | AI (_, ca) -> ca in
    match ca.annot_content with
      | AInvariant (b_list, true, inv) ->
          add_loop_invariant_annot config vloop s ca b_list inv acc
      | AVariant (var_exp, None) ->
          let loop_entry, loop_back = 
            add_variant_annot config s ca var_exp loop_entry loop_back 
          in assigns, loop_entry , loop_back , loop_core
      | AVariant (_v, _rel) ->
            Wp_parameters.warning "Ignoring loop variant with measure : %a"
              !Ast_printer.d_code_annotation ca;
            acc
      | AAssigns (_,WritesAny) -> assert false
      | AAssigns (b_list, Writes a) -> (* loop assigns *)
          let h_assigns, g_assigns = assigns in
          let check_assigns old cur =
            match old with
                None -> Some cur
              | Some _ ->
                Wp_parameters.fatal
                  "At most one loop assigns can be associated to a behavior"
          in
          let assigns =
            match is_annot_for_config config vloop s b_list with
            | TBRok | TBRpart ->
              check_assigns h_assigns (ca,a), check_assigns g_assigns (ca,a)
            | TBRhyp ->
              check_assigns h_assigns (ca,a), g_assigns
            | TBRno -> assigns
          in (assigns, loop_entry , loop_back , loop_core)
      | _ -> acc (* see get_stmt_annots *)
  in
  let acc = ((None,None), 
             WpStrategy.empty_acc, WpStrategy.empty_acc, WpStrategy.empty_acc) 
  in
  let (h_assigns, g_assigns), loop_entry , loop_back , loop_core =
    Annotations.single_fold_stmt do_annot s acc
  in
  let loop_back = match g_assigns with 
    | None -> loop_back
    | Some a -> add_loop_assigns_goal config s a loop_back 
  in
  let loop_core = 
    WpStrategy.add_loop_assigns_hyp loop_core config.kf s h_assigns
  in (loop_entry , loop_back , loop_core)

let get_stmt_annots config v s =
  let do_annot a ((b_acc, (a_acc, e_acc)) as acc) =
    let ca = Annotations.get_code_annotation a in
    match ca.annot_content with
      | AInvariant (b_list, loop_inv, inv) ->
          if loop_inv then (* see get_loop_annots *) acc
          else if Wp_parameters.Invariants.get() then
            add_stmt_invariant_annot config v s ca b_list inv acc
          else begin
              Wp_parameters.warning
                "ignored 'invariant' (use -wp-invariants option) : %a"
                 !Ast_printer.d_code_annotation ca;
              acc
          end
      | AAssert (b_list,p) ->
          let kf = config.kf in
          let acc = match is_annot_for_config config v s b_list with
            | TBRno -> acc
            | TBRhyp ->
                let b_acc = 
                  WpStrategy.add_prop_assert b_acc WpStrategy.Ahyp kf s ca p
                in (b_acc, (a_acc, e_acc))
            | TBRok | TBRpart -> 
                let id = WpPropId.mk_assert_id config.kf s ca in
                let kind = WpStrategy.Aboth (goal_to_select config id) in
                let b_acc = WpStrategy.add_prop_assert b_acc kind kf s ca p in
                  (b_acc, (a_acc, e_acc))
          in acc
      | AAssigns (_b_list, _assigns) -> 
        (* loop assigns: see get_loop_annots *) acc
      | AVariant (_v, _rel) -> (* see get_loop_annots *) acc
      | APragma _ ->
          Wp_parameters.warning "Ignored annotation:@ %a"
            !Ast_printer.d_code_annotation ca;
          acc
      | AStmtSpec (b_list, spec) ->
          if b_list <> [] then (* TODO ! *)
            Wp_parameters.warning 
              "Ignored 'for %a' (generalize to all behavior)"
              (Pretty_utils.pp_list ~sep:", " Format.pp_print_string)
              b_list;
          add_stmt_spec_annots config v s spec acc
  in
  let before_acc = WpStrategy.empty_acc in
  let  after_acc = WpStrategy.empty_acc in
  let  exits_acc = WpStrategy.empty_acc in
  let acc = before_acc, (after_acc, exits_acc) in
    Annotations.single_fold_stmt do_annot s acc

let get_fct_pre_annots config spec =
  let acc = WpStrategy.empty_acc in
  let acc = add_fct_pre config acc spec in
  let acc = add_behaviors_props config Kglobal spec acc in
  let acc = add_variant acc spec in
  let acc = add_terminates acc spec in
    acc

let get_fct_post_annots config tkind spec =
  let acc = WpStrategy.empty_acc in
  match get_behav config Kglobal spec.spec_behavior with
    | None -> acc
    | Some b ->
        (* add the postconditions *)
        let f_nothing () _ = () in
        let add tk acc p = 
          let id = WpPropId.mk_fct_post_id config.kf b (tk, p) in
            if goal_to_select config id then
              WpStrategy.add_prop_fct_post acc WpStrategy.Agoal config.kf b tk p
            else acc
        in
        let acc = match tkind with
          | Normal ->
              let acc, _ =
                WpStrategy.fold_bhv_post_cond ~warn:true (add Normal) f_nothing (acc, ()) b
              in acc
          | Exits -> 
              let _, acc =
                WpStrategy.fold_bhv_post_cond ~warn:false f_nothing (add Exits) ((), acc) b
              in acc
          | _ -> assert false
        in (* also add the [assigns] *)
        let acc =
          if Kernel_function.is_definition config.kf
          then add_fct_assigns_goal config acc tkind b
          else WpStrategy.add_fct_bhv_assigns_hyp acc config.kf tkind b
        in acc

(*----------------------------------------------------------------------------*)
(* Build graph annotation for the strategy                                    *)
(*----------------------------------------------------------------------------*)

(** Builds tables that give hypotheses and goals relative to [b] behavior
 * for edges of the cfg to consider during wp computation.
 * [b = None] means that we only consider internal properties to select for the
 * default behavior. This is useful when the function doesn't have any
 * specification.
 * @param asked_prop = Some id -> select only this goal (use all hyps).
 *)
let get_behavior_annots config =
  debug "build strategy for %a@." pp_strategy_info config;
  let cfg = config.cfg in
  let spec = Kernel_function.get_spec config.kf in
  let annots = WpStrategy.create_tbl () in

  let get_node_annot v =
    debug "get_node_annot for node %a" Cil2cfg.pp_node v;
    match Cil2cfg.node_type v with
    | Cil2cfg.Vstart | Cil2cfg.Vend -> ()

    | Cil2cfg.VfctIn -> 
        let pre = get_fct_pre_annots config spec in
          WpStrategy.add_on_edges annots pre (Cil2cfg.succ_e cfg v) 

    | Cil2cfg.VfctOut  -> 
        let post = get_fct_post_annots config Normal spec in
          WpStrategy.add_on_edges annots post (Cil2cfg.succ_e cfg v)

    | Cil2cfg.Vexit -> 
        let post = get_fct_post_annots config Exits spec in
          WpStrategy.add_on_edges annots post (Cil2cfg.succ_e cfg v)

    | Cil2cfg.VblkIn (Cil2cfg.Bstmt s, _)
    | Cil2cfg.Vstmt s
    | Cil2cfg.Vswitch (s,_) | Cil2cfg.Vtest (true, s, _)
      -> 
        let stmt_annots = get_stmt_annots config v s in
          WpStrategy.add_node_annots annots cfg v stmt_annots 

    | Cil2cfg.Vcall (s,_,fct,_) ->
        let stmt_annots = get_stmt_annots config v s in
          WpStrategy.add_node_annots annots cfg v stmt_annots;
        let call_annots = get_call_annots config v s fct in
          WpStrategy.add_node_annots annots cfg v call_annots

    | Cil2cfg.Vloop (_, s) ->
        let stmt_annots = get_stmt_annots config v s in
        let before, _after = stmt_annots in
          (* TODO: what about after ? *)
          WpStrategy.add_loop_annots annots cfg v ~entry:before 
            ~back:WpStrategy.empty_acc ~core:WpStrategy.empty_acc;
          debug "add_loop_annots stmt ok";
        let (entry , back , core) = get_loop_annots config v s in
          debug "get_loop_annots ok";
          WpStrategy.add_loop_annots annots cfg v ~entry ~back ~core

    | Cil2cfg.Vloop2 _ -> (* nothing to do *) ()
    | Cil2cfg.VblkIn (_, _) | Cil2cfg.VblkOut (_, _) -> (* nothing *) ()
    | Cil2cfg.Vtest (false, _s, _) -> (* done in Cil2cfg.Vtest (true) *) ()
  in
    Cil2cfg.iter_nodes get_node_annot cfg;
    annots

(* ------------------------------------------------------------------------ *)
(* ---  Global Properties                                               --- *)
(* ------------------------------------------------------------------------ *)

let add_global_annotations annots =
  let globs = Globals.Annotations.get_all () in
  let globs = List.map (fun (g, _generated) -> g) globs in
  let rec do_g g =
    let (source,_) = Cil_datatype.Global_annotation.loc g in
    match g with
    | Daxiomatic (_ax_name, globs,_) -> do_globs globs 
    | Dvolatile _ ->
        (* nothing to do *) ()
    | Dfun_or_pred _ ->
        (* will be processed while translation is needed *) ()
    | Dtype _ ->
        (* will be processed while translation is needed *) ()
    | Dtype_annot (linfo,_) ->
        Wp_parameters.warning ~source
          "Type invariant not handled yet ('%s' ignored)"
          linfo.l_var_info.lv_name;
        ()
    | Dmodel_annot (linfo,_) ->
        Wp_parameters.warning ~source
          "Model fields not handled yet (model field '%s' ignored)"
          linfo.l_var_info.lv_name;
        ()
    | Dinvariant (linfo,_) ->
        Wp_parameters.warning ~source
          "Global invariant not handled yet ('%s' ignored)"
          linfo.l_var_info.lv_name;
        ()
    | Dlemma (name, is_axiom, labels, _, pred,_) ->
        if not is_axiom then
          Wp_parameters.warning ~once:true ~source
            "Proof obligation for property '%s' not generated." name ;
        WpStrategy.add_axiom annots name labels pred
  and do_globs globs = List.iter do_g globs in
    do_globs globs;
    annots

(* ------------------------------------------------------------------------ *)
(* --- Main functions to build the strategies                           --- *)
(* ------------------------------------------------------------------------ *)

let behavior_name_of_config config =
  match config.cur_bhv with
    | FunBhv None -> None
    | FunBhv (Some b) when b.b_name = Cil.default_behavior_name -> None
    | FunBhv (Some b) -> Some b.b_name
    | StmtBhv (_, s, b) when b.b_name = Cil.default_behavior_name ->
        Some ("default_for_stmt_"^(string_of_int s.sid))(*TODO better name ?*)
    | StmtBhv (_, s, b) -> Some (b.b_name^"_stmt_"^(string_of_int s.sid))

let build_bhv_strategy config =
  let annots = get_behavior_annots config in
  let annots = add_global_annotations annots in
  let desc = Pretty_utils.sfprintf "%a" pp_strategy_info config in
  let new_loops = Wp_parameters.Invariants.get() in
    WpStrategy.mk_strategy desc config.cfg (behavior_name_of_config config)
      new_loops WpStrategy.SKannots annots

(* Visit the CFG to find all the internal statement specifications.
 * (see [HdefAnnotBhv] documentation for infomation about this table).
 *)
let internal_function_behaviors cfg =
  let def_annot_bhv = HdefAnnotBhv.create 42 in
  let get_stmt_bhv node stmt acc =
    let add_bhv_info acc b =
      if b.b_name = Cil.default_behavior_name then
        begin
          let _, int_edges = Cil2cfg.get_internal_edges cfg node in
          let n = Cil2cfg.Eset.cardinal int_edges in
          let reg e =
            try
              let (_old_s, old_n) = HdefAnnotBhv.find def_annot_bhv e in
                if n < old_n then
                  (* new spec is included in the old one : override. *)
                  raise Not_found
            with Not_found ->
              HdefAnnotBhv.replace def_annot_bhv e (stmt, n)
          in
            Cil2cfg.Eset.iter reg int_edges
        end;
      (node, stmt, b)::acc
    in
    let spec_bhv_names acc annot = match annot with
      | {annot_content = AStmtSpec (_,spec)} ->
          List.fold_left add_bhv_info acc spec.spec_behavior
      | _ -> Wp_parameters.fatal "filter on is_contract didn't work ?"
    in
    let annots = Annotations.get_filter Logic_utils.is_contract stmt in
    let annots = List.map Annotations.get_code_annotation annots in
      List.fold_left spec_bhv_names acc annots
  in
  let get_bhv n ((seen_stmts, bhvs) as l) =
    match Cil2cfg.start_stmt_of_node n with None -> l
    | Some s ->
        if List.mem s.sid seen_stmts then l
        else
          let seen_stmts = s.sid::seen_stmts in
          let bhvs = get_stmt_bhv n s bhvs in
            (seen_stmts, bhvs)
  in
  let _, bhvs = Cil2cfg.fold_nodes get_bhv cfg ([], []) in
    bhvs, def_annot_bhv


(** empty [bhv_names] means all (whatever [ki] is) *)
let find_behaviors kf cfg ki bhv_names =
  let spec = Kernel_function.get_spec kf in
  let f_bhvs = spec.spec_behavior in
  let s_bhvs, def_annot_bhv = internal_function_behaviors cfg in

  let add_fct_bhv (def, acc) b =
    let add () =
      let def = if Cil.is_default_behavior b then true else def in
        def, (FunBhv (Some b))::acc
    in
    if bhv_names = [] then add()
    else match ki with
      | None (* not specified ki *) | Some Kglobal ->
          if List.mem b.b_name bhv_names then add () else (def, acc)
      | Some Kstmt _ -> def, acc
  in

  let add_stmt_bhv acc (n,s,b) = 
    if bhv_names = [] then (StmtBhv (n,s,b))::acc 
    else if List.mem b.b_name bhv_names then
      let acc = match ki with
        | None -> (* not specified ki *) (StmtBhv (n, s, b))::acc
        | Some (Kstmt stmt) when stmt.sid = s.sid ->
            (StmtBhv (n, s, b))::acc
        | _ -> (* specified ki but not this one *) acc
      in acc
    else acc
  in
    
  let f_bhvs = List.rev f_bhvs in (* for compatibility with previous version *)
  let def, bhvs = List.fold_left add_fct_bhv (false, []) f_bhvs in
  let bhvs = List.fold_left add_stmt_bhv bhvs s_bhvs in
  let bhvs = 
    if def then (* fct default behavior already in *) bhvs
    else if bhv_names = [] then (FunBhv None)::bhvs
    else match ki with
      | None (* not specified ki *) | Some Kglobal ->
          if List.mem Cil.default_behavior_name bhv_names 
          then (FunBhv None)::bhvs
          else bhvs
      | Some Kstmt _ -> bhvs
  in def_annot_bhv, bhvs

(*----------------------------------------------------------------------------*)
(* Unreachable                                                                *)
(*----------------------------------------------------------------------------*)

let process_unreached_annots cfg =
  debug "collecting unreachable annotations@.";
  let unreached = Cil2cfg.unreachable_nodes cfg in
  let kf = Cil2cfg.cfg_kf cfg in
  let spec = Kernel_function.get_spec kf in
  let add_id id acc =
    if filter_status id then id::acc
    else (* non-selected property : nothing to do *) acc
  in
  let do_post b tk acc (termk, _ as p) =
    if tk = termk then add_id (WpPropId.mk_fct_post_id kf b p) acc else acc
  in
  let do_bhv termk acc b = List.fold_left (do_post b termk) acc b.b_post_cond in
  let do_annot s a acc =
    let ca = match a with User ca | AI (_, ca) -> ca in
      add_id (WpPropId.mk_code_annot_id kf s ca) acc
  in
  let do_node acc n =
     debug
       "process annotations of unreachable node %a@."
       Cil2cfg.pp_node_type n;
    match n with
    | Cil2cfg.Vstart -> Wp_parameters.fatal "Start must be reachable"
    | Cil2cfg.VfctIn -> Wp_parameters.fatal "FctIn must be reachable"
    | Cil2cfg.VfctOut  -> List.fold_left (do_bhv Normal) acc spec.spec_behavior
    | Cil2cfg.Vexit  -> List.fold_left (do_bhv Exits) acc spec.spec_behavior
    | Cil2cfg.Vstmt s
    | Cil2cfg.VblkIn (Cil2cfg.Bstmt s, _) | Cil2cfg.Vcall (s, _, _, _)
    | Cil2cfg.Vtest (true, s, _) | Cil2cfg.Vloop (_, s) | Cil2cfg.Vswitch (s,_)
      -> Annotations.single_fold_stmt (do_annot s) s acc
    | Cil2cfg.Vtest (false, _, _) | Cil2cfg.Vloop2 _ 
    | Cil2cfg.VblkIn _ | Cil2cfg.VblkOut _ | Cil2cfg.Vend -> acc
  in
  let annots = List.fold_left do_node [] unreached in
    debug
      "found %d unreachable annotations@." (List.length annots) ;
  List.iter (fun pid -> set_unreachable pid) annots

(*----------------------------------------------------------------------------*)
(* Everything must go through here.                                           *)
(*----------------------------------------------------------------------------*)

let get_cfg kf =
  if Wp_parameters.RTE.get () then compute_rte_for kf ;
  let cfg = Cil2cfg.get kf in
  let _ = process_unreached_annots cfg in
  let do_dot = Wp_parameters.Dot.get () in
  let _dot = if do_dot then Some (Cil2cfg.dot_cfg cfg) else None in
    cfg

let build_configs assigns kf behaviors ki property =
  debug "[get_strategies] for behaviors names: %a@."
    (Wp_error.pp_string_list ~sep:" " ~empty:"<none>")
    (match behaviors with [] -> ["<all>"] | _ :: _ as l -> l) ;
  let _ = match ki with
    | None -> ()
    | Some Kglobal ->
        debug
          "[get_strategies] select in function properies@."
    | Some (Kstmt s) ->
        debug
          "[get_strategies] select stmt %d properties@." s.sid
  in
  let cfg = get_cfg kf in
  let property = match property with
    | AllProps -> if Cil2cfg.cfg_spec_only cfg then OnlyPreconds else AllProps
    | _ -> property
  in
  let def_annot_bhv, bhvs = find_behaviors kf cfg ki behaviors in
  if bhvs = [] then
    Wp_parameters.warning "[get_strategies] no behaviors found"
  else
    debug "[get_strategies] %d behaviors"
      (List.length bhvs);
  let mk_bhv_config bhv = { kf = kf;
                     cfg = cfg;
                     cur_bhv = bhv;
                     asked_prop = property;
                     asked_bhvs = bhvs;
                     assigns_filter = assigns;
                     def_annots_info = def_annot_bhv }
  in List.map mk_bhv_config bhvs

let get_strategies assigns kf behaviors ki property =
  let configs = build_configs assigns kf behaviors ki property in
  let rec add_stgs l = match l with [] -> []
    | config::tl ->
        let stg = build_bhv_strategy config in
        let stgs = stg::(add_stgs tl) in
          match config.cur_bhv, config.asked_prop with
            | FunBhv (Some b), AllProps -> 
                let froms = Property.ip_from_of_behavior kf Kglobal b in
                let add acc ip = match ip with
                  | Property.IPFrom id_from ->
                      (WpFroms.get_strategy_for_from id_from)::acc
                  | _ -> acc
                in List.fold_left add stgs froms

            | _, _ -> (* TODO *) stgs
  in add_stgs configs

(*----------------------------------------------------------------------------*)
(* Public functions to build the strategies                                   *)
(*----------------------------------------------------------------------------*)

let get_precond_strategies p =
  debug "[get_precond_strategies] %s@."
    (WpPropId.id_prop_txt p);
  match p with
    | Property.IPPredicate (Property.PKRequires b, kf, Kglobal, _) ->
        let strategies =
          if WpStrategy.is_main_init kf then
              get_strategies NoAssigns kf [b.b_name] None (IdProp p)
          else []
        in
        let call_sites = Kernel_function.find_syntactic_callsites kf in
        let add_call_pre_stategy acc (kf_caller, stmt) =
          let asked = CallPre (stmt, Some p) in
          let strategies = get_strategies NoAssigns kf_caller [] None asked in
            strategies @ acc
        in
          if call_sites = [] then
            (Wp_parameters.warning
              "no direct call sites for '%a': cannot check pre-conditions"
              Kernel_function.pretty kf;
             strategies)
          else List.fold_left add_call_pre_stategy strategies call_sites
    | _ ->
      invalid_arg "[get_precond_strategies] not a function precondition"

let get_call_pre_strategies stmt =
  debug
    "[get_call_pre_strategies] on statement %a@." Stmt.pretty_sid stmt;
  match stmt.skind with
    | Instr(Call(_,f,_,_)) ->
        let strategies = match WpStrategy.get_called_kf f with
          | None ->
              Wp_parameters.warning
                "call through function pointer not implemented yet: \
                 cannot check pre-conditions for statement %a"
                Stmt.pretty_sid stmt;
              []
          | Some _kf_called ->
              let kf_caller = Kernel_function.find_englobing_kf stmt in
              let asked = CallPre (stmt, None) in
                get_strategies NoAssigns kf_caller [] None asked
        in strategies
    | _ -> Wp_parameters.warning
             "[get_call_pre_strategies] this is not a call statement"; []

let get_id_prop_strategies ?(assigns=WithAssigns) p =
  debug "[get_id_prop_strategies] %s@."
    (WpPropId.id_prop_txt p);
    match p with
      | Property.IPCodeAnnot (kf,_,ca) ->
          let bhvs = match ca.annot_content with
            | AAssert (l, _) | AInvariant (l, _, _) | AAssigns (l, _) -> l
            | _ -> []
          in get_strategies assigns kf bhvs None (IdProp p)
      | Property.IPAssigns (kf, _, Property.Id_code_annot _, _)
      (*loop assigns: belongs to the default behavior *)
      | Property.IPDecrease (kf,_,_,_) ->
      (* any variant property is attached to the default behavior of
       * the function, NOT to a statement behavior *)
          let bhvs = [ Cil.default_behavior_name ] in
            get_strategies assigns kf bhvs None (IdProp p)
      | Property.IPPredicate (Property.PKRequires _, _kf, Kglobal, _p) ->
          get_precond_strategies p
      | Property.IPFrom id_from ->
          [ WpFroms.get_strategy_for_from id_from ]
      | _ ->
          let strategies = match Property.get_kf p with
            | None -> Wp_parameters.warning
                        "WP of property outside functions: ignore %s"
                        (WpPropId.id_prop_txt p); []
            | Some kf ->
                let ki = Some (Property.get_kinstr p) in
                let bhv = match Property.get_behavior p with
                  | None -> Cil.default_behavior_name
                  | Some fb -> fb.b_name
                in get_strategies assigns kf [bhv] ki (IdProp p)
          in strategies

let get_behavior_strategies ?(assigns=WithAssigns) kf bhvs =
  let ki = None in
  let stgs = get_strategies assigns kf bhvs ki AllProps in
    stgs

let get_function_strategies ?(assigns=WithAssigns) kf =
  get_strategies assigns kf [] None AllProps

(* TODO: it could be better to first find the property in order
 * to compute only in its functions and behaviors...
 * At the moment [p_bhvs] is given as a hint (see [get_id_prop_strategies])
 *)
let get_prop_strategies ?(assigns=WithAssigns) kf (p_bhvs, prop_name) =
  get_strategies assigns kf p_bhvs None (NamedProp prop_name)

(*----------------------------------------------------------------------------*)
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
(*----------------------------------------------------------------------------*)
