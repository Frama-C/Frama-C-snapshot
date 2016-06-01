(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- WP Computer (main entry points)                                    --- *)
(* -------------------------------------------------------------------------- *)

class type computer =
  object
    method lemma : bool
    method add_strategy : WpStrategy.strategy -> unit
    method add_lemma : LogicUsage.logic_lemma -> unit
    method compute : Wpo.t Bag.t
  end

(* -------------------------------------------------------------------------- *)
(* --- Property Entry Point                                               --- *)
(* -------------------------------------------------------------------------- *)

let compute_ip cc ip =
  match ip with
  | Property.IPLemma _
  | Property.IPAxiomatic _
    ->
      let rec iter cc = function
        | Property.IPLemma(name,_,_,_,_) -> cc#add_lemma (LogicUsage.logic_lemma name)
        | Property.IPAxiomatic(_,ips) -> List.iter (iter cc) ips
        | _ -> ()
      in iter cc ip ;
      cc#compute

  | Property.IPBehavior (kf,_,_,b)  ->
      let bhv = [b.Cil_types.b_name] in
      List.iter cc#add_strategy
        (WpAnnot.get_function_strategies ~assigns:WpAnnot.WithAssigns ~bhv kf) ;
      cc#compute
  | Property.IPComplete _
  | Property.IPDisjoint _
  | Property.IPCodeAnnot _
  | Property.IPAllocation _
  | Property.IPAssigns _
  | Property.IPDecrease _
  | Property.IPPredicate _
    ->
      List.iter cc#add_strategy
        (WpAnnot.get_id_prop_strategies ~assigns:WpAnnot.WithAssigns ip) ;
      cc#compute

  | Property.IPFrom _
  | Property.IPAxiom _
  | Property.IPReachable _
  | Property.IPPropertyInstance _
  | Property.IPOther _
  | Property.IPTypeInvariant _
  | Property.IPGlobalInvariant _
    ->
      Wp_parameters.result "Nothing to compute for '%a'" Property.pretty ip ;
      Bag.empty

(* -------------------------------------------------------------------------- *)
(* --- Annotations Entry Point                                            --- *)
(* -------------------------------------------------------------------------- *)

type functions =
  | F_All
  | F_List of Cil_datatype.Kf.Set.t
  | F_Skip of Cil_datatype.Kf.Set.t

let iter_kf phi = function
  | None -> Globals.Functions.iter phi
  | Some kf -> phi kf

let iter_fct phi = function
  | F_All -> Globals.Functions.iter phi
  | F_Skip fs ->
      Globals.Functions.iter
        (fun kf -> if not (Cil_datatype.Kf.Set.mem kf fs) then phi kf)
  | F_List fs -> Cil_datatype.Kf.Set.iter phi fs

let add_kf cc ?bhv ?prop kf =
  List.iter cc#add_strategy
    (WpAnnot.get_function_strategies ~assigns:WpAnnot.WithAssigns ?bhv ?prop kf)

let compute_kf cc ?kf ?bhv ?prop () =
  begin
    iter_kf (add_kf cc ?bhv ?prop) kf ;
    cc#compute
  end

let do_lemmas = function F_All | F_Skip _ -> true | F_List _ -> false

let compute_selection cc ?(fct=F_All) ?bhv ?prop () =
  begin
    if do_lemmas fct then
      begin
        match prop with
        | None | Some[] ->
            LogicUsage.iter_lemmas
              (fun lem ->
                 let idp = WpPropId.mk_lemma_id lem in
                 if WpAnnot.filter_status idp then cc#add_lemma lem)
        | Some ps ->
            if List.mem "-@lemmas" ps then ()
            else LogicUsage.iter_lemmas
                (fun lem ->
                   let idp = WpPropId.mk_lemma_id lem in
                   if WpAnnot.filter_status idp && WpPropId.select_by_name ps idp
                   then cc#add_lemma lem)
      end ;
    iter_fct (add_kf cc ?bhv ?prop) fct ;
    cc#compute
  end

(* -------------------------------------------------------------------------- *)
(* --- Calls Entry Point                                                  --- *)
(* -------------------------------------------------------------------------- *)

let compute_call cc stmt =
  List.iter cc#add_strategy (WpAnnot.get_call_pre_strategies stmt) ;
  cc#compute
