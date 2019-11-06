(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(* --- WP Computer (main entry points)                                    --- *)
(* -------------------------------------------------------------------------- *)

class type computer =
  object
    method lemma : bool
    method model : WpContext.model
    method add_strategy : WpStrategy.strategy -> unit
    method add_lemma : LogicUsage.logic_lemma -> unit
    method compute : Wpo.t Bag.t
  end

(* -------------------------------------------------------------------------- *)
(* --- Property Entry Point                                               --- *)
(* -------------------------------------------------------------------------- *)

let compute_ip cc ip =
  let open Property in match ip with
  | IPLemma _
  | IPAxiomatic _
    ->
      let rec iter cc = function
        | IPLemma {il_name} -> cc#add_lemma (LogicUsage.logic_lemma il_name)
        | IPAxiomatic {iax_props} -> List.iter (iter cc) iax_props
        | _ -> ()
      in iter cc ip ;
      cc#compute

  | IPBehavior {ib_kf; ib_bhv} ->
      let model = cc#model in
      let bhv = [ib_bhv.Cil_types.b_name] in
      let assigns = WpAnnot.WithAssigns in
      List.iter cc#add_strategy
        (WpAnnot.get_function_strategies ~model ~assigns ~bhv ib_kf) ;
      cc#compute
  | IPComplete _
  | IPDisjoint _
  | IPCodeAnnot _
  | IPAllocation _
  | IPAssigns _
  | IPDecrease _
  | IPPredicate _
    ->
      let model = cc#model in
      let assigns = WpAnnot.WithAssigns in
      List.iter cc#add_strategy
        (WpAnnot.get_id_prop_strategies ~model ~assigns ip) ;
      cc#compute

  | IPFrom _
  | IPAxiom _
  | IPReachable _
  | IPPropertyInstance _
  | IPOther _
  | IPTypeInvariant _
  | IPGlobalInvariant _
  | IPExtended _
    ->
      Wp_parameters.result "Nothing to compute for '%a'" pretty ip ;
      Bag.empty

(* -------------------------------------------------------------------------- *)
(* --- Annotations Entry Point                                            --- *)
(* -------------------------------------------------------------------------- *)

let add_kf cc ?bhv ?prop kf =
  let model = cc#model in
  let assigns = WpAnnot.WithAssigns in
  List.iter cc#add_strategy
    (WpAnnot.get_function_strategies ~model ~assigns ?bhv ?prop kf)

let add_lemmas cc = function
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

let compute_kf cc ?kf ?bhv ?prop () =
  begin
    Extlib.may (add_kf cc ?bhv ?prop) kf ;
    cc#compute
  end

let compute_selection cc ?(fct=Wp_parameters.Fct_all) ?bhv ?prop () =
  begin
    add_lemmas cc prop ;
    Wp_parameters.iter_fct (add_kf cc ?bhv ?prop) fct ;
    cc#compute
  end

let compute_call cc stmt =
  let model = cc#model in
  List.iter cc#add_strategy (WpAnnot.get_call_pre_strategies ~model stmt) ;
  cc#compute

(* -------------------------------------------------------------------------- *)
