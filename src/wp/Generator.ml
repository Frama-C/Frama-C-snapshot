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
(* --- WP Computer (main entry points)                                    --- *)
(* -------------------------------------------------------------------------- *)

class type computer =
object
  method lemma : bool
  method add_strategy : WpStrategy.strategy -> unit
  method add_lemma : LogicUsage.logic_lemma -> unit
  method compute : Wpo.t Bag.t
end

type model =
  | Generic of computer
  | NonAssigns of computer
  | TwoPasses of computer * computer (* non-assigns / assigns *)

let generic = function Generic cc | NonAssigns cc | TwoPasses(cc,_) -> cc

let add_lemmas model iter_lemmas =
  let cc = generic model in
  iter_lemmas cc#add_lemma

let add_annots model iter_annots =
  match model with
    | Generic cc ->
	iter_annots cc#add_strategy WpAnnot.WithAssigns
    | NonAssigns cc ->
	Wp_parameters.warning
          ~current:false ~once:true
          "Ignored Assigns-Goals with '%s' model"
          (Wp_parameters.Model.get ()) ;
	iter_annots cc#add_strategy WpAnnot.NoAssigns
    | TwoPasses(cc,ca) ->
	begin
	  iter_annots cc#add_strategy WpAnnot.NoAssigns ;
	  iter_annots ca#add_strategy WpAnnot.OnlyAssigns ;
	end

let compute = function
  | Generic cc | NonAssigns cc -> cc#compute
  | TwoPasses(c1,c2) -> 
      let g1 = c1#compute in
      let g2 = c2#compute in
      Bag.concat g1 g2

(* -------------------------------------------------------------------------- *)
(* --- Property Entry Point                                               --- *)
(* -------------------------------------------------------------------------- *)

let iter_ip_annots ip cc assigns =
  List.iter cc (WpAnnot.get_id_prop_strategies ~assigns ip)

let iter_ip_lemmas ip cc =
  let rec iter cc = function
    | Property.IPLemma(name,_,_,_,_) -> cc (LogicUsage.logic_lemma name)
    | Property.IPAxiomatic(_,ips) -> List.iter (iter cc) ips
    | _ -> ()
  in iter cc ip

let compute_ip model ip =
  match ip with
    | Property.IPLemma _ 
    | Property.IPAxiomatic _ 
      ->
	add_lemmas model (iter_ip_lemmas ip) ; 
	compute model

    | Property.IPBehavior _ 
    | Property.IPComplete _ 
    | Property.IPDisjoint _
    | Property.IPCodeAnnot _
    | Property.IPAllocation _
    | Property.IPAssigns _
    | Property.IPFrom _
    | Property.IPDecrease _
    | Property.IPPredicate _ 
      ->
	add_annots model (iter_ip_annots ip) ;
	compute model
	
    | Property.IPAxiom _
    | Property.IPReachable _
    | Property.IPOther _
      ->
	Wp_parameters.result "Nothing to compute for '%a'" Property.pretty ip ; 
	Bag.empty

(* -------------------------------------------------------------------------- *)
(* --- Annotations Entry Point                                            --- *)
(* -------------------------------------------------------------------------- *)

let iter_kf phi = function
  | None -> Globals.Functions.iter phi
  | Some kf -> phi kf

let iter_fct phi = function
  | [] -> Globals.Functions.iter phi
  | fs -> List.iter
      (fun f -> 
	 try phi (Globals.Functions.find_by_name f)
	 with Not_found -> 
	   Wp_parameters.error "Unknown function '%s' (skipped)" f
      ) fs

let iter_prop_lemmas prop cc = 
  match prop with
    | None | Some[] -> 
	LogicUsage.iter_lemmas
	  (fun lem ->
	     let idp = WpPropId.mk_lemma_id lem in
	     if WpAnnot.filter_status idp then cc lem)
    | Some ps -> 
	if List.mem "-@lemmas" ps then () 
	else LogicUsage.iter_lemmas
	  (fun lem ->
	     let idp = WpPropId.mk_lemma_id lem in
	     if WpAnnot.filter_status idp && WpPropId.select_by_name ps idp
	     then cc lem)

let iter_prop_annots ?bhv ?prop kf cc assigns =
  List.iter cc (WpAnnot.get_function_strategies ~assigns ?bhv ?prop kf)
    
let add_kf model ?bhv ?prop kf =
  add_annots model (iter_prop_annots ?bhv ?prop kf)

let compute_kf model ?kf ?bhv ?prop () =
  begin
    iter_kf (add_kf model ?bhv ?prop) kf ;
    compute model
  end

let compute_selection model ?(fct=[]) ?bhv ?prop () =
  begin
    if fct=[] then add_lemmas model (iter_prop_lemmas prop) ;
    iter_fct (add_kf model ?bhv ?prop) fct ;
    compute model
  end

(* -------------------------------------------------------------------------- *)
(* --- Calls Entry Point                                                  --- *)
(* -------------------------------------------------------------------------- *)

let compute_call model stmt =
  let cc = generic model in
  List.iter cc#add_strategy (WpAnnot.get_call_pre_strategies stmt) ;
  cc#compute

(* -------------------------------------------------------------------------- *)
(* --- Froms Entry Point                                                  --- *)
(* -------------------------------------------------------------------------- *)
  
let compute_froms model ?(fct=[]) () =
  let cc = generic model in
  iter_fct 
    (fun kf ->
       List.iter cc#add_strategy (WpFroms.get_strategies_for_froms kf)
    ) fct ;
  cc#compute
