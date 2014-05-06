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
	  
    | Property.IPBehavior (kf,_,b)  ->
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
    | Property.IPOther _
      ->
	Wp_parameters.result "Nothing to compute for '%a'" Property.pretty ip ; 
	Bag.empty

(* -------------------------------------------------------------------------- *)
(* --- Annotations Entry Point                                            --- *)
(* -------------------------------------------------------------------------- *)

type functions =
  | F_All
  | F_List of string list
  | F_Skip of string list

let iter_kf phi = function
  | None -> Globals.Functions.iter phi
  | Some kf -> phi kf

let iter_fct phi = function
  | F_All -> Globals.Functions.iter phi
  | F_Skip fs -> Globals.Functions.iter 
      (fun kf ->
	 let f = Kernel_function.get_name kf in
	 if not (List.mem f fs) then phi kf)
  | F_List fs -> List.iter
      (fun f -> 
	 try phi (Globals.Functions.find_by_name f)
	 with Not_found -> 
	   Wp_parameters.error "Unknown function '%s' (skipped)" f
      ) fs
    
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
