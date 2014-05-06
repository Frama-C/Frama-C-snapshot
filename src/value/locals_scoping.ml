(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

open Cil_types
open Locations

type clobbered_set = {
  mutable clob: Base.SetLattice.t
}

let bottom () = { clob = Base.SetLattice.bottom }
let top () = { clob = Base.SetLattice.top }

let remember_bases_with_locals clob new_clob =
  clob.clob <- Base.SetLattice.join new_clob clob.clob

let remember_if_locals_in_value clob left_loc v =
  if Cvalue.V.contains_addresses_of_any_locals v then
    let new_clob = Location_Bits.get_bases left_loc.loc in
    remember_bases_with_locals clob new_clob

let remember_if_locals_in_offsetmap clob left_loc offm =
  try
    Cvalue.V_Offsetmap.iter_on_values
      (fun v _ ->
         if Cvalue.V.contains_addresses_of_any_locals
           (Cvalue.V_Or_Uninitialized.get_v v)
         then
           let new_clob = Location_Bits.get_bases left_loc.loc in
           remember_bases_with_locals clob new_clob;
           raise Exit
      ) offm
  with Exit -> ()


type topify_offsetmap =
  Cvalue.V_Offsetmap.t ->
  Base.SetLattice.t * Cvalue.V_Offsetmap.t

type topify_offsetmap_approx =
  exact:bool ->
  topify_offsetmap

type topify_state = Cvalue.Model.t -> Cvalue.Model.t


(* For all bindings [v] of [offsm] that verify [test], replace them by
   [snd (topify v)], and gather [fst (topify v)] within [acc_locals] *)
let top_gather_locals test topify join acc_locals : topify_offsetmap =
  fun offsm ->
  assert (not (Cvalue.V_Offsetmap.is_empty offsm));
  Cvalue.V_Offsetmap.fold
    (fun (_,_ as i) (v, m, r) (acc_locals, acc_o as acc) ->
       if test v
       then
         let locals, topified_v = topify v in
         (join acc_locals locals),
       Cvalue.V_Offsetmap.add i (topified_v, m, r) acc_o
       else acc)
    offsm
    (acc_locals, offsm)


(* Return a function that topifies all parts of an offsetmap that contains a
   pointer that verifying [is_local]. *)
let offsetmap_top_addresses_of_locals is_local : topify_offsetmap_approx =
  (* Partial application is important, this function has a cache *)
  let is_local_bytes = Location_Bytes.contains_addresses_of_locals is_local in
  fun ~exact offsetmap ->
    if Cvalue.V_Offsetmap.is_empty offsetmap
    then Base.SetLattice.top, offsetmap
    else
      let loc_contains_addresses_of_locals t =
	let v = Cvalue.V_Or_Uninitialized.get_v t in
	is_local_bytes v
      in
      let locals, result =
        top_gather_locals
	  loc_contains_addresses_of_locals
	  (Cvalue.V_Or_Uninitialized.unspecify_escaping_locals ~exact is_local)
	  Base.SetLattice.join
	  Base.SetLattice.bottom
	  offsetmap
      in
      locals, result

(* Topify the locals in the offsetmaps bound to [bases] in [state]. *)
let state_top_addresses_of_locals ~exact fwarn_escape (topify_offsetmap:topify_offsetmap_approx) bases state =
  (* Assumes [offsm] is bound to [base] in [state]. Remove locals from [offsm],
     and bind it again to [base] in the result. *)
  let aux base offsm state =
    let locals, offsm' = topify_offsetmap ~exact offsm in
    let found_locals = not (Cvalue.V_Offsetmap.equal offsm' offsm) in
    if found_locals then
      ((fwarn_escape base locals : unit);
       Cvalue.Model.add_base base offsm' state)
    else state
  in
  (* Clean the locals in the offsetmap bound to [base] in [state] *)
  let aux' base state =
    try
      let offsm = Cvalue.Model.find_base base state in
      aux base offsm state
    with Not_found -> state
  in
  try (* Iterate on all the bases that might contain a local, and clean them*)
    Base.SetLattice.fold aux' bases.clob (aux' Base.null state)
  with Base.SetLattice.Error_Top ->
    begin (* [bases] is too imprecise. Iterate on the entire memory state
             instead, which is much slower *)
      try
        Cvalue.Model.fold_base_offsetmap aux state state
      with Cvalue.Model.Error_Bottom -> Cvalue.Model.bottom
    end

(* Topifies all references to the locals and formals of [fdec]*)
let top_addresses_of_locals fdec clob =
  let entry_point, lib = Kernel.MainFunction.get (), Kernel.LibEntry.get () in
  (* Do nothing for main, except in lib-entry mode (no sense to warn for
     a variable escaping the main function) *)
  if lib || not (fdec.svar.vname = entry_point)
  then
    let offsetmap_top_addresses_of_locals =
      offsetmap_top_addresses_of_locals
        (Extlib.swap Base.is_formal_or_local fdec)
    in
    let state_top_addresses_of_locals =
      state_top_addresses_of_locals 
        (Warn.warn_locals_escape false fdec)
        offsetmap_top_addresses_of_locals clob
    in
    (offsetmap_top_addresses_of_locals ~exact:true,
     state_top_addresses_of_locals ~exact:true)
  else (fun x -> Base.SetLattice.bottom, x),(fun x -> x)

(* Topifies all the references to the variables local to [blocks] *)
let block_top_addresses_of_locals fdec clob blocks =
  (* no need to topify references to [v] if it is not referenced, or if it
     a Cil temporary *)
  let safe_var v = v.vgenerated || not v.vreferenced in
  if List.for_all (fun b -> List.for_all safe_var b.blocals) blocks then
    fun x -> x
  else
    let offsetmap_top_addresses_of_locals =
      offsetmap_top_addresses_of_locals
	(fun v -> List.exists (Base.is_block_local v) blocks)
    in
    let state_top_addresses_of_locals =
      state_top_addresses_of_locals
        (Warn.warn_locals_escape true fdec)
	offsetmap_top_addresses_of_locals
	clob
    in
    state_top_addresses_of_locals ~exact:true
