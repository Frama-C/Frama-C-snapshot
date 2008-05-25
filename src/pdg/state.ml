(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** DataState is associated with a program point
    and provide the dependancies for the data,
    ie. it stores for each location the nodes of the pdg where its value
    was last defined.
  *)

module M = Macros
open PdgTypes

type t_loc = Locations.Zone.t
type t_node = Node.t
type t_info = NodeSetLattice.t
type t = PdgTypes.t_data_state
        (* { loc_info : LocInfo.t ; under_outputs : Locations.Zone.t } *)

let make loc_info under_outputs =
  { loc_info = loc_info; under_outputs = under_outputs }

let empty = make LocInfo.empty Locations.Zone.bottom

let pretty fmt state = 
  Format.fprintf fmt "State : under_outputs = %a\nstate = %a"
    Locations.Zone.pretty state.under_outputs
    LocInfo.pretty state.loc_info

let add_loc_node state ~exact loc node =
  if M.debug2 () then
    Format.printf "[pdg] add_loc_node (%s) : node %a -> %a@."
      (if exact then "exact" else "merge")
      PdgTypes.Node.pretty node
      Locations.Zone.pretty loc ;
  let new_info = NodeSetLattice.inject_singleton node in
  let new_loc_info = LocInfo.add_binding exact state.loc_info loc new_info in
  let new_outputs = (* Zone.link in the under-approx version of Zone.join *)
    if exact then Locations.Zone.link state.under_outputs loc
    else state.under_outputs
  in make new_loc_info new_outputs

let test_and_merge ~old new_ =
  if LocInfo.is_included new_.loc_info old.loc_info then (false, old)
  else
    let new_loc_info = LocInfo.join old.loc_info new_.loc_info in
    let new_outputs =
      Locations.Zone.meet old.under_outputs new_.under_outputs
    in
    let new_state =
      { loc_info = new_loc_info ; under_outputs = new_outputs }
    in (true, new_state)


let get_loc_nodes state loc =
  if Locations.Zone.equal loc Locations.Zone.bottom
  then  [], Locations.Zone.bottom (* nothing to do *)
  else
    let node_set = LocInfo.find state.loc_info loc in
    let nodes = NodeSetLattice.fold (fun n acc -> n::acc) node_set [] in
    let undef_zone = Locations.Zone.diff loc state.under_outputs in
      if M.debug2 () then
        begin
        Format.printf "[pdg state] state = %a@." pretty state ;
        Format.printf "[pdg state] get_loc_nodes %a@." 
          Locations.Zone.pretty loc;
        Format.printf "[pdg state] get_loc_nodes -> undef = %a@." 
          Locations.Zone.pretty undef_zone
        end;
      nodes, undef_zone

let get_all_nodes state =
  let add _z (_def, nodes) acc = NodeSetLattice.join acc nodes in
  let node_set = LocInfo.fold add state.loc_info NodeSetLattice.empty in
  let nodes = NodeSetLattice.fold (fun n acc -> n::acc) node_set [] in
    nodes

               (*
let fold_on_nodes state loc f acc0 =
  let info = LocInfo.find state.loc_info loc in
  let fct node acc = f node acc in
    NodeSet.fold fct info acc0
    *)

let iter_on_nodes state loc f =
  let nodes, undef_zone = get_loc_nodes state loc in
    List.iter f nodes;
    undef_zone

type t_states = t Inthash.t

let store_init_state states state = Inthash.add states (-1) state
let store_last_state states state = Inthash.add states 0 state

let get_init_state states = Inthash.find states (-1)
let get_last_state states = Inthash.find states 0
let get_stmt_state states stmt = Inthash.find states stmt.Cil_types.sid


let get_pdg_states states = states

