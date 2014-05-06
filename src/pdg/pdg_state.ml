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

(** DataState is associated with a program point
    and provide the dependancies for the data,
    ie. it stores for each location the nodes of the pdg where its value
    was last defined.
  *)

let dkey = Pdg_parameters.register_category "state"

module P = Pdg_parameters
open PdgTypes

exception Cannot_fold

let make loc_info under_outputs =
  { loc_info = loc_info; under_outputs = under_outputs }

let empty = make LocInfo.empty Locations.Zone.bottom
(* Convention: bottom is used for statements that are not reachable,
   and for calls that never terminate. In this case, the second field
   must be ignored *)
let bottom = make LocInfo.bottom Locations.Zone.bottom

let pretty fmt state =
  Format.fprintf fmt "state = %a@.with under_outputs = %a@."
    LocInfo.pretty state.loc_info
    Locations.Zone.pretty state.under_outputs

let add_loc_node state ~exact loc node =
  P.debug ~dkey ~level:2 "add_loc_node (%s) : node %a -> %a@."
      (if exact then "exact" else "merge")
      PdgTypes.Node.pretty node
      Locations.Zone.pretty loc ;
  if LocInfo.is_bottom state.loc_info then
    (* Do not add anything to a bottom state (which comes from an unreachable
       statement *)
    state
  else
    let new_info = NodeSetLattice.inject_singleton node in
    let new_loc_info = LocInfo.add_binding exact state.loc_info loc new_info in
    let new_outputs = (* Zone.link in the under-approx version of Zone.join *)
      if exact then Locations.Zone.link state.under_outputs loc
      else state.under_outputs
    in
    P.debug ~dkey ~level:2 "add_loc_node -> %a" pretty state;
    make new_loc_info new_outputs

(** this one is very similar to [add_loc_node] except that
* we want to accumulate the nodes (exact = false) but nonetheless
* define under_outputs like (exact = true) *)
let add_init_state_input state loc node =
  match loc with
  | Locations.Zone.Top(_p,_o) ->
      (* don't add top because it loses everything*)
      state
  | _ ->
      let new_info = NodeSetLattice.inject_singleton node in
      let new_loc_info =
        LocInfo.add_binding false state.loc_info loc new_info
      in
      let new_outputs = Locations.Zone.link state.under_outputs loc in
      make new_loc_info new_outputs

let test_and_merge ~old new_ =
  if LocInfo.is_included new_.loc_info old.loc_info
  && Locations.Zone.is_included old.under_outputs new_.under_outputs
  then (false, old)
  else
    (* Catch Bottom states, as under_outputs get a special value *)
    if LocInfo.is_bottom old.loc_info then true, new_
    else if LocInfo.is_bottom new_.loc_info then true, old
    else
      let new_loc_info = LocInfo.join old.loc_info new_.loc_info in
      let new_outputs =
        Locations.Zone.meet old.under_outputs new_.under_outputs
      in
      let new_state =
        { loc_info = new_loc_info ; under_outputs = new_outputs }
      in
      true, new_state

(** returns pairs of (n, z_opt) where n is a node that computes a part of [loc]
* and z is the intersection between [loc] and the zone computed by the node.
* @raise Cannot_fold if the state is top (TODO : something better ?)
* *)
let get_loc_nodes_and_part state loc =
  let process z (_default, nodes) acc =
    if Locations.Zone.intersects z loc then
      let z =
        if Locations.Zone.equal loc z
        then Some loc
          (* Be carreful not ot put None here, because if we have n_1 : (s1 =
             s2) and then n_2 : (s1.b = 3) the state looks like :
             s1.a -> n_1; s1.b -> n_2 ; s1.c -> n_1.  And if we
             look for s1.a in that state, we get n_1 but this node
             represent more that s1.a even if it is so in the
             state...  *)
        else Some (Locations.Zone.narrow z loc) in
      let add n acc =
        P.debug ~dkey ~level:2 "get_loc_nodes ->  %a@."
          PdgTypes.Node.pretty_with_part (n,z);
        (n,z)::acc
      in
      NodeSetLattice.fold add nodes acc
    else
      acc
  in
  try LocInfo.fold process state.loc_info []
  with LocInfo.Cannot_fold -> raise Cannot_fold

(** @raise Cannot_fold (see [get_loc_nodes_and_part]) *)
let get_loc_nodes state loc =
  P.debug ~dkey ~level:2 "get_loc_nodes %a@.            in %a@."
    Locations.Zone.pretty loc pretty state ;
  if Locations.Zone.equal loc Locations.Zone.bottom
  then  [], None (* nothing to do *)
  else
    let nodes = get_loc_nodes_and_part state loc in
    let undef_zone = Locations.Zone.diff loc state.under_outputs in
    P.debug ~dkey ~level:2 "get_loc_nodes -> undef = %a@."
      Locations.Zone.pretty undef_zone;
    let undef_zone =
      if (Locations.Zone.equal undef_zone Locations.Zone.bottom) then None
      else Some undef_zone
    in
    nodes, undef_zone

open Cil_datatype

type states = PdgTypes.data_state Stmt.Hashtbl.t

(* Slightly ugly, but should not be a problem unless the sid counter wraps *)
let stmt_init = List.hd Stmt.reprs
let stmt_last = { stmt_init with Cil_types.sid = stmt_init.Cil_types.sid - 1 }

let store_init_state states state = Stmt.Hashtbl.add states stmt_init state
let store_last_state states state = Stmt.Hashtbl.add states stmt_last state

let get_init_state states = Stmt.Hashtbl.find states stmt_init
let get_last_state states = Stmt.Hashtbl.find states stmt_last
let get_stmt_state states stmt = Stmt.Hashtbl.find states stmt
