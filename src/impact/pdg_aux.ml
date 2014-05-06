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

open PdgIndex
open Locations

type node = PdgTypes.Node.t * Zone.t


module NS = struct
  include Hptmap.Make
    (PdgTypes.Node)
    (Locations.Zone)
    (Hptmap.Comp_unused)
    (struct let v = [[]] end)
    (struct let l = [Ast.self] end)

  let intersects =
    let name = "Impact.Pdg_aux.NS.intersects" in
    let z_intersects _ z1 z2 = Locations.Zone.intersects z1 z2 in
    let map_intersects =
      symmetric_binary_predicate (PersistentCache name) ExistentialPredicate
	~decide_fast:decide_fast_intersection
	~decide_one:(fun _ _ -> false)
	~decide_both:z_intersects
    in
    fun s1 s2 -> map_intersects s1 s2

  let inter =
    let decide_some _ z1 z2 =
      let inter = Locations.Zone.narrow z1 z2 in
      if Locations.Zone.is_bottom inter then None
      else Some inter
    in
    let symmetric_inter =
      symmetric_inter ~cache:("Pdg_aux.NS.inter", ()) ~decide_some
    in
    fun m1 m2 ->
      if m1 == m2 then m1
      else symmetric_inter m1 m2

  let union =
    let decide_none _ n = n in
    let decide_some z1 z2 = Zone.join z1 z2 in
    let merge =
      symmetric_merge ~cache:("Pdg_aux.NS.union", ()) ~decide_none ~decide_some
    in
    fun m1 m2 -> merge m1 m2

  let find_default n m =
    try find n m
    with Not_found -> Zone.bottom

  (* We reimplement the following functions to get a Set semantics *)
  let add' (n, z) m =
    let z' = find_default n m in
    let z'' = Zone.join z z' in
    if Zone.equal z z'
    then m
    else add n z'' m

  let mem' (n, z) m =
    let z' = find_default n m in
    Zone.is_included z z'

  let remove' (n, z) m =
    let z' = find_default n m in
    let z'' = Zone.diff z' z in (* TODO: z is not an under-approximation *)
    if Zone.equal z Zone.top || Zone.is_bottom z'' then
      remove n m
    else
      add n z'' m

  let iter' f m =
    iter (fun n z -> f (n, z)) m

  let for_all' f m =
    try
      iter (fun n z -> if not (f (n, z)) then raise Exit) m;
      true
    with Exit -> false

  let diff m1 m2 =
    fold (fun n z acc -> remove' (n, z) acc) m2 m1

  let filter' f =
    map' (fun n z -> if f (n, z) then Some z else None)

  let fold f =
    fold (fun n z -> f (n, z))


  let () = Db.Value.Table.add_hook_on_update (fun _ -> clear_caches ())
end

type call_interface = (PdgTypes.Node.t * NS.t) list

let pretty_node fmt (n, z) =
  if Locations.Zone.equal z Locations.Zone.top then
    PdgTypes.Node.pretty_node fmt n
  else
    let open PdgIndex.Signature in
    let default () =
      Format.fprintf fmt "%a/%a"
        PdgTypes.Node.pretty_node n Locations.Zone.pretty z
    in
    let narrow_by_z = function
      | Out OutRet | In (InCtrl | InNum _) -> default ()
      | In (InImpl z')
      | Out (OutLoc z') ->
        if Locations.Zone.equal z z' then
          PdgTypes.Node.pretty_node fmt n
        else
          default ()
    in
    match PdgTypes.Node.elem_key n with
    | PdgIndex.Key.SigCallKey (_, key)
    | PdgIndex.Key.SigKey key -> narrow_by_z key
    | _ -> default ()


let node_list_to_set ?(z=Zone.top) =
  List.fold_left
    (fun set (n, zopt) ->
      match zopt, z with
      | Some z, _ | None, z -> NS.add' (n, z) set
    )
    NS.empty


(** [find_call_input_nodes pdg_caller s ?z input] find all the nodes of
    [pdg_caller] that define the pdg input [input] above the call statement [s].
    If [input] is an implicit input, its value is refined according to [z]. *)
(*   Copied from pdg/sets.ml, as it is currently not exported *)
let find_call_input_nodes pdg_caller call_stmt ?(z=Locations.Zone.top) in_key =
  match in_key with
  | PdgIndex.Signature.InCtrl
  | PdgIndex.Signature.InNum _ ->
    let idx = PdgTypes.Pdg.get_index pdg_caller in
    let _, call_sgn = FctIndex.find_call idx call_stmt in
    let node = PdgIndex.Signature.find_in_info call_sgn in_key in
    [ node, None ]
  | PdgIndex.Signature.InImpl zone ->
      let zone' = Locations.Zone.narrow zone z in
      (* skip undef zone: any result different from None is due to calldeps or
         some imprecision. *)
      let nodes, _undef = 
        !Db.Pdg.find_location_nodes_at_stmt
          pdg_caller call_stmt ~before:true zone'
      in
      nodes

let all_call_input_nodes ~caller:pdg_caller ~callee:(kf_callee, pdg_callee) call_stmt =
  let real_inputs =
    let inout =
      !Db.Operational_inputs.get_internal_precise ~stmt:call_stmt kf_callee
    in
    inout.Inout_type.over_inputs_if_termination
  in
  let test_in acc (in_key, in_node) =
    let default ?z () =
      let in_nodes = find_call_input_nodes pdg_caller call_stmt ?z in_key in
      let in_nodes = node_list_to_set ?z in_nodes in
      (in_node, in_nodes) :: acc
    in
    match in_key with
      | Signature.InCtrl | Signature.InNum _ -> default ()
      | Signature.InImpl z ->
          if Locations.Zone.intersects z real_inputs
          then default ~z:real_inputs ()
          else acc
  in
  try
    let sgn = FctIndex.sgn (PdgTypes.Pdg.get_index pdg_callee) in
    PdgIndex.Signature.fold_all_inputs test_in [] sgn
  with PdgTypes.Pdg.Top ->
    Options.warning ~source:(fst (Cil_datatype.Stmt.loc call_stmt)) ~once:true
      "skipping impact within imprecisely analyzed function %a"
      Kernel_function.pretty kf_callee;
    []


let all_call_out_nodes ~callee ~caller call_stmt =
  try
    let _, call_sgn =
      FctIndex.find_call (PdgTypes.Pdg.get_index caller) call_stmt
    in
    let test_out acc (out_key, call_out_node) =
      (* skip undef: any zone found undef is due to an imprecision or a bug*)
      let out_nodes, _ = !Db.Pdg.find_output_nodes callee out_key in
      let out_nodes = node_list_to_set out_nodes in
      (call_out_node, out_nodes) :: acc
    in
    PdgIndex.Signature.fold_all_outputs test_out [] call_sgn
  with PdgTypes.Pdg.Top ->
    Options.warning ~source:(fst (Cil_datatype.Stmt.loc call_stmt)) ~once:true
      "cannot propagate impact into imprecisely analyzed caller function %a"
      Kernel_function.pretty (Kernel_function.find_englobing_kf call_stmt);
    []
    
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
