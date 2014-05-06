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

open Locations
open Abstract_interp
open Lattice_Interval_Set
open Cvalue

let pretty =
  let module M =
        Base.Map.Make(struct
        include Datatype.Pair(Zone)(Location_Bits)
        let pretty fmt (z, loc) =
          Format.fprintf fmt "@[<hov 1>[Zone:%a@ Loc_bits:%a]@]"
            Zone.pretty z
            Location_Bits.pretty loc
        end)
  in
  fun fmt m ->
    Format.fprintf fmt "Access_path:@\n%a@\n=============@\n" M.pretty m


(** [reciprocal_image b m] is the set of bits in the offsetmap [m]
      that may lead to Top([b]) and  the set of offsets in [m]
      where one can read an address [b]+_ *)
let reciprocal_image_offsm base m =
  let treat_binding (bi,ei as itv) (v, modu, r) (acc1,acc2) =
    let r = Integer.c_rem (Rel.add_abs bi r) modu in
    let v = Cvalue.V_Or_Uninitialized.get_v v in
    let acc1 = if Locations.Location_Bytes.may_reach base v
      then Int_Intervals.join acc1 (Int_Intervals.inject [itv])
      else acc1
    in
    let acc2 =
      if (Locations.Location_Bytes.intersects
            (Locations.Location_Bytes.inject base Ival.top)
            v)
        && Int.compare modu (Integer.of_int (Bit_utils.sizeofpointer ())) = 0
      then
        let first = Int.round_up_to_r ~min:bi ~r ~modu in
        let last =
          Integer.mul
            (Integer.pred (Integer.div (Integer.succ (Integer.sub ei first)) modu))
            modu
        in
        if Integer.lt last Integer.zero then acc2
        else
          Ival.join
            acc2
            (Ival.inject_top (Some first) (Some (Integer.add first last)) r modu)
      else acc2
    in
    acc1,acc2
  in
  Cvalue.V_Offsetmap.fold treat_binding m (Int_Intervals.bottom, Ival.bottom)

(** [reciprocal_image m b] is the set of bits in the map [m] that may lead
    to Top([b]) and  the location in [m] where one may read an address [b]+_ *)

let reciprocal_image base m : Zone.t*Location_Bits.t =
  if Base.is_null base then Zone.top,Location_Bits.top
  else
    Model.fold_base_offsetmap
      (fun b offsm (acc1,acc2) ->
        let interv_set,ival = reciprocal_image_offsm base offsm in
        let acc1 = Zone.join acc1 (Zone.inject b interv_set) in
        let acc2 = Location_Bits.join acc2 (Location_Bits.inject b ival) in
        acc1,acc2
      ) m (Zone.bottom,Location_Bits.bottom)

let compute state base_set =
  let q = Queue.create () in
  let result = ref Base.Map.empty in
  Base.Set.iter (fun elt -> Queue.add elt q) base_set;
  while not (Queue.is_empty q) do
    let current_base = Queue.take q in
    let recip = reciprocal_image current_base state in
    result := Base.Map.add current_base recip !result ;
    try
      Zone.fold_bases
        (fun base () ->
          try ignore (Base.Map.find base !result)
          with Not_found -> Queue.add base q)
        (fst recip)
        ()
    with Zone.Error_Top -> ()
  done;
  Inout_parameters.result "%a" pretty !result;
  !result

let filter m inputs =
  Base.Map.map
    (fun (zone,loc) ->
      Zone.narrow zone inputs,
      (Locations.filter_loc
         (Locations.make_loc
            loc
            (Int_Base.inject (Int.of_int (Bit_utils.sizeofpointer ()))))
         inputs).Locations.loc)
    m

let main () =
  if Inout_parameters.ForceAccessPath.get () then
    !Db.Semantic_Callgraph.topologically_iter_on_functions
      (fun kf ->
         if Kernel_function.is_definition kf && !Db.Value.is_called kf then
           let state =
             Db.Value.get_state
               (Cil_types.Kstmt (Kernel_function.find_first_stmt kf))
           in
           let inputs = !Db.Operational_inputs.get_internal kf in
           let s = !Db.Access_path.compute state
             (Cvalue.Model.fold_base
                (fun base acc -> Base.Set.add base acc)
		state
                Base.Set.empty)
           in
           Inout_parameters.result
             "Filtered access_path for %a :@ %a@."
             Kernel_function.pretty kf
             !Db.Access_path.pretty
             (!Db.Access_path.filter s
                (Locations.Zone.filter_base
                   (fun b ->
                      not (Base.is_local b (Kernel_function.get_definition kf)))
                   inputs.Inout_type.over_inputs)))

let () = Db.Main.extend main

let () =
  Db.Access_path.compute := compute;
  Db.Access_path.filter := filter;
  Db.Access_path.pretty := pretty

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
