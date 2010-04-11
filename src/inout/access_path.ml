(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(* $Id: access_path.ml,v 1.9 2008-11-06 13:03:28 uid568 Exp $ *)
open Locations
open Abstract_interp
open BaseUtils

let pretty fmt m = 
  Format.fprintf fmt 
    "Access_path:@\n%a@\n=============@\n" 
    (BaseMap.pretty 
       (fun fmt (z,loc) -> Format.fprintf fmt "@[<hov 1>[Zone:%a@ Loc_bits:%a]@]" 
          Zone.pretty z 
          Location_Bits.pretty loc))
    m

let compute state base_set = 
  let state = Relations_type.Model.value_state state in
  let q = Queue.create () in
  let result = ref BaseMap.empty in
  BaseSet.iter (fun elt -> Queue.add elt q) base_set;
  while not (Queue.is_empty q) do
    let current_base = Queue.take q in
    let recip = Cvalue_type.Model.reciprocal_image current_base state in
    result := BaseMap.add current_base recip !result ;
    try 
      Zone.fold_bases 
        (fun base () -> try ignore (BaseMap.find base !result)
         with Not_found -> Queue.add base q)
        (fst recip) 
        ()
    with Zone.Error_Top -> ()
  done;
  Inout_parameters.result "%a" pretty !result;
  !result

let filter m inputs =
  BaseMap.map
    (fun (zone,loc) -> (Zone.narrow zone inputs),
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
	   let inputs = !Db.InOutContext.get_internal kf in
	   let s = !Db.Access_path.compute state
	     (Cvalue_type.Model.fold_base
		(fun base acc -> BaseUtils.BaseSet.add base acc)
		(Relations_type.Model.value_state state)
		BaseUtils.BaseSet.empty)
	   in
	   Inout_parameters.result
	     "Filtered access_path for %a :@ %a@."
	     Kernel_function.pretty_name kf
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
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
