(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: access_path.ml,v 1.8 2008/04/01 09:25:20 uid568 Exp $ *)
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
  Format.printf "%a" pretty !result;
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
  

let () = 
  Db.Access_path.compute := compute;
  Db.Access_path.filter := filter;
  Db.Access_path.pretty := pretty
   
let option =
  "-access_path",
  Arg.Unit Cmdline.ForceAccessPath.on,
  ": force the access path information to be computed"

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
