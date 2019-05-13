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
(* --- Variable Partitionning                                             --- *)
(* -------------------------------------------------------------------------- *)

type param = NotUsed | ByAddr | ByValue | ByShift | ByRef | InContext | InArray

let pp_param fmt = function
  | NotUsed -> Format.pp_print_string fmt "not used"
  | ByAddr -> Format.pp_print_string fmt "in heap"
  | ByValue -> Format.pp_print_string fmt "by value"
  | ByShift -> Format.pp_print_string fmt "by value with shift"
  | ByRef -> Format.pp_print_string fmt "by ref."
  | InContext -> Format.pp_print_string fmt "in context"
  | InArray -> Format.pp_print_string fmt "in array"

(* -------------------------------------------------------------------------- *)
(* --- Separation Hypotheses                                              --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype

type zone =
  | Var of varinfo   (* &x     - the cell x *)
  | Ptr of varinfo   (* p      - the cell pointed by p *)
  | Arr of varinfo   (* p+(..) - the cell and its neighbors pointed by p *)

type partition = {
  globals : zone list ; (* [ &G , G[...], ... ] *)
  to_heap : zone list ; (* [ p, ... ] *)
  context : zone list ; (* [ p+(..), ... ] *)
}

type clause = Valid of zone | Separated of zone list list

(* -------------------------------------------------------------------------- *)

let is_separated_true = function [] | [_] -> true | _ -> false

(* -------------------------------------------------------------------------- *)
let pp_zone fmt = function
  | Arr vi -> Format.fprintf fmt "%a+(..)" Varinfo.pretty vi
  | Ptr vi -> Varinfo.pretty fmt vi
  | Var vi -> Format.fprintf fmt "&%a" Varinfo.pretty vi

let pp_region fmt = function
  | [] -> Format.pp_print_string fmt "\\empty"
  | [z] -> pp_zone fmt z
  | z::zs ->
      Format.fprintf fmt "@[<hov 2>\\union(%a" pp_zone z ;
      List.iter (fun z -> Format.fprintf fmt ",@,%a" pp_zone z) zs ;
      Format.fprintf fmt ")@]"

let pp_separation fmt = function
  | [] | [_]  -> Format.pp_print_string fmt "\\true"
  | r::rs ->
      Format.fprintf fmt "@[<hov 2>\\separated(%a" pp_region r ;
      List.iter (fun r -> Format.fprintf fmt ",@,%a" pp_region r) rs ;
      Format.fprintf fmt ")@]"

let pp_clause fmt = function
  | Separated sep -> Format.fprintf fmt "@ @[<hov 2>requires %a;@]" pp_separation sep
  | Valid zone -> Format.fprintf fmt "@ @[<hov 2>requires \\valid(%a);@]" pp_zone zone

(* -------------------------------------------------------------------------- *)
(* --- Memory Context                                                     --- *)
(* -------------------------------------------------------------------------- *)
let add_region r s = if r = [] then s else r::s

let separated partition =
  List.rev @@
  add_region (List.rev partition.to_heap) @@
  add_region (List.rev partition.globals) @@
  List.map (fun z -> [z]) partition.context

let validity partition =
  List.rev @@ List.map (fun z -> Valid z) partition.context

let requires partition =
  let s = separated partition in
  let v = validity partition in
  if not (is_separated_true s) then Separated s :: v else v

(* -------------------------------------------------------------------------- *)
(* --- Partition                                                          --- *)
(* -------------------------------------------------------------------------- *)

let empty = {
  globals = [] ;
  context = [] ;
  to_heap = [] ;
}

let set x p w =
  match p with
  | NotUsed -> w
  | ByAddr -> w
  | ByRef | InContext ->
      if Cil.isFunctionType x.vtype then w else
        { w with context = Ptr x :: w.context }
  | InArray ->
      if Cil.isFunctionType x.vtype then w else
        { w with context = Arr x :: w.context }
  | ByValue | ByShift ->
      if x.vghost then w else
      if Cil.isFunctionType x.vtype then w else
      if x.vglob && (x.vstorage <> Static || x.vaddrof) then
        let z = if Cil.isArrayType x.vtype then Arr x else Var x in
        { w with globals = z :: w.globals }
      else
      if x.vformal && Cil.isPointerType x.vtype then
        let z = if p = ByShift then Arr x else Ptr x in
        { w with to_heap = z :: w.to_heap }
      else w

(* -------------------------------------------------------------------------- *)
