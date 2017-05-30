(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
(* --- Separation Hypotheses                                              --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Cil_datatype

type region =
  | Var of varinfo   (* &x     - the cell x *)
  | Ptr of varinfo   (* p      - the cell pointed by p *)
  | Arr of varinfo   (* p+(..) - the cell and its neighbors pointed by p *)

let pp_region fmt = function
  | Var vi  -> Format.fprintf fmt "&%a" Varinfo.pretty vi
  | Ptr vi  -> Varinfo.pretty fmt vi
  | Arr vi -> Format.fprintf fmt "%a+(..)" Varinfo.pretty vi

(* interpreted as \separated(mutex, ... ,\union( other, ... )) *)
type clause = {
  mutex : region list ;
  other : region list ;
}

let is_true = function
  | { mutex = [] }
  | { mutex = [ _ ] ; other = [] } -> true
  | _ -> false

let requires = List.filter (fun s -> not (is_true s))

let pp_clause fmt s =
  match s with
  | { mutex = [] }
  | { mutex = [ _ ] ; other = [] } -> Format.pp_print_string fmt "\\true"
  | { mutex = (m::ms) ; other = [] } ->
      begin
        Format.fprintf fmt "@[<hov 2>\\separated(%a" pp_region m ;
        List.iter (fun r -> Format.fprintf fmt ",@,%a" pp_region r) ms ;
        Format.fprintf fmt ")@]" ;
      end
  | { mutex ; other = r::rs } ->
      begin
        Format.fprintf fmt "@[<hov 2>\\separated(" ;
        List.iter (fun r -> Format.fprintf fmt "%a,@," pp_region r) mutex ;
        if rs = [] then pp_region fmt r else
          begin
            Format.fprintf fmt "@[<hov 2>\\union(%a" pp_region r ;
            List.iter (fun r -> Format.fprintf fmt ",@,%a" pp_region r) rs ;
            Format.fprintf fmt ")@]" ;
          end ;
        Format.fprintf fmt ")@]" ;
      end
