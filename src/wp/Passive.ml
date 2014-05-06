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
(* --- Passive Forms                                                      --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

type binding = 
  | B1 of var * pred
  | B2 of var * var * pred
type t = binding list

let empty = []
let union = List.append

let bind ~fresh ~bound bs = 
  B1(bound , p_equal (e_var fresh) (e_var bound)) :: bs

let join x y bs =
  if Var.equal x y then bs else B2(x,y,p_equal (e_var x) (e_var y)) :: bs

let rec collect phi hs = function
  | [] -> hs
  | B1(x,eq)::bs -> collect phi (if phi x then eq :: hs else hs) bs
  | B2(x,y,eq)::bs -> collect phi (if phi x || phi y then eq :: hs else hs) bs

let apply bindings p = 
  let xs = varsp p in
  let hs = collect (fun x -> Vars.mem x xs) [] bindings in
  p_conj (p::hs)

let conditions bindings phi = collect phi [] bindings

let pretty fmt =
  List.iter
    begin function
      | B1(x,p) -> 
	  Format.fprintf fmt "@ @[([%a] %a)@]"
	    F.pp_var x F.pp_pred p
      | B2(x,y,p) ->
	  Format.fprintf fmt "@ @[([%a,%a] %a)@]"
	    F.pp_var x F.pp_var y F.pp_pred p
    end
