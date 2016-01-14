(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
(* --- Upper approximated Set of (un-)bound variables                     --- *)
(* -------------------------------------------------------------------------- *)

type t = {
  lower : int ; (* lower bound of variables, or 0 if empty *)
  upper : int ; (* upper bound of variables +1, or 0 is empty *)
  order : int ; (* depth of binders inside *)
}

let empty = { lower=0 ; upper=0 ; order=0 }

let is_empty a = (a.upper = 0)
let closed s = s.upper <= s.order
let closed_at d s = s.upper = 0 || d <= s.lower

let union a b =
  if is_empty a then b else
  if is_empty b then a else
    {
      lower = min a.lower b.lower ;
      order = max a.order b.order ;
      upper = max a.upper b.upper ;
    }

let singleton k = {
  order = 0 ;
  lower = k ;
  upper = k+1 ;
}

let contains k s = s.lower <= k && k < s.upper
let overlap k n s = s.lower < k+n && k < s.upper

let order s = s.order
let bind s = {
  upper = s.upper ;
  lower = s.lower ;
  order = succ s.order ;
}

let pretty fmt s =
  if is_empty s then
    Format.fprintf fmt "<empty>"
  else
    Format.fprintf fmt "\\%d.[%d-%d]" s.order s.lower (s.upper - 1)
