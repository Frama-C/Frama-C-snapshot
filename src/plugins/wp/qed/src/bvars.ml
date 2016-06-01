(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
