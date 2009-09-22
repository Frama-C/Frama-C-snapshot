(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

open Abstract_interp
open Abstract_value

(* Locates (b2, e2) with respect to (b1, e1).
   Therefore the meaning of "Above" and "Below" may look as if it
   is reversed, beware. *)
let fuzzy_order (b1,e1) (b2, e2) =
  if Int.lt e1 b2
  then Rangemap.Above
  else if Int.lt e2 b1
  then Rangemap.Below
  else Rangemap.Match


exception Cannot_compare_intervals

type t = Int.t * Int.t
let compare x y =
  match fuzzy_order x y with
    Rangemap.Above -> -1
  | Rangemap.Below -> 1
  | Rangemap.Match ->
      if Int.eq (fst x) (fst y) &&
        Int.eq (snd x) (snd y)
      then 0
      else begin
	  (*Format.printf "Comparaison d'intervalles non comparables [%a..%a] et [%a..%a]@\n@\n"
	    Int.pretty (fst x) Int.pretty (snd x)
            Int.pretty (fst y) Int.pretty (snd y);*)
	  raise Cannot_compare_intervals
	end

let hash (x, y) = Int.hash x + 7 * Int.hash y
	
let shift s (b,e) =
  Int.add b s, Int.add e s


let check_coverage (bi,ei) concerned =
  ( match concerned with
        [] -> raise Is_not_included
      | ((_bj,ej),_) :: _ ->
	  if Int.gt ei ej then raise Is_not_included);
  let rec check_joint concerned =
    match concerned with
	[] -> assert false
      | [(bj,_ej),_] ->
	  if Int.lt bi bj then raise Is_not_included
      | ((bj,_ej),_) :: ((((_bk,ek),_)::_) as tail) ->
	  if Int.neq bj (Int.succ ek) then raise Is_not_included;
	  check_joint tail
  in
  check_joint concerned


let clip_itv (refb1,refe1) (b2,e2) =
  assert (Int.le b2 refe1 && Int.ge e2 refb1); 
  (* the 2 is a concerned_interval of the ref *)
  let min = Int.max refb1 b2 in
  let max = Int.min refe1 e2 in
  min,max
    
