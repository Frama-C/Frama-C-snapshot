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
(* --- Generic Sigma Factory                                              --- *)
(* -------------------------------------------------------------------------- *)

open Lang.F

module Make
  (C : Memory.Chunk)
  (H : Qed.Collection.S with type t = C.t) :
  Memory.Sigma with type chunk = C.t and type domain = H.set =
struct

  type chunk = C.t
  type domain = H.set

  type t = { id : int ; mutable map : var H.map }

  let id = ref 0 (* for debugging purpose *)
  let build map = let k = !id in incr id ; { id = k ; map = map }
  let create () = build H.Map.empty
  let copy s = build s.map

  let newchunk c = 
    Lang.freshvar ~basename:(C.basename_of_chunk c) (C.tau_of_chunk c)

  let merge a b =
    let pa = ref Passive.empty in
    let pb = ref Passive.empty in
    let merge_chunk c x y =
      if Var.equal x y then x else
	let z = newchunk c in
	pa := Passive.bind ~fresh:z ~bound:x !pa ;
	pb := Passive.bind ~fresh:z ~bound:y !pb ;
	z in
    let w = H.Map.union merge_chunk a.map b.map in
    build w , !pa , !pb

  let get w c =
    try H.Map.find c w.map
    with Not_found ->
      let x = newchunk c in
      w.map <- H.Map.add c x w.map ; x

  let mem w c = H.Map.mem c w.map    

  let join a b =
    let p = ref Passive.empty in
    H.Map.iter2
      (fun chunk x y ->
	 match x,y with
	   | Some x , Some y -> p := Passive.join x y !p
	   | Some x , None -> b.map <- H.Map.add chunk x b.map
	   | None , Some y -> a.map <- H.Map.add chunk y a.map
	   | None , None -> ())
      a.map b.map ; !p

  let assigned a b written =
    let p = ref Bag.empty in
    H.Map.iter2
      (fun chunk x y ->
	 if not (H.Set.mem chunk written) then
	   match x,y with
	     | Some x , Some y when x != y -> 
		 p := Bag.add (p_equal (e_var x) (e_var y)) !p
	     | Some x , None -> b.map <- H.Map.add chunk x b.map
	     | None , Some y -> a.map <- H.Map.add chunk y a.map
	     | _ -> ())
      a.map b.map ; !p

  let value w c = e_var (get w c)

  let iter f w = H.Map.iter f w.map
  let iter2 f w1 w2 = H.Map.iter2 f w1.map w2.map

  let havoc w xs =
    let ys = H.Set.mapping newchunk xs in
    build (H.Map.union (fun _c _old y -> y) w.map ys)

  let havoc_chunk w c =
    let x = newchunk c in
    build (H.Map.add c x w.map)

  let havoc_any ~call w = 
    let frame = 
      if call 
      then H.Map.filter (fun c _ -> C.is_framed c) w.map
      else H.Map.empty
    in build frame
      
  let domain w = H.Map.domain w.map

  let pretty fmt w =
    Format.fprintf fmt "@@%s%d" C.self w.id ;
    H.Map.iter 
      (fun c x -> Format.fprintf fmt "@ %a:%a" C.pretty c Var.pretty x) w.map

end
