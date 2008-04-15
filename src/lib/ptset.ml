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

(* $Id: ptset.ml,v 1.7 2008/11/04 10:05:05 uid568 Exp $ *)

module type S = sig
  type elt
  type t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
(*  val compare: t -> t -> int*)
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (*  val for_all: (elt -> bool) -> t -> bool *)
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
(*  val partition: (elt -> bool) -> t -> t * t*)
  val cardinal: t -> int
(*  val min_elt: t -> elt
  val max_elt: t -> elt
  val choose: t -> elt
  val split: elt -> t -> t * bool * t*)
end

module Make
  (X: sig
     type t 
     val id: t -> int  
     val name : string
     val pretty : Format.formatter -> t -> unit
   end) = struct

  include
    Ptmap.Generic
      (X)
      (struct 
	 type t = unit 
	 let tag () = 0
	 let equal () () = true
	 let pretty fmt () = Format.fprintf fmt "()"
	 module Datatype = Datatype.Unit
       end)
      (struct let v = [] end)

  type elt = X.t

  external getperfcount : unit -> int = "getperfcount"

  let time name f =
    let cpt = ref 0 in
    fun x ->
      let b = getperfcount () in
      let res = f x in
      let e = getperfcount () in
      let diff = e - b in
      cpt := !cpt + diff;
      Format.eprintf "timing of %s (in %s): %d (%d)@." 
        name Datatype.name !cpt diff;
      res

  let time2 name f =
    let cpt = ref 0 in
    fun x y ->
      let b = getperfcount () in
      let res = f x y in
      let e = getperfcount () in
      let diff = e - b in
      cpt := !cpt + diff;
      Format.eprintf "timing of %s (in %s): %d (%d)@." 
        name Datatype.name !cpt diff;
      res

  let add k = add k ()
  let iter f = iter (fun x () -> f x)
  let fold f = fold (fun x () -> f x)

  let filter f s = fold (fun x acc -> if f x then add x acc else acc) s empty

  let mem x s = try find x s; true with Not_found -> false

  let diff s1 s2 = 
    fold (fun x acc -> if mem x s2 then acc else add x acc) s1 empty

  let inter s1 s2 = 
    fold (fun x acc -> if mem x s1 then add x acc else acc) s2 empty
(*  let inter = time2 "inter" inter*)

  let union = 
    generic_merge ~cache:("Ptset.union", 12) ~decide:(fun _k  _ _ -> ())
(*  let union = time2 "union" union*)

  let singleton x = add x empty

  let exists f s = 
    try
      iter (fun x -> if f x then raise Exit) s;
      false
    with Exit ->
      true

  let subset s1 s2 = not (exists (fun x -> not (mem x s2)) s1)
(*  let subset = time2 "subset" subset*)

  let cardinal s = fold (fun _ acc -> acc + 1) s 0
(*  let cardinal = time "cardinal" cardinal*)

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
