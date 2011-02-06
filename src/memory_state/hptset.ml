(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

module type S = sig
  type elt
  include Datatype.S
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val elements: t -> elt list
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool 
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
(*  val partition: (elt -> bool) -> t -> t * t*)
  val cardinal: t -> int
  val min_elt: t -> elt
(*  val max_elt: t -> elt
*)
  val contains_single_elt: t -> elt option
(*  val choose: t -> elt
  val split: elt -> t -> t * bool * t*)
end

module type Id_Datatype = sig
  include Datatype.S
  val id: t -> int
end

module Make(X: Id_Datatype) = struct

  include
    Hptmap.Make
    (X)
    (struct include Datatype.Unit let tag () = 0 end)
    (Hptmap.Comp_unused)
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
      Format.eprintf "timing of %s: %d (%d)@." name !cpt diff;
      res

  let time2 name f =
    let cpt = ref 0 in
    fun x y ->
      let b = getperfcount () in
      let res = f x y in
      let e = getperfcount () in
      let diff = e - b in
      cpt := !cpt + diff;
      Format.eprintf "timing of %s: %d (%d)@." name !cpt diff;
      res

  let add k = add k ()
  let iter f = iter (fun x () -> f x)
  let fold f = fold (fun x () -> f x)

  let elements s = fold (fun h t -> h::t) s []

  let contains_single_elt s =
    match is_singleton s with
      Some (k, _v) -> Some k
    | None -> None

  let min_elt s =
    fst (min_binding s)

  let filter f s = fold (fun x acc -> if f x then add x acc else acc) s empty

  let mem x s = try find x s; true with Not_found -> false

  let diff s1 s2 =
    fold (fun x acc -> if mem x s2 then acc else add x acc) s1 empty

  let inter s1 s2 =
    fold (fun x acc -> if mem x s1 then add x acc else acc) s2 empty
(*  let inter = time2 "inter" inter*)

  let binary_unit _ _ = ()

  let union =
    symetric_merge 
      ~cache:("Hptset.union", 12) 
      ~decide_none:binary_unit
      ~decide_some:binary_unit

(*    generic_merge ~cache:("Hptset.union", 12) ~decide:(fun _ _ _ -> ()) *)
(*  let union = time2 "union" union*)

  let singleton x = add x empty

  exception Elt_found

  let exists f s =
    try
      iter (fun x -> if f x then raise Elt_found) s;
      false
    with Elt_found ->
      true

  let for_all f s =
    try
      iter (fun x -> if not (f x) then raise Elt_found) s;
      true
    with Elt_found ->
      false

  (* completely sub-optimal: subset should be divide-and-conquer *)
  let subset s1 s2 = not (exists (fun x -> not (mem x s2)) s1)
(*  let subset = time2 "subset" subset*)

  let cardinal s = fold (fun _ acc -> acc + 1) s 0
(*  let cardinal = time "cardinal" cardinal*)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
