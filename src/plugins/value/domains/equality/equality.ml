(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Type of the keys of the map. *)
module type Element = sig
  include Datatype.S_with_collections
  val id: t -> int (** Identity of a key. Must verify [id k >= 0] and
                       [equal k1 k2 ==> id k1 = id k2] *)
  val self : State.t
  val pretty_debug: t Pretty_utils.formatter
end

module Make
  (Elt : Element)
  (Set : Hptset.S with type elt = Elt.t)
  (Functor_info : Datatype.Functor_info)
  : Equality_sig.S_with_collections with type elt = Elt.t
= struct

  type 'a trivial = 'a Equality_sig.trivial = Trivial | NonTrivial of 'a

  type 'a tree =
    'a Equality_sig.tree = Empty | Leaf of 'a | Node of 'a tree * 'a tree

  module Equality = struct
    include Set

    let name = Functor_info.module_name

    (* cardinality less or equal to 1: not a real equivalence class *)
    let is_trivial s =
      try fold (fun _ acc -> if acc = 0 then 1 else raise Exit) s 0 <= 1
      with Exit -> false

    let return s = if is_trivial s then Trivial else NonTrivial s

    (* TODO: consistency *)
    let pair e1 e2 =
      if Elt.equal e1 e2
      then Trivial
      else NonTrivial (add e2 (singleton e1))

    let remove e s = return (remove e s)

    let inter s s' = return (inter s s')

    let filter f s = return (filter f s)

    let pretty fmt s =
      Pretty_utils.pp_iter ~pre:"@[<hov 3>{" ~sep:"@ =@ " ~suf:"}@]"
        iter (fun fmt a -> Elt.pretty fmt a)
        fmt s

    let pretty_debug = pretty
  end

  include (Equality : Equality_sig.S with type elt = Elt.t
                                      and type t = Equality.t)

  module Set = struct

    module Initial_Values = struct let v = [[]] end
    module Dependencies = struct let l = [ Elt.self ] end

    include Hptmap.Make (Elt) (Equality)
        (Hptmap.Comp_unused) (Initial_Values) (Dependencies)

    let find_option elt map =
      try Some (find elt map)
      with Not_found -> None

    type element = Equality.elt
    type equality = Equality.t

    let contains = mem

    let mem equality map =
      let head = Equality.choose equality in
      match find_option head map with
      | None -> false
      | Some eq -> Equality.subset equality eq

    let subset =
      binary_predicate
        (Hptmap_sig.PersistentCache "equality.set.subset")
        UniversalPredicate
        ~decide_fast:decide_fast_inclusion
        ~decide_fst:(fun _ _ -> false)
        ~decide_snd:(fun _ _ -> true)
        ~decide_both:(fun _ e1 e2 -> Equality.subset e1 e2)

    (* TODO: replace all occurrences of Equality.fold by an heterogeneous
       iteration on the equality and the set of equalities. *)

    let singleton equality =
      Equality.fold (fun elt map -> add elt equality map) equality empty

    let remove elt map =
      match find_option elt map with
      | None -> map
      | Some eq -> match Equality.remove elt eq with
        | Trivial -> Equality.fold (fun e map -> remove e map) eq map
        | NonTrivial eq ->
          let map = Equality.fold (fun e map -> add e eq map) eq map in
          remove elt map

    let add equality map =
      (* Compute the transitive closure of [equality], taking the equalities
         already in [map] into account. *)
      let overall_equality =
        Equality.fold
          (fun elt acc -> match find_option elt map with
             | None -> acc
             | Some eq -> Equality.union eq equality)
          equality equality
      in
      (* map all the elements in this transitive closure to the closure itself*)
      Equality.fold
        (fun elt map -> add elt overall_equality map) overall_equality map

    let unite a b map =
      match Equality.pair a b with
      | Trivial -> map
      | NonTrivial equality -> add equality map

    (* The implementation of this function is buggy. Take e.g.
       [a == b, c == d] and [b == c]. Another function and a fixpoint is
       needed. However, this may not be critical because this only endangers
       the transitive closure of the equality. TODO: check. *)
    let _union =
      join
        ~cache:(Hptmap_sig.PersistentCache "equality.set.union")
        ~symmetric:true
        ~idempotent:true
        ~decide:(fun _key left right -> Equality.union left right)

    let union a b =
      if cardinal a > cardinal b
      then fold (fun _ eq acc -> add eq acc) b a
      else fold (fun _ eq acc -> add eq acc) a b

    let inter =
      let decide _key left right =
        match Equality.inter left right with
        | Trivial -> None
        | NonTrivial eq -> Some eq
      in
      inter
        ~cache:(Hptmap_sig.PersistentCache "equality.set.inter")
        ~symmetric:true
        ~idempotent:true
        ~decide

    let choose map = snd (min_binding map)

    let elements map = fold (fun _ eq acc -> eq :: acc) map []

    (* is representative? *)
    let is_rep elt eq = Elt.equal (Equality.choose eq) elt

    let fold f map acc =
      fold (fun elt eq acc -> if is_rep elt eq then f eq acc else acc) map acc

    let deep_fold f map acc =
      fold (fun eq accu -> Equality.fold (f eq) eq accu) map acc

    let iter f map =
      iter (fun elt eq -> if is_rep elt eq then f eq) map
    let exists f map =
      exists (fun elt eq -> if is_rep elt eq then f eq else false) map
    let for_all f map =
      for_all (fun elt eq -> if is_rep elt eq then f eq else true) map


    let pretty fmt map =
      Pretty_utils.pp_iter ~pre:"@[" ~sep:"@ " ~suf:"@]"
        iter (fun fmt eq -> Format.fprintf fmt "@[%a@]" Equality.pretty eq)
        fmt map

    let keys =
      let cache_name = "Equalities.Set.keys" in 
      let temporary = false in
      let f k _ = Leaf k in
      let joiner t1 t2 = Node (t1, t2) in
      let empty = Empty in
      cached_fold ~cache_name ~temporary ~f ~joiner ~empty

    let elements_only_left =
      let cache = Hptmap_sig.PersistentCache "Equality.Set.only_left" in
      let empty_left _ = Empty (* impossible *) in
      let empty_right t = keys t in
      let both _ _ _ = Empty in
      let join t1 t2 = Node (t1, t2) in
      let empty = Empty in
      let f = fold2_join_heterogeneous
          ~cache ~empty_left ~empty_right ~both ~join ~empty
      in
      fun eqs1 eqs2 -> f eqs1 (shape eqs2)

  end
end
