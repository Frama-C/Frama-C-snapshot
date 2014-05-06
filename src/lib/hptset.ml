(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
  include Datatype.S_with_collections
  include FCSet.S_Basic_Compare with type t := t

    val contains_single_elt: t -> elt option
    val intersects: t -> t -> bool

    type 'a shape
    val shape: t -> unit shape
    val from_shape: 'a shape -> t

    val clear_caches: unit -> unit
end

module Make(X: Hptmap.Id_Datatype)
  (Initial_Values : sig val v : X.t list list end)
  (Datatype_deps: sig val l : State.t list end) :   sig
    include S with type elt = X.t
              and type 'a shape = 'a Hptmap.Shape(X).t
    val self : State.t
  end
  = struct

  type elt = X.t
  type 'a shape = 'a Hptmap.Shape(X).t

  include
    Hptmap.Make
    (X)
    (Datatype.Unit)
    (Hptmap.Comp_unused)
    (struct let v = List.map (List.map (fun k -> k, ())) Initial_Values.v end)
    (Datatype_deps)

  let add k s = add k () s
  let iter f s = iter (fun x () -> f x) s
  let fold f s = fold (fun x () -> f x) s

  let elements s = fold (fun h t -> h::t) s []

  let contains_single_elt s =
    match is_singleton s with
      Some (k, _v) -> Some k
    | None -> None

  let choose s = fst (min_binding s)

  let filter f s = fold (fun x acc -> if f x then add x acc else acc) s empty

  let partition f s =
    fold
      (fun x (w, wo) -> if f x then add x w, wo else w, add x wo) s (empty, empty)

  let mem x s = try find x s; true with Not_found -> false

  let find x s = find_key x s

  let diff s1 s2 =
    fold (fun x acc -> if mem x s2 then acc else add x acc) s1 empty

  let inter =
    let name = Format.sprintf "Hptset(%s).inter" X.name in
    let res = Some () in
    let aux =
      symmetric_inter ~cache:(name, ()) ~decide_some:(fun _ () () -> res)
    in
    fun m1 m2 -> aux m1 m2

  (* Test that implementation of function inter in Hptmap is correct *)
  let _test_inter s1 s2 =
    let i1 =
      fold (fun x acc -> if mem x s1 then add x acc else acc) s2 empty
    in
    let i2 = inter s1 s2 in
    if not (i1 == i2) then
      Kernel.error "%a@./@.%a@.->@.%a@./@.%a"
        pretty_debug s1 pretty_debug s2 pretty_debug i1 pretty_debug i2;
    i1

  let union =
    let name = Format.sprintf "Hptset(%s).union" X.name in
    symmetric_merge
      ~cache:(name, ())
      ~decide_none:(fun _k () -> ())
      ~decide_some:(fun () () -> ())

  let singleton x = add x empty

  let exists f s = exists (fun k () -> f k) s

  let for_all f s = for_all (fun k () -> f k) s

  let subset =
    let name = Format.sprintf "Hptset(%s).subset" X.name in
    binary_predicate (PersistentCache name) UniversalPredicate
      ~decide_fast:decide_fast_inclusion
      ~decide_fst:(fun _ () -> false)
      ~decide_snd:(fun _ () -> true)
      ~decide_both:(fun _ () () -> true)

  let pretty =
    if X.pretty == Datatype.undefined then
      Datatype.undefined
    else
      Pretty_utils.pp_iter
        ~pre:"@[<hov 1>{" ~sep:",@ " ~suf:"}@]" iter X.pretty

  let split key t =
    let l, pres, r = split key t in
    l, pres <> None, r

  let intersects =
    let name = Pretty_utils.sfprintf "Hptset(%s).intersects" X.name in
    symmetric_binary_predicate (PersistentCache name) ExistentialPredicate
      ~decide_fast:decide_fast_intersection
      ~decide_one:(fun _ () -> false)
      ~decide_both:(fun _ () () -> true)

  let of_list l = List.fold_left (fun acc key -> add key acc) empty l

  let from_shape m = from_shape (fun _ _ -> ()) m

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
