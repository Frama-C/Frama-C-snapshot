(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

    type action = Neutral | Absorbing | Traversing of (elt -> bool)

    val merge :
      cache:Hptmap_sig.cache_type ->
      symmetric:bool ->
      idempotent:bool ->
      decide_both:(elt -> bool) ->
      decide_left:action ->
      decide_right:action ->
      t -> t -> t

    type 'a shape
    val shape: t -> unit shape
    val from_shape: 'a shape -> t

    val fold2_join_heterogeneous:
      cache:Hptmap_sig.cache_type ->
      empty_left:('a shape -> 'b) ->
      empty_right:(t -> 'b) ->
      both:(elt -> 'a -> 'b) ->
      join:('b -> 'b -> 'b) ->
      empty:'b ->
      t -> 'a shape ->
      'b

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

  module M =
    Hptmap.Make
    (X)
    (struct include Datatype.Unit let pretty_debug = pretty end)
    (Hptmap.Comp_unused)
    (struct let v = List.map (List.map (fun k -> k, ())) Initial_Values.v end)
    (Datatype_deps)

  include M

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

  let inter =
    let name = Format.sprintf "Hptset(%s).inter" X.name in
    inter
      ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:true
      ~idempotent:true
      ~decide:(fun _ () () -> Some ())

  (* Test that implementation of function inter in Hptmap is correct *)
  let _test_inter s1 s2 =
    let i1 =
      fold (fun x acc -> if mem x s1 then add x acc else acc) s2 empty
    in
    let i2 = inter s1 s2 in
    if not (i1 == i2) then
      Cmdline.Kernel_log.error "%a@./@.%a@.->@.%a@./@.%a"
        pretty_debug s1 pretty_debug s2 pretty_debug i1 pretty_debug i2;
    i1

  let union =
    let name = Format.sprintf "Hptset(%s).union" X.name in
    join ~cache:(Hptmap_sig.PersistentCache name) ~decide:(fun _ () () -> ())
      ~symmetric:true ~idempotent:true

  let singleton x = add x empty

  let exists f s = exists (fun k () -> f k) s

  let for_all f s = for_all (fun k () -> f k) s

  let subset =
    let name = Format.sprintf "Hptset(%s).subset" X.name in
    binary_predicate (Hptmap_sig.PersistentCache name) UniversalPredicate
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
    symmetric_binary_predicate
      (Hptmap_sig.PersistentCache name)
      ExistentialPredicate
      ~decide_fast:decide_fast_intersection
      ~decide_one:(fun _ () -> false)
      ~decide_both:(fun _ () () -> true)

  let of_list l = List.fold_left (fun acc key -> add key acc) empty l

  type action = Neutral | Absorbing | Traversing of (elt -> bool)

  let translate_action = function
    | Neutral -> M.Neutral
    | Absorbing -> M.Absorbing
    | Traversing f -> M.Traversing (fun k () -> if f k then Some () else None)

  let merge ~cache ~symmetric ~idempotent
      ~decide_both ~decide_left ~decide_right =
    let decide_both = fun k () () -> if decide_both k then Some () else None
    and decide_left = translate_action decide_left
    and decide_right = translate_action decide_right in
    merge ~cache ~symmetric ~idempotent
      ~decide_both ~decide_left ~decide_right

  let diff =
    let name = Format.sprintf "Hptset(%s).diff" X.name in
    merge
      ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:false
      ~idempotent:false
      ~decide_both:(fun _ -> false)
      ~decide_left:Neutral
      ~decide_right:Absorbing

  let from_shape m = from_shape (fun _ _ -> ()) m

  (* Partial application is needed becauses of caches *)
  let fold2_join_heterogeneous ~cache ~empty_left ~empty_right ~both ~join ~empty =
    let both k () v = both k v in
    fold2_join_heterogeneous ~cache ~empty_left ~empty_right ~both ~join ~empty

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
