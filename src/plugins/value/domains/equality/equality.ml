(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

  module Equality = struct
    include Set

    let name = Functor_info.module_name

    let return s = if cardinal s > 1 then `Value s else `Void

    let cardinal s = cardinal s - 1

    (* TODO: consistency *)
    let pair e1 e2 =
      if Elt.equal e1 e2
      then `Void
      else return (add e2 (singleton e1))

    let remove e s = return (remove e s)

    let inter s s' = return (inter s s')

    let intersect set1 set2 = exists (fun elt -> mem elt set2) set1

    let filter f s = return (filter f s)

    let subst f set =
      let process x result = match f x with
        | Some r -> add r result
        | None -> result
      in
      return (fold process set empty)

    let pretty fmt s =
      Pretty_utils.pp_iter ~pre:"@[{" ~sep:"@ =@ " ~suf:"}@]"
        iter (fun fmt a -> Elt.pretty fmt a)
        fmt s

    let pretty_debug = pretty
  end

  include (Equality : Equality_sig.S with type elt = Elt.t
                                      and type t = Equality.t)

  module Set = struct

    module Initial_Values = struct let v = [[]] end
    module Dependencies = struct let l = [ Elt.self ] end

    module Union = struct
      include
        Hptmap.Make (Elt) (Equality)
          (Hptmap.Comp_unused) (Initial_Values) (Dependencies)

      let find rep union =
        try find rep union
        with Not_found ->
          Value_parameters.warning
            "Representing %a not found in the equalities; return void equality."
            Elt.pretty rep;
          Equality.empty
    end

    module Find = struct
      include Hptmap.Make (Elt) (Elt)
          (Hptmap.Comp_unused) (Initial_Values) (Dependencies)

      let find_option elt map =
        try Some (find elt map)
        with Not_found -> None
    end

    include Datatype.Pair (Union) (Find)

    type element = Equality.elt
    type equality = Equality.t

    let pretty fmt (union, _) =
      Pretty_utils.pp_iter ~pre:"@[" ~sep:"@ " ~suf:"@]"
        (fun f union -> Union.iter (fun _ eq -> f eq) union)
        (fun fmt eq -> Format.fprintf fmt "@[%a@]" Equality.pretty eq)
        fmt union

    let empty = (Union.empty, Find.empty)
    let is_empty (union, _) = Union.is_empty union

    let equal (u1, _) (u2, _) = Union.equal u1 u2
    let compare (u1, _) (u2, _) = Union.compare u1 u2


    let fold f (union, _) accu =
      Union.fold (fun _ eq accu -> f eq accu) union accu

    let deep_fold f unfi accu =
      fold (fun eq accu -> Equality.fold (f eq) eq accu) unfi accu

    let iter f (union, _) = Union.iter (fun _ eq -> f eq) union
    let exists f (union, _) = Union.exists (fun _ eq -> f eq) union
    let for_all f (union, _) = Union.for_all (fun _ eq -> f eq) union


    let find_option elt (union, find) =
      try
        let rep = Find.find elt find in
        Some (Union.find rep union)
      with
        | Not_found -> None

    let find elt (union, find) =
      let rep = Find.find elt find in Union.find rep union

    let contains elt (_, find) = Find.mem elt find

    let mem equality (union, find) =
      try
        let head = Equality.choose equality in
        let rep = Find.find head find in
        Equality.subset equality (Union.find rep union)
      with
        | Not_found -> false

    let subset (u1, _) (u2, f2) =
      let process rep1 equality1 =
        let rep2 = Find.find rep1 f2 in
        let equality2 = Union.find rep2 u2 in
        Equality.subset equality1 equality2
      in
      try
        Union.for_all process u1
      with
        Not_found -> false

    let cardinal (union, _) =
      Union.fold (fun _ e acc -> acc + Equality.cardinal e) union 0


    let update_representative set rep find =
      Equality.fold (fun elt find -> Find.add elt rep find) set find

    let register equality rep (union, find) =
      Union.add rep equality union, update_representative equality rep find

    let inset equality (union, find) =
      let representative = Equality.choose equality in
      register equality representative (union, find)

    let singleton equality = inset equality empty

    let add equality (union, find) =
      let representatives =
        Equality.fold
          (fun elt accu ->
             try let rep = Find.find elt find in Set.add rep accu
             with Not_found -> accu)
          equality Set.empty
      in
      let process rep equality = Equality.union (Union.find rep union) equality in
      if Set.is_empty representatives
      then inset equality (union, find)
      else
        let equality = Set.fold process representatives equality in
        let union = Set.fold Union.remove representatives union in
        inset equality (union, find)

    let union unfi1 unfi2 =
      if Union.cardinal (fst unfi1) > Union.cardinal (fst unfi2)
      then fold add unfi2 unfi1
      else fold add unfi1 unfi2

    let add equality unfi = add equality unfi

    let remove elt (union, find) =
      try
        let rep = Find.find elt find in
        let equality = Union.find rep union in
        match Equality.remove elt equality with
          | `Void -> Union.remove rep union,
                     Equality.fold Find.remove equality find
          | `Value equality ->
              if Elt.equal rep elt
              then
                let uf = Union.remove rep union, Find.remove elt find in
                inset equality uf
              else
                Union.add rep equality union, Find.remove elt find
      with
        | Not_found -> (union, find)

    let unite (union, find) =
      (* Add the new element [e] to the equality whose representative si [rep]. *)
      let add_one e rep =
        let equality = Equality.add e (Union.find rep union) in
        let rep' = Equality.choose equality in
        let res =
          if Elt.equal rep rep'
          then
            Union.add rep equality union, Find.add e rep find
          else
            register equality rep' (Union.remove rep union, find)
        in
        res
      in
      (* Join the two equalities whose representative are [rep_a] and [rep_b]. *)
      let join rep_a rep_b =
        if Elt.equal rep_a rep_b then union, find
        else
          let equality_a = Union.find rep_a union
          and equality_b = Union.find rep_b union in
          let equality = Equality.union equality_a equality_b in
          let rep = Equality.choose equality in
          let res =
            if Elt.equal rep_a rep
            then
              (Union.add rep equality (Union.remove rep_b union),
               update_representative equality_b rep find)
            else if Elt.equal rep_b rep
            then
              (Union.add rep equality (Union.remove rep_a union),
               update_representative equality_a rep find)
            else
              assert false
          in
          res
      in
      fun a b ->
        if Elt.equal a b
        then union, find
        else
          let rep_a = Find.find_option a find
          and rep_b = Find.find_option b find in
          match rep_a, rep_b with
            | Some rep_a, Some rep_b -> join rep_a rep_b
            | Some rep_a, None -> add_one b rep_a
            | None, Some rep_b -> add_one a rep_b
            | None, None ->
              match Equality.pair a b with
              | `Void -> assert false (* a != b *)
              | `Value equality ->
                let rep = Equality.choose equality in
                let find = Find.add a rep (Find.add b rep find)
                and union = Union.add rep equality union in
                union, find

    let unite a b unfi = unite unfi a b

    let replace elt ersatz (union, find) =
      if contains elt (union, find)
      then
        let united = unite elt ersatz (union, find) in
        remove elt united
      else union, find

    let inter (u1, f1) (u2, f2) =
      if u1 == u2 then (u1, f1)
      else
        let u' = ref Union.empty in
        let inter =
          let cache = Hashtbl.create 24 in
          fun _key rep1 rep2 ->
            try Hashtbl.find cache (rep1, rep2)
            with Not_found ->
              let eq1 = Union.find rep1 u1 and eq2 = Union.find rep2 u2 in
              let rep' = match Equality.inter eq1 eq2 with
                | `Value eq' ->
                    let rep' = Equality.choose eq' in
                    u' := Union.add rep' eq' !u';
                    Some rep'
                | `Void -> None
              in
              Hashtbl.add cache (rep1, rep2) rep'; rep'
        in
        let f' =
          Find.inter
            ~cache:Hptmap_sig.NoCache
            ~symmetric:false
            ~idempotent:false
            ~decide:inter
            f1 f2
        in
        (!u', f')

    let diff (union1, find1) (union2, find2) =
      let process rep1 equality1 (union, find as acc) =
        try
          let rep2 = Find.find rep1 find2 in
          let equality2 = Union.find rep2 union2 in
          if Equality.subset equality1 equality2
          then
            Union.remove rep1 union,
            Equality.fold Find.remove equality1 find
          else
            acc
        with Not_found -> acc
      in
      Union.fold process union1 (union1, find1)

    (* This function is correct only if the second argument is a subset of
       the first. *)
    let _diff' =
      let union_merge =
        Union.merge
          ~cache:(Hptmap_sig.PersistentCache "Equalities.diff")
          ~symmetric:false
          ~idempotent:false
          ~decide_both:(fun _ v v' -> if Equality.equal v v' then None else Some v)
          ~decide_left:Union.Neutral
          ~decide_right:Union.Absorbing
      in
      fun former inter ->
        let u, f = former and u', _ = inter in
        let union = union_merge u u' in
        let filter _ rep = if Union.mem rep union then Some rep else None in
        let find = Find.map' filter f in
        union, find

    let choose (union, _) = snd (Union.min_binding union)

    let elements (union, _) = Union.fold (fun _ eq accu -> eq :: accu) union []

    let terms (_, find) =
      Find.fold (fun elt _ accu -> elt :: accu) find []

    let subst s (union, find) =
      let process elt _ (union, find) =
        match s elt with
          | Some ersatz ->
              begin
                if Elt.equal elt ersatz then (union, find)
                else replace elt ersatz (union, find)
              end
          | None -> remove elt (union, find)
      in
      Find.fold process find (union, find)
  end


end
