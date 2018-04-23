(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Hcexprs

type 'a trivial = Trivial | NonTrivial of 'a
type 'a tree = Empty | Leaf of 'a | Node of 'a tree * 'a tree

type elt = Hcexprs.HCE.t

(* ------------------------------ Equality ---------------------------------- *)

module Equality = struct
  include HCESet

  (* cardinality less or equal to 1: not a real equivalence class *)
  let is_trivial s =
    try fold (fun _ acc -> if acc = 0 then 1 else raise Exit) s 0 <= 1
    with Exit -> false

  let return s = if is_trivial s then Trivial else NonTrivial s

  (* TODO: consistency *)
  let pair e1 e2 =
    if HCE.equal e1 e2
    then Trivial
    else NonTrivial (add e2 (singleton e1))

  let remove e s = return (remove e s)

  let inter s s' = return (inter s s')

  let filter f s = return (filter f s)

  let pretty fmt s =
    Pretty_utils.pp_iter ~pre:"@[<hov 3>{" ~sep:"@ =@ " ~suf:"}@]"
      iter (fun fmt a -> HCE.pretty fmt a)
      fmt s
end

type equality = Equality.t

(* --------------------------- Equality Sets -------------------------------- *)

module Set = struct

  module Initial_Values = struct let v = [[]] end
  module Dependencies = struct let l = [ HCE.self ] end

  (* A set of equalities between lvalues and expressions is encoded as a map
     from each lvalue or expression to:
     - the equality in which it is involved;
     - for a lvalue [lv], the set of expressions that contain [lv] in the map.
     This last information is needed when removing a lvalue, to remove or
     replace all expressions containing this lvalue. *)

  module Data = struct
    (* For a lvalue [lv], the first set gathers the expressions that depends on
       the value of [lv], and the second set gathers the expressions that
       contains [&lv]. *)
    include Datatype.Triple (Equality) (HCESet) (HCESet)
    let pretty_debug = pretty

    let inter (left_eq, left_set, left_set') (right_eq, right_set, right_set') =
      let equality = Equality.inter left_eq right_eq
      and set = HCESet.inter left_set right_set
      and set' = HCESet.inter left_set' right_set' in
      match equality with
      | NonTrivial eq -> Some (eq, set, set')
      | Trivial ->
        if HCESet.is_empty set && HCESet.is_empty set'
        then None
        else Some (Equality.empty, set, set')

    let union (left_eq, left_set, left_set') (right_eq, right_set, right_set') =
      Equality.union left_eq right_eq,
      HCESet.union left_set right_set,
      HCESet.union left_set' right_set'
  end

  include Hptmap.Make (HCE) (Data)
      (Hptmap.Comp_unused) (Initial_Values) (Dependencies)

  let find_option elt map =
    try
      let equality, _, _ = find elt map in
      if Equality.is_empty equality then None else Some equality
    with Not_found -> None

  let contains = mem

  let mem equality map =
    let head = Equality.choose equality in
    match find_option head map with
    | None -> false
    | Some eq -> Equality.subset equality eq

  let subset =
    binary_predicate
      (Hptmap_sig.PersistentCache "Equality.Set.subset")
      UniversalPredicate
      ~decide_fast:decide_fast_inclusion
      ~decide_fst:(fun _ (e, _, _) -> Equality.is_empty e)
      ~decide_snd:(fun _ _ -> true)
      ~decide_both:(fun _ (e1, _, _) (e2, _, _) -> Equality.subset e1 e2)

  let equal =
    binary_predicate
      (Hptmap_sig.PersistentCache "Equality.Set.subset")
      UniversalPredicate
      ~decide_fast:(fun s t -> if s == t then PTrue else PUnknown)
      ~decide_fst:(fun _ (e, _, _) -> Equality.is_empty e)
      ~decide_snd:(fun _ (e, _, _) -> Equality.is_empty e)
      ~decide_both:(fun _ (e1, _, _) (e2, _, _) -> Equality.equal e1 e2)

  (* TODO: replace all occurrences of Equality.fold by an heterogeneous
     iteration on the equality and the set of equalities. *)

  let register lvalues term map =
    let add_read = function
      | None -> Some (Equality.empty, HCESet.singleton term, HCESet.empty)
      | Some (equality, set, set') -> Some (equality, HCESet.add term set, set')
    and add_addr = function
      | None -> Some (Equality.empty, HCESet.empty, HCESet.singleton term)
      | Some (equality, set, set') -> Some (equality, set, HCESet.add term set')
    in
    let map = HCESet.fold (replace add_read) lvalues.Hcexprs.read map in
    HCESet.fold (replace add_addr) lvalues.Hcexprs.addr map

  (* Binds each element of [equality] into [equality] in [map], without
     changing dependances. *)
  let update_equality equality map =
    let update = function
      | Some (_, set, set') -> Some (equality, set, set')
      | None -> Some (equality, HCESet.empty, HCESet.empty)
    in
    Equality.fold (replace update) equality map

  let unite (a, a_lvalues) (b, b_lvalues) map =
    match Equality.pair a b with
    | Trivial -> map
    | NonTrivial equality ->
      (* Computes the transitive closure of [equality], taking the equalities
         already in [map] into account. *)
      let overall_equality, map =
        Equality.fold
          (fun elt (equality, map) -> match find_option elt map with
             | None ->
               let map =
                 if HCE.equal elt a
                 then register a_lvalues a map
                 else register b_lvalues b map
               in
               equality, map
             | Some eq -> Equality.union eq equality, map)
          equality (equality, map)
      in
      (* Binds each element of this transitive closure to the closure itself. *)
      update_equality overall_equality map

  (* ----------------------- Remove or replace ---------------------------- *)

  (* When replacing a lvalue by an equal term, we pick the lvalue or
     expression with the smallest height possible. [set] must not be empty. *)
  let pick_representative set =
    let choose elt (current, height) =
      let elt = HCE.to_exp elt in
      let h = Value_util.height_expr elt in
      if h < height then (elt, h) else (current, height)
    in
    let head = HCESet.choose set in
    let current = HCE.to_exp head in
    let height = Value_util.height_expr current in
    fst (HCESet.fold choose (HCESet.remove head set) (current, height))

  (* Binds the terms of the [equality] to [equality] in the [map].
     [equality] may be trivial, in which case its element is removed. *)
  let replace_by_equality equality map =
    let replace key =
      let update = function
        | None -> assert false
        | Some (_, set, set') ->
          if Equality.is_trivial equality
          then
            (* Do not remove an lvalue that is related to other expressions
               in the map. *)
            if HCE.is_lval key && not HCESet.(is_empty set && is_empty set')
            then Some (Equality.empty, set, set')
            else None
          else Some (equality, set, set')
      in
      replace update key
    in
    Equality.fold replace equality map

  (* Removes [elt] from the equalities of the [map], where [elt] is not
     a lvalue. Does not update the dependencies of [elt] (the lvalues that
     [elt] contains still link to [elt]). *)
  let remove_from_equalities elt map =
    match find_option elt map with
    | None -> map
    | Some eq ->
      let map = replace_by_equality (HCESet.remove elt eq) map in
      remove elt map

  (* In the expression [elt], replaces [late] by [heir] and updates the map
     accordingly (the equality involving [elt] and the lvalues pointing to
     [elt]. *)
  let replace_in_element kind elt ~late ~heir map =
    if contains elt map
    then
      try
        (* Replaces [late] by [heir] in [elt]. *)
        let new_elt = HCE.replace kind ~late ~heir elt in
        let empty_lvalues = Hcexprs.empty_lvalues in
        let new_lvalues =
          if HCE.is_lval new_elt
          then empty_lvalues
          else syntactic_lvalues (HCE.to_exp new_elt)
        in
        (* Unite [elt] and [new_elt] before removing [elt].*)
        let map = unite (elt, empty_lvalues) (new_elt, new_lvalues) map in
        (* Removes [elt] from the new equality and the map.
           TODO: updates lvals to remove [elt] from their binding? *)
        let equality, _, _ = find elt map in
        let equality = HCESet.remove elt equality in
        let map = remove elt map in
        (* Updates the new equality in the map. *)
        replace_by_equality equality map
      with NonExchangeable -> remove_from_equalities elt map
    else map

  (* [remove lval map] removes any occurence of the lvalue [lval] in the [map].
     When possible, [lval] is replaced by an equal lvalue or expression in any
     term of [map] that contains [lval]. Otherwise, these terms are simply
     removed as well. *)
  let remove kind lval map =
    let elt = HCE.of_lval lval in
    try
      let (equality, deps, addr_deps) = find elt map in
      (* If [lval] is out of scope, removes all terms that contain &lval. *)
      let map =
        if kind = Hcexprs.Deleted
        then HCESet.fold remove_from_equalities addr_deps map
        else map
      in
      (* Removes [lval] from [equality] and from the [map]. *)
      let equality = HCESet.remove elt equality in
      let map = replace_by_equality equality map in
      let map = remove elt map in
      (* If possible, replaces [lval] by an equal term (namely a term from
         [equality] that is not [lval] itself, nor contains [lval]). Otherwise,
         removes all terms containing [lval]. *)
      let equality = HCESet.diff equality deps in
      if HCESet.is_empty equality
      then HCESet.fold remove_from_equalities deps map
      else
        (* Replaces all occurrences of [lval] by [rep]. *)
        let rep = pick_representative equality in
        let process elt map =
          replace_in_element kind elt ~late:lval ~heir:rep map
        in
        HCESet.fold process deps map
    with
    (* If [lval] is not bound in the [map], nothing to do. *)
      Not_found -> map

  let find elt map =
    let equality, _, _ = find elt map in
    if Equality.is_empty equality then raise Not_found else equality


  (* ---------------------- Merges and iterators -------------------------- *)

  (* The pointwise union of the maps is incomplete: the naive union of [a=b] and
     [b=c] binds [b] to [a=b=c], but binds [a] to [a=b] and [c] to [b=c].  This
     function computes properly the join of separated equalities, as well as the
     connections between lvalues and expressions. It must however be completed
     by the transitive closure of equalities. *)
  let naive_union =
    let decide _key left right = Data.union left right in
    join
      ~cache:(Hptmap_sig.PersistentCache "Equality.Set.union")
      ~symmetric:true
      ~idempotent:true
      ~decide

  (* Computes the transitive closure of the equalities from two maps. Ignores
     the equalities that do not involve a same term in both maps, as they are
     properly handled by [naive_union]. Thus, this function only considers the
     keys that belongs to both maps (and their bound equalities). It does not
     update the connections between lvalues and expressions, as they are
     properly computed by [naive_union]. *)
  let transitive_closure =
    (* The terms present in only one set are ignored. *)
    let empty_left _ = empty
    and empty_right _ = empty in
    (* If the equalities bound to [key] are different and non empty in both
       maps, join them. The [naive_union] has correctly join the equal
       equalities.*)
    let both key (eq, _, _) (eq', _, _) =
      if Equality.is_empty eq || Equality.is_empty eq' || Equality.equal eq eq'
      then empty
      else
        let join = Equality.union eq eq' in
        singleton key (join, HCESet.empty, HCESet.empty)
    in
    (* The join of two sets computes the transitive closure of their equalities. *)
    let join set set' =
      (* Adds an equality to a set. If the equality contains some terms bound to
         other equalities in the set, then performs the union of all these
         equalities, and updates these terms with the resulting equality.
         Does not handle connections between lvalues and expressions. *)
      let add key equality set =
        let process elt (list, eq as acc) =
          match find_option elt set with
          | None -> acc
          | Some eq' -> elt :: list, Equality.union eq eq'
        in
        (* [keys] are the keys to update with the new [equality]. *)
        let keys, equality = Equality.fold process equality ([key], equality) in
        let data = equality, HCESet.empty, HCESet.empty in
        List.fold_left (fun acc key -> add key data acc) set keys
      in
      (* Addition of each equality from the right set to the left set. *)
      fold (fun elt (eq, _, _) acc -> add elt eq acc) set' set
    in
    fold2_join_heterogeneous
      ~cache:(Hptmap_sig.PersistentCache "Equality.Set.in_both")
      ~empty_left ~empty_right ~both ~join ~empty

  let union a b =
    (* Naive pointwise union of maps. *)
    let r = naive_union a b in
    (* Computes the equalities that are missing in [r]. *)
    let missing_equalities = transitive_closure a (shape b) in
    (* Binds the equalities of [missing_equalities] in [r]. [processed] is
       the set of the terms that have been already updated. *)
    let update key (equality, _, _) (map, processed) =
      if HCESet.mem key processed
      then map, processed
      else update_equality equality map, HCESet.union equality processed
    in
    fst (fold update missing_equalities (r, HCESet.empty))

  let inter =
    inter
      ~cache:(Hptmap_sig.PersistentCache "Equality.Set.inter")
      ~symmetric:true
      ~idempotent:true
      ~decide:(fun _ a b -> Data.inter a b)

  let choose map =
    let equality, _, _ =  snd (min_binding map) in equality

  (* is representative? *)
  let is_rep elt eq =
    if Equality.is_empty eq
    then false
    else HCE.equal (Equality.choose eq) elt

  let fold f map acc =
    fold
      (fun elt (eq, _, _) acc -> if is_rep elt eq then f eq acc else acc)
      map acc

  let elements map = fold (fun eq acc -> eq :: acc) map []

  let iter f map =
    iter (fun elt (eq, _, _) -> if is_rep elt eq then f eq) map
  let exists f map =
    exists (fun elt (eq, _, _) -> if is_rep elt eq then f eq else false) map
  let for_all f map =
    for_all (fun elt (eq, _, _) -> if is_rep elt eq then f eq else true) map

  let deep_fold f map acc =
    fold (fun eq accu -> Equality.fold (f eq) eq accu) map acc

  let pretty fmt map =
    Pretty_utils.pp_iter ~pre:"@[" ~sep:"@ " ~suf:"@]"
      iter (fun fmt eq -> Format.fprintf fmt "@[%a@]" Equality.pretty eq)
      fmt map

  let keys =
    let cache_name = "Equality.Set.keys" in
    let temporary = false in
    let f k _ = if HCE.is_lval k then Leaf k else Empty in
    let joiner t1 t2 = Node (t1, t2) in
    let empty = Empty in
    cached_fold ~cache_name ~temporary ~f ~joiner ~empty

  let lvalues_only_left =
    let cache = Hptmap_sig.PersistentCache "Equality.Set.elements_only_left" in
    let empty_left _ = Empty in
    let empty_right t = keys t in
    let both _ _ _ = Empty in
    let join t1 t2 = Node (t1, t2) in
    let empty = Empty in
    let f = fold2_join_heterogeneous
        ~cache ~empty_left ~empty_right ~both ~join ~empty
    in
    fun eqs1 eqs2 -> f eqs1 (shape eqs2)

end
