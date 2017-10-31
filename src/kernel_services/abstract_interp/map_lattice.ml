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


(* --------------------------- Module Types --------------------------------- *)

module type Value = sig
  include Datatype.S
  val top: t
  val bottom: t
end

module type Lattice = sig
  include Lattice_type.Bounded_Join_Semi_Lattice
  include Lattice_type.With_Narrow with type t := t
  include Lattice_type.With_Under_Approximation with type t := t
  include Lattice_type.With_Intersects with type t := t
  include Lattice_type.With_Diff with type t := t
end

module type Lattice_with_cardinality = sig
  include Lattice_type.With_Cardinal_One
  include Lattice_type.With_Diff_One with type t := t
  include Lattice_type.With_Enumeration with type t := t
end

module type Map_Lattice = sig
  include Hptmap_sig.S
  include Lattice with type t := t
  val find_or_bottom: key -> t -> v
  val find_lonely_key: t -> key * v
end

module type Map_Lattice_with_cardinality = sig
  include Lattice_with_cardinality
  type key
  type v
  val find_lonely_binding: t -> key * v
end

module type MapSet_Lattice = sig
  type set
  type map
  type t = Top of set * Origin.t | Map of map
  include Datatype.S_with_collections with type t := t
  include Lattice with type t := t

  val bottom: t
  val top: t
  type key
  type v
  val add: key -> v -> t -> t
  val find: key -> t -> v
  val find_lonely_key: t -> key * v
  val split : key -> t -> v * t
  val inject : key -> v -> t
  val get_keys : t -> set
  val filter_keys : (key -> bool) -> t -> t
  val map: (v -> v) -> t -> t
  val fold_keys : (key -> 'a -> 'a) -> t -> 'a -> 'a
  val fold : (key -> v -> 'a -> 'a) -> t -> 'a -> 'a
  val cached_fold:
    cache_name:string -> temporary:bool ->
    f:(key -> v -> 'a) ->
    projection:(key -> v) -> joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a
  val for_all: (key -> v -> bool) -> t -> bool
  val exists: (key -> v -> bool) -> t -> bool
  val pretty_debug : Format.formatter -> t -> unit
end

module type MapSet_Lattice_with_cardinality = sig
  include Lattice_with_cardinality
  type key
  type v
  val find_lonely_binding: t -> key * v
end


(* --------------------------- Map Lattice ---------------------------------- *)

module Make_Map_Lattice
    (Key: Hptmap.Id_Datatype)
    (Value : Lattice_type.Full_Lattice)
    (KVMap : Hptmap_sig.S with type key = Key.t
                           and type v = Value.t)
= struct

  include KVMap

  let bottom = KVMap.empty

  let add_or_bottom k v m =
    if Value.equal v Value.bottom
    then KVMap.remove k m
    else KVMap.add k v m

  let find_or_bottom k m =
    (* locations are usually small, so the difference between [M.find] and
       [M.find_check_missing] is usually unimportant.  However,
       [find_check_missing] is more efficient when we query NULL, which is a
       very common case. *)
    try KVMap.find_check_missing k m
    with Not_found -> Value.bottom

  let join =
    let decide _ v1 v2 = Value.join v1 v2
    and name = Printf.sprintf "Map_Lattice(%s).join" Value.name in
    KVMap.join ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:true ~idempotent:true ~decide

  let link =
    let decide _k v1 v2 = Value.link v1 v2 in
    let name = Printf.sprintf "Map_Lattice(%s).link" Value.name in
    KVMap.join ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:true ~idempotent:true ~decide

  let check_join_assert = ref 0

  let _debug_join m1 m2 =
    let result = join m1 m2 in
    assert (true ||
            let n = succ !check_join_assert in
            check_join_assert := n;
            n land 63 <> 0  ||
            let merge_key k v acc =
              KVMap.add k (Value.join v (find_or_bottom k m2)) acc
            in
            let mr' = KVMap.fold merge_key m1 m2 in
            if KVMap.equal result mr' then
              true
            else begin
              let pp_one fmt mm =
                Format.fprintf fmt "%a (%d;%x)@."
                  KVMap.pretty_debug mm (KVMap.hash mm)
                  (Extlib.address_of_value mm)
              in
              Format.printf "Map_Lattice.join incorrect@. %a+%a->@. %a/%a"
                pp_one m1 pp_one m2 pp_one result pp_one mr';
              false;
            end);
    result

  let narrow =
    let decide _k v1 v2 =
      let r = Value.narrow v1 v2 in
      if Value.equal Value.bottom r then None else Some r
    in
    let name = Printf.sprintf "Map_Lattice(%s).narrow" Value.name in
    KVMap.inter ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:true ~idempotent:true ~decide

  let meet =
    let decide _k v1 v2 =
      let r = Value.meet v1 v2 in
      if Value.equal Value.bottom r then None else Some r
    in
    let name = Printf.sprintf "Map_Lattice(%s).meet" Value.name in
    KVMap.inter ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:true ~idempotent:true ~decide

  let is_included =
    let name = Format.asprintf "Map_Lattice(%s).is_included" Value.name in
    let decide_fst _ _ = false in
    let decide_snd _ _ = true in
    let decide_both _ v1 v2 = Value.is_included v1 v2 in
    let decide_fast = KVMap.decide_fast_inclusion in
    KVMap.binary_predicate
      (Hptmap_sig.PersistentCache name) KVMap.UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let intersects =
    let name = Format.asprintf "Map_Lattice(%s).intersects" Value.name in
    KVMap.symmetric_binary_predicate
      (Hptmap_sig.PersistentCache name)
      KVMap.ExistentialPredicate
      ~decide_fast:KVMap.decide_fast_intersection
      ~decide_one:(fun _ _ -> false)
      ~decide_both:(fun _ x y -> Value.intersects x y)

  let diff =
    let name = Format.asprintf "Map_Lattice(%s).diff" Value.name in
    let decide_both _key v1 v2 =
      let v = Value.diff v1 v2 in
      if Value.(equal bottom v) then None else Some v
    in
    KVMap.merge
      ~cache:(Hptmap_sig.PersistentCache name)
      ~symmetric:false
      ~idempotent:false
      ~decide_left:KVMap.Neutral
      ~decide_right:KVMap.Absorbing
      ~decide_both

  (** if there is only one key [k] in map [m], then returns the pair [k,v]
      where [v] is the value associated to [k].
      @raise Not_found otherwise. *)
  let find_lonely_key m =
    match KVMap.is_singleton m with
    | Some p -> p
    | None -> raise Not_found


  module With_Cardinality
      (Value :
         Lattice_type.Full_AI_Lattice_with_cardinality with type t := Value.t)
  = struct

    (** if there is only one binding [k -> v] in map [m] (that is, only one key
        [k] and [cardinal_zero_or_one v]), returns the pair [k,v].
        @raise Not_found otherwise *)
    let find_lonely_binding m =
      let _, v as pair = find_lonely_key m in
      if not (Value.cardinal_zero_or_one v)
      then raise Not_found
      else pair

    let cardinal_zero_or_one m =
      equal m bottom ||
      try ignore (find_lonely_binding m); true
      with Not_found -> false

    (** the cardinal of a map [m] is the sum of the cardinals of the
        values bound to a key in [m] *)
    let cardinal_less_than m n =
      KVMap.fold
        (fun _b v card -> card + Value.cardinal_less_than v (n - card))
        m
        0

    let fold_enum f m acc =
      KVMap.fold
        (fun k vl acc ->
           let g one_ival acc =
             let one_loc = add_or_bottom k one_ival empty in
             f one_loc acc
           in
           Value.fold_enum g vl acc)
        m
        acc

    let diff_if_one m1 m2 =
      try
        let k2, v2 = find_lonely_binding m2 in
        let v1 = find_or_bottom k2 m1 in
        let v = Value.diff_if_one v1 v2 in
        add_or_bottom k2 v m1
      with Not_found -> m1
  end
end


(* ------------------------- Map/Set Lattice -------------------------------- *)

module Make_MapSet_Lattice
    (Key: Hptmap.Id_Datatype)
    (KSet: Lattice_type.Lattice_Set with type O.elt = Key.t)
    (Value : Value)
    (KVMap : Map_Lattice with type key = Key.t
                          and type v = Value.t)
= struct

  type t = Top of KSet.t * Origin.t | Map of KVMap.t

  let top = Top (KSet.top, Origin.top)

  let bottom = Map KVMap.empty

  let hash = function
    | Map m -> KVMap.hash m
    | Top (s, orig) -> Origin.hash orig + (299 * (KSet.hash s))

  let remove k = function
    | Top (KSet.Top, _) as t -> t
    | Top (KSet.Set s, o) -> Top (KSet.(inject (O.remove k s)), o)
    | Map m -> Map (KVMap.remove k m)

  let add k v = function
    | Top (KSet.Top, _) as t -> t
    | Top (KSet.Set s, o) -> Top (KSet.(inject (O.add k s)), o)
    | Map m -> Map (KVMap.add k v m)

  let add k v t =
    if Value.equal v Value.bottom
    then remove k t
    else add k v t

  let inject k v =
    if Value.equal v Value.bottom
    then bottom
    else Map (KVMap.singleton k v)


  let split k = function
    | Top (set, _) as t ->
      if KSet.mem k set
      then Value.top, remove k t
      else Value.bottom, t
    | Map m -> KVMap.find_or_bottom k m, Map (KVMap.remove k m)


  let get_keys = function
    | Top (set, _) -> set
    | Map m ->
      KSet.inject (KVMap.fold (fun k _ acc -> KSet.O.add k acc) m KSet.O.empty)


  let equal t t' =
    t == t' ||
    match t, t' with
    | Top (s, a), Top (s', a') -> KSet.equal s s' && Origin.equal a a'
    | Map m, Map m' -> KVMap.equal m m'
    | _ -> false

  let compare t t' =
    if t == t' then 0
    else match t, t' with
      | Top _, Map _  -> -1
      | Map _, Top _  -> 1
      | Map m, Map m' -> KVMap.compare m m'
      | Top (s, a), Top (s', a') ->
        let r = KSet.compare s s' in
        if r = 0 then Origin.compare a a' else r

  let filter_keys f = function
    | Top (s, o) -> Top (KSet.filter f s, o)
    | Map m -> Map (KVMap.filter f m)


  let join t1 t2 =
    if t1 == t2 then t1 else
      match t1, t2 with
      | Top (s1, o1), Top (s2, o2) -> Top (KSet.join s1 s2, Origin.join o1 o2)
      | Top (KSet.Top, _) as x, Map _
      | Map _, (Top (KSet.Top, _) as x) -> x
      | Top (KSet.Set s, o), Map m | Map m, Top (KSet.Set s, o) ->
        let s = KVMap.fold (fun k _ acc -> KSet.O.add k acc) m s in
        Top (KSet.inject s, o)
      | Map m1, Map m2 -> Map (KVMap.join m1 m2)

  let link t1 t2 =
    match t1, t2 with
    | Top _ as x, Map _
    | Map _, (Top _ as x) -> x (* arbitrary, may be approximated *)
    | Top (s1, o1), Top (s2, o2) -> Top (KSet.link s1 s2, Origin.link o1 o2)
    | Map m1, Map m2 -> Map (KVMap.link m1 m2)

  let meet t1 t2 =
    match t1, t2 with
    | Top (s1, o1), Top (s2, o2) -> Top (KSet.meet s1 s2, Origin.meet o1 o2)
    | Top (KSet.Top, _), (Map _ as m)
    | (Map _ as m), Top (KSet.Top, _) -> m
    | Top (KSet.Set s, _), (Map m)
    | (Map m), Top (KSet.Set s, _) ->
      Map (KVMap.filter (fun v -> KSet.O.mem v s) m)
    | Map m1, Map m2 -> Map (KVMap.meet m1 m2)

  let narrow t1 t2 =
    match t1, t2 with
    | Top (s1, o1), Top (s2, o2) ->
      Top (KSet.narrow s1 s2, Origin.narrow o1 o2)
    | Top (KSet.Top, _), (Map _ as m)
    | (Map _ as m), Top (KSet.Top, _) -> m
    | Top (KSet.Set set, _), (Map m)
    | (Map m), Top (KSet.Set set, _) ->
      Map (KVMap.filter (fun v -> KSet.O.mem v set) m)
    | Map m1, Map m2 -> Map (KVMap.narrow m1 m2)

  let is_included t1 t2 =
    match t1, t2 with
    | Top (s1, o1), Top (s2, o2)   ->
      KSet.is_included s1 s2 && Origin.is_included o1 o2
    | Map _, Top (KSet.Top, _)   -> true
    | Map m, Top (KSet.Set s, _) -> KVMap.for_all (fun k _ -> KSet.O.mem k s) m
    | Top _, Map _               -> false
    | Map m1, Map m2             -> KVMap.is_included m1 m2

  let intersects t1 t2 =
    match t1, t2 with
    | Top (s1, _), Top (s2, _) -> KSet.intersects s1 s2
    | Top (KSet.Top, _), Map m | Map m, Top (KSet.Top, _) ->
      not (KVMap.equal m KVMap.empty)
    | Top (KSet.Set s, _), Map m | Map m, Top (KSet.Set s, _) ->
      KVMap.exists (fun b _ -> KSet.O.mem b s) m
    | Map m1, Map m2 -> KVMap.intersects m1 m2

  let diff t1 t2 =
    match t1, t2 with
    | Top _, _ | _, Top _ -> t1
    | Map m1, Map m2      -> Map (KVMap.diff m1 m2)


  let find k = function
    | Top (s, _) -> if KSet.mem k s then Value.top else Value.bottom
    | Map m      -> KVMap.find_or_bottom k m

  let find_lonely_key = function
    | Top _ -> raise Not_found
    | Map m -> KVMap.find_lonely_key m


  let map f = function
    | Top _ as t -> t
    | Map m      -> Map (KVMap.map f m)

  let fold_keys f t acc =
    match t with
    | Top (s, _) -> KSet.fold f s acc
    | Map m -> KVMap.fold (fun k _ acc -> f k acc) m acc

  let fold f m acc =
    match m with
    | Top (s, _) -> KSet.fold (fun x acc -> f x Value.top acc) s acc
    | Map m      -> KVMap.fold f m acc

  let cached_fold ~cache_name ~temporary ~f ~projection ~joiner ~empty =
    let folded_f = KVMap.cached_fold ~cache_name ~temporary ~f ~joiner ~empty in
    function
    | Top (KSet.Top, _) -> raise Abstract_interp.Error_Top
    | Top (KSet.Set s, _) ->
      let f_base base acc =
        let total_itvs = projection base in
        joiner (f base total_itvs) acc
      in
      KSet.O.fold f_base s empty
    | Map m -> folded_f m


  let for_all f = function
    | Map m               -> KVMap.for_all f m
    | Top (KSet.Set t, _) -> KSet.O.for_all (fun x -> f x Value.top) t
    | Top (KSet.Top, _)   -> false

  let exists f = function
    | Map m               -> KVMap.exists f m
    | Top (KSet.Set t, _) -> KSet.O.exists (fun x -> f x Value.top) t
    | Top (KSet.Top, _)   -> true


  let pretty fmt = function
    | Top (t, a) ->
        Format.fprintf fmt "@[<hov 2>{{ mix of %a.@ Origin: %a}}@]"
          KSet.pretty t Origin.pretty a
    | Map m ->
        Pretty_utils.pp_iter
          ~pre:"@[<hv 3>{{ "
          ~suf:" }}@]"
          ~sep:";@ "
          (fun pp map -> KVMap.iter (fun k v -> pp (k, v)) map)
          (fun fmt (k, v) ->
             Format.fprintf fmt "%a -> %a" Key.pretty k Value.pretty v)
          fmt m

  let pretty_debug fmt = function
    | Top (t, a) ->
        Format.fprintf fmt "@[<hov 2>{{ mix of %a.@ Origin: %a}}@]"
          KSet.pretty t Origin.pretty a
    | Map m -> KVMap.pretty_debug fmt m


  include
    (Datatype.Make_with_collections
       (struct
         type tt = t
         type t = tt
         let name = KVMap.name ^ " mapset_lattice"
         let structural_descr =
           Structural_descr.t_sum
             [| [| KSet.packed_descr; Structural_descr.p_abstract |];
                [| KVMap.packed_descr |] |]
         let reprs = List.map (fun m -> Map m) KVMap.reprs
         let equal = equal
         let compare = compare
         let hash = hash
         let rehash = Datatype.identity
         let copy = Datatype.undefined
         let internal_pretty_code = Datatype.pp_fail
         let pretty = pretty
         let mem_project = Datatype.never_any_project
         let varname = Datatype.undefined
       end): Datatype.S_with_collections with type t := t)


  module With_Cardinality
      (KVMap : Map_Lattice_with_cardinality with type t := KVMap.t
                                             and type key := Key.t
                                             and type v := Value.t)
  = struct

    let find_lonely_binding = function
      | Top _ -> raise Not_found
      | Map m -> KVMap.find_lonely_binding m

    let cardinal_zero_or_one = function
      | Top _ -> false
      | Map m -> KVMap.cardinal_zero_or_one m

    let cardinal_less_than t n =
      match t with
      | Top _ -> raise Abstract_interp.Not_less_than
      | Map m -> KVMap.cardinal_less_than m n

    let fold_enum f m acc =
      match m with
      | Top _ -> raise Abstract_interp.Error_Top
      | Map m -> KVMap.fold_enum (fun m acc -> f (Map m) acc) m acc

    let diff_if_one t1 t2 =
      match t1, t2 with
      | Map m1, Map m2 -> Map (KVMap.diff_if_one m1 m2)
      | _, _ -> t1
  end
end
