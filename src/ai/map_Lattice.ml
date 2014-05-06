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

(** Map from a set of keys to values (a [Lattice_With_Diff]), equipped
    with the natural lattice interpretation. Keys must be mappable to
    integers in an unique way, and one of its elements ([null]) is
    singled out. *)

open Abstract_interp

module type Key = sig
  include Datatype.S
  val id : t -> int
end

module Make_without_cardinal
  (K : Key)
  (Top_Param : Lattice_type.Lattice_Hashconsed_Set with type O.elt=K.t)
  (V : Lattice_type.Full_Lattice)
  (Comp: sig (** See {!Hptmap} for the documentation of this option *)
     val e: bool
     val f : K.t -> V.t -> bool
     val compose : bool -> bool -> bool
     val default:bool
  end)
  (L: sig val v : (K.t * V.t) list list end)
  (Null_Behavior: sig
    val null : K.t
    val is_null : K.t -> bool
    val zone: bool
  end)
  =
struct

  module M =
    Hptmap.Make
      (K)
      (V)
      (Comp)
      (struct let v = [] :: [Null_Behavior.null,V.top]::L.v end)
      (struct let l = [ Ast.self ] end) (* TODO: this should be an argument of the functor *)
  let () = Ast.add_monotonic_state M.self


  module Top_Param = Top_Param

  type map_t = M.t
  type tt = Top of Top_Param.t * Origin.t | Map of map_t
    (* Invariant :
       [Top (s,_)] ok if [Top_Param.null] is not in [s]
       [Top (emptyset,_)] is injected to [Map (Null,Top)] *)

  let top = Top(Top_Param.top, Origin.top)

  let hash v =
    match v with
      Map m ->
        (* let f k v acc =
           (V.hash v) +  11 * acc + 54971 * K.hash k in
           M.fold f m 3647 *)
        M.hash m
    | Top (bases, orig) ->
        Origin.hash orig + (299 * (Top_Param.hash bases))

  let add_or_bottom k v m =
    if V.equal v V.bottom
    then M.remove k m
    else M.add k v m

  let bottom = Map M.empty

  let inject k v =
    Map (add_or_bottom k v M.empty)

  let top_int = inject Null_Behavior.null V.top

  let inject_top_origin origin t =
    if Null_Behavior.zone
    then
      Top (Top_Param.inject t, origin)
    else
      let s = Top_Param.O.remove Null_Behavior.null t in
      if Top_Param.O.is_empty s
      then top_int
      else Top (Top_Param.inject s, origin)

  let is_in_topset set elt =
    (Null_Behavior.is_null elt && not Null_Behavior.zone) ||
      Top_Param.O.mem elt set

  let pretty fmt m =
    match m with
    | Top (t, a) ->
        Format.fprintf fmt "@[<hov 2>{{ mix of %a.@ Origin: %a}}@]"
          Top_Param.pretty t
          Origin.pretty a
    | Map m ->
        Pretty_utils.pp_iter
          ~pre:"@[<hv 3>{{ "
          ~suf:" }}@]"
          ~sep:";@ "
          (fun pp map -> M.iter (fun k v -> pp (k, v)) map)
          (fun fmt (k, v) -> Format.fprintf fmt "%a -> %a" K.pretty k V.pretty v)
          fmt m


  let find_or_bottom k m =
    try
      M.find k m
    with
      Not_found -> V.bottom

  let split k m =
    match m with
    | Top (t,_) ->
        if Top_Param.is_included (Top_Param.inject_singleton k) t
        then V.top, m
        else V.bottom, m
    | Map m ->
        find_or_bottom k m,
        Map (M.remove k m)

  let inject_map m = Map m

  let get_bases map =
    (M.fold (fun k _ acc -> Top_Param.O.add k acc) map Top_Param.O.empty)

  exception Error_Top

  let decide_none _k v = v
  let decide_some v1 v2 = V.join v1 v2

  let equal m1 m2 =
    m1 == m2 ||
      match m1, m2 with
      | Top (s, a), Top (s', a') ->
          Top_Param.equal s s' && Origin.equal a a'
      | Map m1, Map m2 ->
          M.equal m1 m2
      | _ -> false

  let compare =
    if M.compare == Datatype.undefined ||
      Top_Param.compare == Datatype.undefined ||
      Origin.compare == Datatype.undefined
    then (Kernel.debug "%s map_lattice, missing comparison function: %b %b %b"
            M.name
            (M.compare == Datatype.undefined)
            (Top_Param.compare == Datatype.undefined)
            (Origin.compare == Datatype.undefined);
          Datatype.undefined)
    else
      fun m1 m2 ->
        if m1 == m2 then 0
        else match m1, m2 with
          | Top _, Map _ -> -1
          | Map _, Top _ -> 1
          | Map m1, Map m2 -> M.compare m1 m2
          | Top (s, a), Top (s', a') ->
              let r = Top_Param.compare s s' in
              if r = 0 then Origin.compare a a'
              else r


  let is_bottom b = equal b bottom

  let check_join_assert = ref 0

  let join =
    let symmetric_merge =
      M.symmetric_merge
        ~cache:("map_Lattice", ()) ~decide_none ~decide_some
    in
    fun m1 m2 ->
      if m1 == m2 then m1 else
          match m1, m2 with
          | Top(x1,a1), Top(x2,a2) ->
              Top(Top_Param.join x1 x2, Origin.join a1 a2)
          | Top (Top_Param.Top,_) as x, Map _
          | Map _, (Top (Top_Param.Top,_) as x) ->
              x
          | Top (Top_Param.Set t,a), Map m | Map m, Top (Top_Param.Set t,a) ->
              inject_top_origin a
                (M.fold
                   (fun k _ acc -> Top_Param.O.add k acc)
                   m
                   t)
          | Map mm1, Map mm2 ->
              let mresult = symmetric_merge mm1 mm2 in
              assert (
                let n = succ !check_join_assert in
                check_join_assert := n;
                n land 63 <> 0  ||
                  let merge_key k v acc =
                    M.add k (V.join v (find_or_bottom k mm2)) acc
                  in
                  let mr' = M.fold merge_key mm1 mm2 in
                  if M.equal mresult mr' then
                    true
                  else begin
                    let pp_one fmt mm =
                      Format.fprintf fmt "%a (%d;%x)@."
                        M.pretty mm (M.hash mm) (Extlib.address_of_value mm)
                    in
                    Format.printf "Map_Lattice.join incorrect@. %a+%a->@.#%a&%a"
                      pp_one mm1 pp_one mm2 pp_one mresult pp_one mr';
                    false;
                  end);
              Map mresult

  let cached_fold ~cache_name ~temporary ~f ~projection ~joiner ~empty =
    let folded_f = M.cached_fold ~cache_name ~temporary ~f ~joiner ~empty in
    function m ->
      match m with
        Top (Top_Param.Top, _) -> raise Error_Top
      | Top (Top_Param.Set s, _) ->
          let f_base base acc =
            let total_itvs = projection base in
            joiner (f base total_itvs) acc
          in
          Top_Param.O.fold f_base s empty
      | Map mm ->
          folded_f mm

  let map_offsets f m =
    match m with
    | Top _ -> raise Error_Top
    | Map m -> Map (M.map f m)

  (** Over-approximation of the filter (in the case [Top Top])*)
  let filter_base f m =
    match m with
    | Top (t, a) ->
        (try
           inject_top_origin a
             (Top_Param.fold
                (fun v acc -> if f v then Top_Param.O.add v acc else acc)
                t
                Top_Param.O.empty)
         with Top_Param.Error_Top -> top)
    | Map m ->
        Map (M.fold (fun k _ acc -> if f k then acc else M.remove k acc) m m)

  let meet m1 m2 =
    if m1 == m2 then m1 else
      match m1, m2 with
      | Top (x1, a1), Top (x2, a2) ->
          let meet_topparam = Top_Param.meet x1 x2 in
          Top (meet_topparam, Origin.meet a1 a2)
      | Top (Top_Param.Top, _), (Map _ as x)
      | (Map _ as x),Top (Top_Param.Top, _) -> x
      | Top (Top_Param.Set set, _), (Map _ as x)
      | (Map _ as x), Top (Top_Param.Set set, _) ->
          filter_base (fun v -> is_in_topset set v) x
      | Map m1, Map m2 ->
          let merge_key k v acc =
            add_or_bottom k (V.meet v (find_or_bottom k m2)) acc
          in
          Map (M.fold merge_key m1 M.empty)


  let narrow m1 m2 =
    let compute_origin_narrow x1 a1 x2 a2 =
      if Top_Param.equal x1 x2 then Origin.narrow a1 a2 (* equals a1 currently*)
      else if Top_Param.is_included x1 x2 then a1
      else if Top_Param.is_included x2 x1 then a2
      else Origin.top
    in
    let r =
      if m1 == m2 then m1 else
        match m1, m2 with
        | Top (x1, a1), Top (x2, a2) ->
            Top (Top_Param.narrow x1 x2,
                 compute_origin_narrow x1 a1 x2 a2)
        | Top (Top_Param.Top, _), (Map _ as x)
        | (Map _ as x),Top (Top_Param.Top, _) -> x
        | Top (Top_Param.Set set, _), (Map _ as x)
        | (Map _ as x), Top (Top_Param.Set set, _) ->
            filter_base (fun v -> is_in_topset set v) x
        | Map m1, Map m2 ->
            let merge_key k v acc =
              add_or_bottom k (V.narrow v (find_or_bottom k m2)) acc in
            Map (M.fold merge_key m1 M.empty)
    in
(*     Format.printf "Map_Lattice.narrow %a and %a ===> %a@\n"
       pretty x pretty y pretty r;  *)
    r

  let equal m1 m2 =
    m1 == m2 ||
      match m1, m2 with
      | Top (s, a), Top (s', a') ->
          Top_Param.equal s s' && Origin.equal a a'
      | Map m1, Map m2 ->
          M.equal m1 m2
      | _ -> false

  let is_included =
    let name =
      Pretty_utils.sfprintf "Map_Lattice(%s)(%s).is_included" K.name V.name
    in
    let decide_fst _ _ = false in
    let decide_snd _ _ = true in
    let decide_both _ v1 v2 = V.is_included v1 v2 in
    let decide_fast = M.decide_fast_inclusion in
    let map_is_included =
      M.binary_predicate (M.PersistentCache name) M.UniversalPredicate
        ~decide_fast ~decide_fst ~decide_snd ~decide_both
    in
    fun m1 m2 ->
      (match m1,m2 with
         | Top (s,a), Top (s',a') ->
             Top_Param.is_included s s' &&
             Origin.is_included a a'
         | Map _, Top (Top_Param.Top, _) -> true
         | Map m, Top (Top_Param.Set set, _) ->
             M.for_all (fun k _ -> is_in_topset set k) m
         | Top _, Map _ -> false
         | Map m1, Map m2 -> map_is_included m1 m2)

  let join_and_is_included a b =
    let ab = join a b in (ab, equal a b)


  (* under-approximation of union *)
  let link m1 m2 =
    if is_included m1 m2 then m2      (* exact *)
    else if is_included m2 m1 then m1 (* exact *)
    else match m1, m2 with
    | Top _, Map _ -> m1 (* may be approximated *)
    | Map _, Top _ -> m2 (* may be approximated *)
    | Top (s,_), Top (s',_) ->
        if Top_Param.is_included s s' then m2 (* may be approximated *)
        else if Top_Param.is_included s' s then m1 (* may be approximated *)
        else m1  (* very approximated *)
    | Map mm1, Map mm2 ->
        let map =
          M.fold
            (fun k v1 acc ->
               let v2 = find_or_bottom k mm2 in
               let link_v = V.link v1 v2 in
               M.add k link_v acc)
            mm1
            mm2
        in
        Map map

  let intersects =
    let name =
      Pretty_utils.sfprintf "Map_Lattice(%s)(%s).intersects" K.name V.name
    in
    let map_intersects =
      M.symmetric_binary_predicate
        (M.PersistentCache name) M.ExistentialPredicate
	~decide_fast:M.decide_fast_intersection
	~decide_one:(fun _ _ -> false)
	~decide_both:(fun _ x y -> V.intersects x y)
    in    
    fun mm1 mm2 ->
      match mm1, mm2 with
      | Top (s1, _), Top (s2, _) ->
        not Null_Behavior.zone || Top_Param.intersects s1 s2
      | Top (Top_Param.Top, _), Map m | Map m, Top (Top_Param.Top, _) ->
        not (M.equal m M.empty)
      | Top (Top_Param.Set s, _), Map m | Map m, Top (Top_Param.Set s, _) ->
        M.exists (fun b _ -> is_in_topset s b) m
      | Map m1, Map m2 -> map_intersects m1 m2

  (** if there is only one key [k] in map [m], then returns the pair [k,v]
      where [v] is the value associated to [k].
      @raise Not_found otherwise. *)
  let find_lonely_key m =
    match m with
    | Top _ -> raise Not_found
    | Map m ->
      match M.is_singleton m with
        | Some p -> p
        | _ -> raise Not_found

  let diff m1 m2 =
    match m1, m2 with
    | Top _, _ | _, Top _ -> m1
    | Map mm1, Map mm2 ->
        let result =
          M.fold
            (fun k v1 acc ->
               let dif =
                 try
                   let v2 = M.find k mm2 in
                   (V.diff v1 v2)
                 with Not_found -> v1
               in
               add_or_bottom k dif acc)
            mm1
            M.empty
        in
        Map result

  let map_i f m =
    match m with
    | Top _ -> top
    | Map m ->
        M.fold
          (fun k vl acc ->
             join acc (f k vl))
          m
          bottom

  let fold_bases f m acc =
    match m with
      Top(Top_Param.Set t, _) ->
        let acc = if Null_Behavior.zone then acc
          else f Null_Behavior.null acc
        in
        Top_Param.O.fold f t acc
    | Top(Top_Param.Top, _) ->
        raise Error_Top
    | Map m ->
        M.fold (fun k _ acc -> f k acc) m acc

  (** [fold_i f m acc] folds [f] on the bindings in [m].
      @raise Error_Top if [m] is too imprecise for folding. *)
  let fold_i f m acc =
    match m with
      Top(Top_Param.Set _, _) ->
        (* In this function,
           we refuse to iterate on the bases of a value Top(Top_Param.Set _,_)
        *)
        raise Error_Top
    | Top(Top_Param.Top, _) ->
        raise Error_Top
    | Map m ->
        M.fold f m acc

  let fold_topset_ok f m acc =
    match m with
      Top(Top_Param.Set t, _) ->
        let acc =
          if Null_Behavior.zone then acc else f Null_Behavior.null V.top acc
        in
        Top_Param.O.fold
          (fun x acc -> f x V.top acc)
          t
          acc
    | Top(Top_Param.Top, _) ->
        raise Error_Top
    | Map m ->
        M.fold f m acc

  include Datatype.Make_with_collections
      (struct
        type t = tt
        let name = M.name ^ " map_lattice"
        let structural_descr =
           Structural_descr.t_sum
             [| [| Top_Param.packed_descr; Structural_descr.p_abstract |];
                [| M.packed_descr |] |]
        let reprs = List.map (fun m -> Map m) M.reprs
        let equal = equal
        let compare = compare
        let hash = hash
        let rehash = Datatype.identity
        let copy = Datatype.undefined
        let internal_pretty_code = Datatype.pp_fail
        let pretty = pretty
        let mem_project = Datatype.never_any_project
        let varname = Datatype.undefined
       end)

  let clear_caches = M.clear_caches

end


module Make
  (K : Key)
  (Top_Param : Lattice_type.Lattice_Hashconsed_Set with type O.elt=K.t)
  (V : Lattice_type.Full_AI_Lattice_with_cardinality)
  (Comp: sig (** See {!Hptmap} for the documentation of this option *)
     val e: bool
     val f : K.t -> V.t -> bool
     val compose : bool -> bool -> bool
     val default:bool
  end)
  (L: sig val v : (K.t * V.t) list list end)
  (Null_Behavior: sig
    val null : K.t
    val is_null : K.t -> bool
    val zone: bool
  end)
  =
struct
  include Make_without_cardinal(K)(Top_Param)(V)(Comp)(L)(Null_Behavior)

  type widen_hint = K.t -> V.widen_hint

  let widen wh =
    let widen_map =
    let decide k v1 v2 =
      let v1 = match v1 with
        None -> V.bottom
      | Some v1 -> v1
      in
      let v2 = match v2 with
        None -> V.bottom
      | Some v2 -> v2
      in
      V.widen (wh k) v1 v2
    in
    M.generic_merge
      ~cache:("", false (* No cache, because of wh *))
      ~decide
      ~idempotent:true
    in
    fun m1 m2 ->
      match m1, m2 with
      | _ , Top _ -> m2
      | Top _, _ -> assert false (* m2 should be larger than m1 *)
      | Map m1, Map m2 ->
          Map (widen_map m1 m2)

  (** if there is only one binding [k -> v] in map [m] (that is, only one key
      [k] and [cardinal_zero_or_one v]), returns the pair [k,v].
      @raise Not_found otherwise *)
  let find_lonely_binding m =
    let _,v as pair = find_lonely_key m in
    if not (V.cardinal_zero_or_one v)
    then raise Not_found
    else pair

  let cardinal_zero_or_one m =
    equal m bottom ||
      try
        let _,_ = find_lonely_binding m
        in true
      with Not_found -> false

  (** the cardinal of a map [m] is the sum of the cardinals of the
      values bound to a key in [m] *)
  let cardinal_less_than m n =
    match m with
    | Top _ -> raise Not_less_than
    | Map m ->
        M.fold
          (fun _base v card -> card + V.cardinal_less_than v (n-card))
          m
          0

  let fold_enum f m acc =
    match m with
    | Top _ -> raise Error_Top
    | Map m ->
        try
          M.fold
            (fun k vl acc ->
               let g one_ival acc =
                 let one_loc = inject k one_ival in
                 f one_loc acc
               in
               V.fold_enum g vl acc)
            m
            acc
        with V.Error_Top -> raise Error_Top

  let diff_if_one m1 m2 =
    match m1 with
    | Top _ -> m1
    | Map mm1 ->
        try
          let k2,v2 = find_lonely_binding m2 in
          let v1 = find_or_bottom k2 mm1 in
          let v = V.diff_if_one v1 v2 in
          Map (add_or_bottom k2 v mm1)
        with Not_found -> m1

end


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
