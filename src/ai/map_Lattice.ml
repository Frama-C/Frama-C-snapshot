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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please write a .mli and
   document it. *)

(* In this kind of Map, absent keys are implicitly bound to V.bottom *)

open Abstract_interp

module type Lattice_with_rehash = Lattice_With_Diff

module Make
  (K : Key)
  (Top_Param : Lattice_Set with type O.elt=K.t)
  (V : Lattice_with_rehash)
  (L: sig val v : (K.t * V.t) list list end)
  (Null_Behavior: sig val zone: bool end)
  =
struct

(*  module Top_Param = Make_Hashconsed_Lattice_Set(K) *)

  module M =
    Hptmap.Make
      (K)
      (V)
      (Hptmap.Comp_unused)
      (struct let v = [] :: [K.null,V.top]::L.v end)
      (struct let l = [ Ast.self ] end)


  module Top_Param = Top_Param

  type map_t = M.t
  type tt = Top of Top_Param.t * Origin.t | Map of map_t
    (* Invariant :
       [Top (s,_)] ok if [Top_Param.null] is not in [s]
       [Top (emptyset,_)] is injected to [Map (Null,Top)] *)

  type widen_hint = Top_Param.widen_hint * (K.t -> V.widen_hint)

  let top = Top(Top_Param.top, Origin.top)

  let hash v =
    match v with
      Map m ->
        (* let f k v acc =
           (V.hash v) +  11 * acc + 54971 * K.hash k in
           M.fold f m 3647 *)
        M.tag m
    | Top (bases, orig) ->
        Origin.hash orig + (299 * (Top_Param.hash bases))

  let tag = hash

  let add_or_bottom k v m =
    if V.equal v V.bottom
    then M.remove k m
    else M.add k v m

  let bottom = Map M.empty

  let inject k v =
    Map (add_or_bottom k v M.empty)

  let top_int = inject K.null V.top

  let inject_top_origin origin t =
    if Null_Behavior.zone
    then
      Top (Top_Param.inject t, origin)
    else
      let s = Top_Param.O.remove K.null t in
      if Top_Param.O.is_empty s
      then top_int
      else Top (Top_Param.inject s, origin)

  let is_in_set ~set elt =
    (K.is_null elt && not Null_Behavior.zone) || Top_Param.O.mem elt set

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

  exception Error_Bottom
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
    let symetric_merge =
      M.symetric_merge
        ~cache:("map_Lattice",8192) ~decide_none ~decide_some
    in
    fun m1 m2 ->
      if m1 == m2 then m1 else
        let result =
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
              let result = Map (symetric_merge mm1 mm2) in
              assert (

                  let n = succ !check_join_assert in
                  check_join_assert := n;
                  n land 63 <> 0  ||
                (let merge_key k v acc =
                  M.add k (V.join v (find_or_bottom k mm2)) acc
                in
                let r2 = Map (M.fold merge_key mm1 mm2) in
                if equal result r2 then
                  true
                else begin
                  Format.printf "Map_Lattice.join incorrect %a (%d;%x) %a (%d;%x) -> %a (%d;%x) %a (%d;%x)@."
                    pretty m1 (hash m1) (Extlib.address_of_value m1)
                    pretty m2 (hash m2) (Extlib.address_of_value m2)
                    pretty result (hash result) (Extlib.address_of_value result)
                    pretty r2 (hash r2) (Extlib.address_of_value r2);
                  false;
                  end));

              result
        in
        (*Format.printf "Map_Lattice_join@\nm1=%a@\nm2=%a@\nm1Um2=%a@\n"
          pretty m1 pretty m2 pretty result;*)
        result

  let cached_fold ~cache ~temporary ~f ~projection ~joiner ~empty =
    let folded_f = M.cached_fold ~cache ~temporary ~f ~joiner ~empty in
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

  exception Not_exclusive

  (** [find_exclusive k m] returns [v] if [m] contains only the binding [k] ->
      [v]
      @raise Not_exclusive otherwise. *)
  let find_exclusive k m =
    match m with
    | Top _ -> raise Not_exclusive
    | Map m ->
        let v = find_or_bottom k m in
        let map_without = M.remove k m in
        if M.is_empty map_without
        then v
        else raise Not_exclusive

  exception Not_all_keys

  (** If all keys are bound to [v0] in [m], [get_keys_exclusive v0 m] returns
      the list of keys in [m].
      @raise Not_all_keys otherwise. *)
  let get_keys_exclusive v0 m =
    match m with
    | Top _ -> raise Not_all_keys
    | Map m ->
        M.fold
          (fun k v acc ->
             if not (V.equal v v0)
             then raise Not_all_keys
             else k::acc)
          m
          []

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
          filter_base (fun v -> is_in_set ~set v) x
      | Map m1, Map m2 ->
          let merge_key k v acc =
            add_or_bottom k (V.meet v (find_or_bottom k m2)) acc
          in
          Map (M.fold merge_key m1 M.empty)

(*
  let narrow m1 m2 =
    if m1 == m2 then m1 else
      match m1, m2 with
      | Top (x1, a1), Top (x2, a2) ->
          Top (Top_Param.narrow x1 x2, Origin.narrow a1 a2)
      | Top (Top_Param.Top, _), (Map _ as x)
      | (Map _ as x),Top (Top_Param.Top, _) -> x
      | Top (Top_Param.Set set, _), (Map _ as x)
      | (Map _ as x), Top (Top_Param.Set set, _) ->
          filter_base (fun v -> is_in_set ~set v) x
      | Map m1, Map m2 ->
          let merge_key k v acc =
            add_or_bottom k (V.narrow v (find_or_bottom k m2)) acc
          in
          Map (M.fold merge_key m1 M.empty)
*)


let narrow =
    let intersect f origin m1 m2 =
      if m1 == m2 then m1 else
        match m1, m2 with
        | Top (x1, a1), Top (x2, a2) ->
            let meet_topparam = Top_Param.meet x1 x2 in
            Top (meet_topparam, origin x1 a1 x2 a2)
        | Top (Top_Param.Top, _), (Map _ as x)
        | (Map _ as x),Top (Top_Param.Top, _) -> x
        | Top (Top_Param.Set set, _), (Map _ as x)
        | (Map _ as x), Top (Top_Param.Set set, _) ->
            filter_base (fun v -> is_in_set ~set v) x
        | Map m1, Map m2 ->
            let merge_key k v acc =
              add_or_bottom k (f v (find_or_bottom k m2)) acc in
            Map (M.fold merge_key m1 M.empty)
    in
    let compute_origin_narrow x1 a1 x2 a2 =
      if Top_Param.equal x1 x2 then
        Origin.narrow a1 a2
      else if Top_Param.is_included x1 x2
      then a1
      else if Top_Param.is_included x2 x1
      then a2
      else Origin.top
    in
    (fun x y -> let r = intersect V.narrow compute_origin_narrow x y in
(*     Format.printf "Map_Lattice.narrow %a and %a ===> %a@\n"
       pretty x pretty y pretty r;  *)
     r)




  let widen wh =
    let (_, wh_k_v) = wh in
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
      V.widen (wh_k_v k) v1 v2
    in
    M.generic_merge
      ~cache:("map_Lattice.widen",0)
      ~decide
    in
    fun m1 m2 ->
      match m1, m2 with
      | _ , Top _ -> m2
      | Top _, _ -> assert false (* m2 should be larger than m1 *)
      | Map m1, Map m2 ->
          Map (widen_map m1 m2)

  let equal m1 m2 =
    m1 == m2 ||
      match m1, m2 with
      | Top (s, a), Top (s', a') ->
          Top_Param.equal s s' && Origin.equal a a'
      | Map m1, Map m2 ->
          M.equal m1 m2
      | _ -> false

  let decide_fst _k _v = raise Is_not_included
  let decide_snd _k _v = ()
  let decide_both = V.is_included_exn

  let is_included_exn =
    let map_is_included =
      M.generic_is_included Abstract_interp.Is_not_included
        ~cache:("map_Lattice",2048) ~decide_fst ~decide_snd ~decide_both
    in
    fun m1 m2 ->
      if (m1 != m2)
      then
        (*      Format.printf "begin is_included_exn map_lattice@."; *)
        (match m1,m2 with
         | Top (s,a), Top (s',a') ->
             Top_Param.is_included_exn s s' ;
             Origin.is_included_exn a a'
         | Map _, Top (Top_Param.Top, _) -> ()
         | Map m, Top (Top_Param.Set set, _) ->
             M.iter
               (fun k _ ->
                  if not (is_in_set ~set k)
                  then raise Is_not_included)
               m
         | Top _, Map _ -> raise Is_not_included
         | Map m1, Map m2 -> map_is_included m1 m2)

  let check_is_included_assert = ref 0
  let is_included m1 m2 =
    let new_ = try
      is_included_exn m1 m2; true
    with Is_not_included -> false
    in
    assert
      (let n = succ !check_is_included_assert in
       check_is_included_assert := n;
      n land 63 <> 0 ||
      (let mee = meet m1 m2 in
       let eq = equal mee m1  in
       if (eq <> new_)
       then begin
         Format.printf "Map_Lattice.is_included is wrong. Args: %a(h=%d) %a(h=%d) resultnew = %b meet = %a(h=%d)@."
           pretty m1
           (match m1 with Map m -> M.hash_debug m | _ -> 0)
           pretty m2
           (match m2 with Map m -> M.hash_debug m | _ -> 0)
           new_
           pretty mee
           (match mee with Map m -> M.hash_debug m | _ -> 0);
         false
       end
       else true));
    new_

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

  exception Found_inter

  let intersects = 
    let map_intersects =
      M.generic_symetric_existential_predicate 
	Found_inter 
	~decide_one:(fun _ _ -> ())
	~decide_both:(fun x y -> if V.intersects x y then raise Found_inter)
    in    
    fun mm1 mm2 ->
      match mm1, mm2 with
      | Top (_,_), Top (_,_) -> true
      | Top _, (Map _ as m) | (Map _ as m), Top _ -> not (equal m bottom)
      | Map m1, Map m2 ->
	  try
	    map_intersects m1 m2;
	    false
          with
            Found_inter -> true

  (** if there is only one key [k] in map [m], then returns the pair [k,v]
      where [v] is the value associated to [k].
      @raise Not_found otherwise. *)
  let find_lonely_key m =
    match m with
    | Top _ -> raise Not_found
    | Map m ->
        let elt = ref None in
        let rec check_one k v already_seen =
          if already_seen
          then raise Not_found
          else begin
            elt := Some (k,v); true
          end
        in
        ignore (M.fold check_one m false);
        match !elt with
        | None -> raise Not_found
        | Some v -> v

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

  let splitting_cardinal_less_than ~split_non_enumerable m n =
    match m with
    | Top _ -> raise Not_less_than
    | Map m ->
        M.fold
          (fun _base v card ->
            card +
              (V.splitting_cardinal_less_than ~split_non_enumerable
              v  (n-card) ))
          m
          0

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
        let acc = if Null_Behavior.zone then acc else f K.null acc in
        (Top_Param.O.fold f t acc)
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
        let acc = if Null_Behavior.zone then acc else f K.null V.top acc in
        Top_Param.O.fold
          (fun x acc -> f x V.top acc)
          t
          acc
    | Top(Top_Param.Top, _) ->
        raise Error_Top
    | Map m ->
        M.fold f m acc

  let fold_enum ~split_non_enumerable f m acc =
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
               V.fold_enum ~split_non_enumerable g vl acc)
            m
            acc
        with V.Error_Top -> raise Error_Top

  let fold_enum_by_base f m acc =
    fold_i (fun k v acc -> f (inject k v) acc) m acc

  include Datatype.Make_with_collections
      (struct
        type t = tt
        let name = M.name ^ " map_lattice"
        let structural_descr =
           Structural_descr.Structure
             (Structural_descr.Sum
                [| [| Top_Param.packed_descr; Structural_descr.p_abstract |];
                   [| M.packed_descr |] |])
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

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
