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

open Abstract_interp
open Locations

let msg_emitter = Lattice_messages.register "Lmap";;

module Make_LOffset
  (V: module type of Offsetmap_lattice_with_isotropy)
  (Offsetmap: module type of Offsetmap_sig
              with type v = V.t
              and type widen_hint = V.widen_hint)
  (Default_offsetmap: sig
    val default_offsetmap : Base.t -> [`Bottom | `Map of Offsetmap.t]
  end)
  =
struct

  type v = V.t
  type offsetmap = Offsetmap.t
  type offsetmap_top_bottom = [ `Map of offsetmap | `Bottom | `Top ]
  type widen_hint_base = V.widen_hint

    open Default_offsetmap

    module LBase =  struct

      module Comp =
        struct
          let f _base offsetmap =
            Offsetmap.cardinal_zero_or_one offsetmap
          let compose a b = a && b
          let e = true
          let default = true
        end

      module Initial_Values = struct let v = [ [] ] end

      include Hptmap.Make
      (Base.Base)
      (Offsetmap)
      (Comp)
      (Initial_Values)
      (struct let l = [ Ast.self ] end)
      let () = Ast.add_monotonic_state self


      let add b v m =
        add b v m

      let find_or_default b map =
        try `Map (find b map)
        with Not_found -> default_offsetmap b
    end

    type map = LBase.t
    let shape = LBase.shape
    let clear_caches = LBase.clear_caches

    exception Found_prefix = LBase.Found_prefix

    type lmap =
      | Bottom
      | Top
      | Map of LBase.t

    let equal m1 m2 =
      match m1, m2 with
      | Bottom, Bottom -> true
      | Top, Top -> true
      | Map m1, Map m2 -> m1 == m2
      | _ -> false

    let comp_prefixes m1 m2 =
      match m1, m2 with
      | Map m1, Map m2 -> LBase.comp_prefixes m1 m2
      | _ -> ()

    type subtree = LBase.subtree
    let find_prefix m p =
      match m with
        Map m -> LBase.find_prefix m p
      | Top | Bottom -> None
    let equal_subtree = LBase.equal_subtree
    let hash_subtree = LBase.hash_subtree

    let compare =
      if LBase.compare == Datatype.undefined then Datatype.undefined
      else
        fun m1 m2 -> match m1, m2 with
          | Bottom, Bottom | Top, Top -> 0
          | Map m1, Map m2 -> LBase.compare m1 m2
          | Bottom, (Top | Map _) | Top, Map _ -> -1
          | Map _, (Top | Bottom) | Top, Bottom -> 1

    let empty_map = Map LBase.empty

    let hash = function
      | Bottom -> 457
      | Top -> 458
      | Map m -> LBase.hash m

    let pretty fmt m =
      match m with
        | Bottom -> Format.fprintf fmt "@[NOT ACCESSIBLE@]"
        | Map m ->
          Pretty_utils.pp_iter ~pre:"@[<v>" ~sep:"@ " ~suf:"@]"
            (Extlib.iter_uncurry2 LBase.iter)
            (fun fmt (base, offs) ->
              let typ = Base.typeof base in
              Format.fprintf fmt "@[%a@[%a@]@]" Base.pretty base
                (Offsetmap.pretty_generic ?typ ()) offs)
            fmt m
        | Top -> Format.fprintf fmt "@[NO INFORMATION@]"

    include Datatype.Make_with_collections
        (struct
          type t = lmap
          let structural_descr =
            Structural_descr.t_sum [| [| LBase.packed_descr |] |]
          let name = Offsetmap.name ^ " lmap"
          let reprs = Bottom :: Top :: List.map (fun b -> Map b) LBase.reprs
          let equal = equal
          let compare = compare
          let hash = hash
          let pretty = pretty
          let internal_pretty_code = Datatype.undefined
          let rehash = Datatype.identity
          let copy = Datatype.undefined
          let varname = Datatype.undefined
          let mem_project = Datatype.never_any_project
         end)
    let () = Type.set_ml_name ty None

    let top = Top
    let bottom = Bottom
    let is_top x = equal top x

    let add_base base offsetmap acc =
      match acc with
      | Map acc -> Map (LBase.add base offsetmap acc)
      | Bottom -> Bottom
      | Top -> Top

    let is_empty_map = function
      | Bottom -> false
      | Top -> false
      | Map m -> LBase.is_empty m

  let filter_base f m =
    match m with
      Top -> Top
    | Bottom -> Bottom
    | Map m ->
        Map
          (LBase.fold
             (fun k v acc -> if f k then LBase.add k v acc else acc)
             m
             LBase.empty)

  let filter_by_shape shape = function
    | Top -> Top
    | Bottom -> Bottom
    | Map m -> Map (LBase.inter_with_shape shape m)

  let find_base b mem =
    match mem with
    | Bottom -> `Bottom
    | Top -> `Top
    | Map m -> `Map (LBase.find b m)

  let find_base_or_default b mem =
    match mem with
    | Bottom -> `Bottom
    | Top -> `Top
    | Map m -> (LBase.find_or_default b m :> offsetmap_top_bottom)

  let remove_base (vi:LBase.key) (m:t) =
    match m with
    | Bottom -> m
    | Map m -> Map (LBase.remove vi m)
    | Top -> Top

  let is_reachable t =
    match t with
      Bottom -> false
    | Top | Map _ -> true

  let all_bottom m =
    let f v =
      if not (V.equal V.bottom v) then raise Exit
    in
    try Offsetmap.iter_on_values f m; true
    with Exit -> false

  (* Display only the bases present in [filter], but including those
     that are bound to their default value. *)
  let pretty_filter fmt mm zfilter =
    match mm with
      | Bottom -> Format.fprintf fmt "@[NON TERMINATING FUNCTION@]"
      | Top -> Format.fprintf fmt "@[NO INFORMATION@]"
      | Map m ->
        let first = ref true in
        let filter base _itvs () =
          match LBase.find_or_default base m with
          | `Bottom -> ()
          | `Map offsm ->
	    if not (all_bottom offsm)
	    then begin
              if !first then first := false else Format.fprintf fmt "@ ";
              let typ = Base.typeof base in
              Format.fprintf fmt "@[%a%a@]"
	        Base.pretty base
                (Offsetmap.pretty_generic ?typ ()) offsm
          end
        in
        match zfilter with
          | Zone.Top (Base.SetLattice.Top, _) ->
            pretty fmt mm (* fallback *)
          | _ ->
            Format.fprintf fmt "@[<v>";
            Zone.fold_topset_ok filter zfilter ();
            Format.fprintf fmt "@]"

  (* Reduce validity for read-only bases on which we want to write *)
  let for_writing_validity ~reducing b =
    (* If we are reducing, we do not need 
       to exclude readonly base. *)
    if not reducing && Base.is_read_only b then
      Base.Invalid
    else
      Base.validity b

  let add_new_base base ~size v ~size_v state =
    match state with
    | Bottom -> state
    | Top -> state
    | Map mem ->
        Map (LBase.add base (Offsetmap.create ~size v ~size_v) mem)

  let add_binding ~reducing ~exact initial_mem ({loc; size} as lloc) v =
    (*Format.printf "add_binding: loc:%a@\n" Location_Bits.pretty loc;*)
    if V.equal v V.bottom then false, Bottom
    else
      match initial_mem with
      | Top -> (Locations.is_valid ~for_writing:true lloc), Top
      | Bottom -> false, Bottom
      | Map mem -> begin
        let alarm = ref false in
        let had_non_bottom = ref false in
        let result = ref mem in
        let aux origin b offsets =
          let validity = for_writing_validity ~reducing b in
          match LBase.find_or_default b mem with
          | `Bottom -> alarm := true
          | `Map offm ->
            let offm' =
              match size with
              | Int_Base.Top ->
                let orig = Origin.current Origin.K_Arith in
                alarm := true;
                Offsetmap.update_imprecise_everywhere ~validity orig v offm
              | Int_Base.Value size ->
                assert (Int.gt size Int.zero);
                let this_alarm, r =
                  Offsetmap.update ?origin ~validity ~exact ~offsets ~size v offm
                in
                if this_alarm then alarm := true;
                r
            in
            match offm' with
            | `Bottom -> ()
            | `Map offm' ->
              had_non_bottom := true;
              if offm != offm' then result := LBase.add b offm' !result
        in
        match loc with
        | Location_Bits.Top (Base.SetLattice.Top, orig) ->
          Lattice_messages.emit_approximation msg_emitter
	    "writing at a completely unknown address @[%a@]"
            Origin.pretty_as_reason orig;
          true, top (* the map where every location maps to top *)
        | Location_Bits.Top (Base.SetLattice.Set set, origin) ->
          Base.Hptset.iter (fun b -> aux (Some origin) b Ival.top) set;
          true, (if !had_non_bottom then Map !result else bottom)
        | Location_Bits.Map loc_map ->
          Location_Bits.M.iter (fun b off -> aux None b off) loc_map;
          if !had_non_bottom then (!alarm, Map !result) else (true, bottom)
      end

  let find ?(conflate_bottom=true) mem ({loc ; size} as lloc) =
      match mem with
      | Bottom -> false, V.bottom
      | Top -> (Locations.is_valid ~for_writing:true lloc), V.top
      | Map mem ->
        let alarm = ref false in
        let handle_imprecise_base base acc =
          alarm := true;
          match LBase.find_or_default base mem with
          | `Bottom -> acc
          | `Map offsetmap ->
            let validity = Base.validity base in
            let new_v = Offsetmap.find_imprecise ~validity offsetmap in
            V.join new_v acc
        in
        let v = match loc with
          | Location_Bits.Top (topparam,_orig) ->
               begin try
                   Base.SetLattice.fold
                     handle_imprecise_base
                     topparam
                     (handle_imprecise_base Base.null V.bottom)
                 with Base.SetLattice.Error_Top -> V.top
               end
          | Location_Bits.Map loc_map ->
              begin match size with
              | Int_Base.Top ->
                  begin try
                      Location_Bits.M.fold
                        (fun base _offsetmap acc ->
                          handle_imprecise_base base acc)
                        loc_map
                        V.bottom
                    with Base.SetLattice.Error_Top -> V.top
                  end
              | Int_Base.Value size ->
                let aux_base base offsets acc_v =
                  let validity = Base.validity base in
                  match LBase.find_or_default base mem with
                  | `Bottom ->
                    alarm := true;
                    acc_v
                  | `Map offsetmap ->
                    let alarm_o, new_v =
                      Offsetmap.find
                        ~conflate_bottom ~validity ~offsets ~size offsetmap
                    in
                    if alarm_o then alarm := true;
                    V.join new_v acc_v
                in
                Location_Bits.M.fold aux_base loc_map V.bottom
              end
        in
        !alarm, v

  let join_internal =
    let decide _k v1 v2 = Offsetmap.join v1 v2 in
    (* This [join] works because, currently:
     - during the analysis, we merge maps with the same variables
       (all locals are present)
     - after the analysis, for synthetic results, we merge maps with different
       sets of locals, but do not care about the values of the locals that are
       out-of-scope.
     - for dynamic allocation, the default value for variables is Bottom *)
    let symmetric_merge =
      LBase.join ~cache:(Hptmap_sig.PersistentCache "lmap.join")
        ~symmetric:true ~idempotent:true ~decide
    in
    fun m1 m2 ->
      Map (symmetric_merge m1 m2)

  let join  mm1 mm2 =
    match mm1, mm2 with
      | Bottom,m | m,Bottom -> m
      | Top, _ | _, Top -> Top
      | Map m1, Map m2 ->
          if m1 == m2 then mm1
          else
            join_internal m1 m2

  let narrow_internal =
    let _decide_none base v =
      match default_offsetmap base with
      | `Bottom -> assert false
      | `Map v' -> Offsetmap.narrow v v'
    in
    let decide _k v1 v2 = Offsetmap.narrow v1 v2 in
    let symmetric_merge =
      LBase.join ~cache:(Hptmap_sig.PersistentCache "lmap.narrow")
        ~symmetric:true ~idempotent:true ~decide
    in
    fun m1 m2 ->
      Map (symmetric_merge m1 m2)

  let narrow  mm1 mm2 =
    match mm1, mm2 with
      | Bottom,_ | _,Bottom -> Bottom
      | Top, m | m, Top -> m
      | Map m1, Map m2 ->
          if m1 == m2 then mm1
          else
            narrow_internal m1 m2

  let pretty_diff_aux fmt m1 m2 =
    let print base m1 m2 = match m1, m2 with
      | None, None -> ()
      | Some m, None ->
          let typ = Base.typeof base in
          Format.fprintf fmt "@[L %a@[%a@]@]@ "
            Base.pretty base (Offsetmap.pretty_generic ?typ ()) m
      | None, Some m ->
          let typ = Base.typeof base in
          Format.fprintf fmt "@[R %a@[%a@]@]@ "
            Base.pretty base (Offsetmap.pretty_generic ?typ ()) m
      | Some m1, Some m2 ->
          if not (Offsetmap.equal m1 m2) then
            let typ = Base.typeof base in
            let pp = Offsetmap.pretty_generic ?typ () in
            Format.fprintf fmt "@[%a @[<v>L@[%a@]@,R@[%a@]@]@]@ "
              Base.pretty base pp m1 pp m2
    in
    (* The diff is implemented by performing a merge on the two offsetmaps.
       We _must not_ cache the result, as we are interested only in the side
       effects. *)
    let decide b m1 m2 =
      print b m1 m2;
      (match m1, m2 with (* We need something to return *)
      | Some m, _ | _, Some m -> m
      | None, None -> assert false (* generic_merge invariant *))
    in
    let aux =
      LBase.generic_join ~cache:Hptmap_sig.NoCache
        ~idempotent:true ~symmetric:true ~decide
    in
    Format.fprintf fmt "@[<v>";
    ignore (aux m1 m2);
    Format.fprintf fmt "@]"

  let pretty_diff fmt mm1 mm2 =
    match mm1, mm2 with
      | Bottom, _ -> Format.fprintf fmt "BOT / Not BOT"
      | _, Bottom -> Format.fprintf fmt "Not BOT / BOT"
      | Top, _ -> Format.fprintf fmt "TOP / Not TOP"
      | _, Top -> Format.fprintf fmt "Not TOP / TOP"
      | Map m1, Map m2 ->
          if m1 == m2 then Format.fprintf fmt "Equal"
          else pretty_diff_aux fmt m1 m2

  let is_included =
    let name = Pretty_utils.sfprintf "Lmap(%s).is_included" V.name in
    let decide_fst base v1 =
      match default_offsetmap base with
      | `Bottom -> false
      | `Map vb -> Offsetmap.is_included v1 vb
    in
    let decide_snd base v2 =
      match default_offsetmap base with
      | `Bottom -> true
      | `Map vb -> Offsetmap.is_included vb v2
    in
    let decide_both _ m1 m2 = Offsetmap.is_included m1 m2 in
    let decide_fast s t =
      if s == t then LBase.PTrue (* Inclusion holds *)
      else
        if LBase.compositional_bool t
        (* s is a singleton. We have s \subset t iff s == t *)
        then LBase.PFalse
        else LBase.PUnknown
    in
    let generic_is_included =
      LBase.binary_predicate
        (Hptmap_sig.PersistentCache name) LBase.UniversalPredicate
        ~decide_fast ~decide_fst ~decide_snd ~decide_both
    in
    fun (m1:t) (m2:t) ->
      match m1,m2 with
        | Bottom,_ -> true | _,Bottom -> false
        |   _, Top -> true | Top, _ -> false
        | Map m1', Map m2' -> generic_is_included m1' m2'


  type widen_hint = Base.Set.t * (Base.t -> V.widen_hint)

  (* Precondition : m1 <= m2 *)
  let widen (wh_key_set, wh_hints) r1 r2 =
    match r1,r2 with
    | Top, Top | _, Top -> Top
    | Bottom,Bottom -> Bottom
    | _, Bottom | Top, Map _-> assert false (* thanks to precondition *)
    | Bottom, m -> m
    | Map m1,Map m2 ->
        let widened, something_done =
          Base.Set.fold
            (fun key (widened, something_done) ->
               let offs2 = LBase.find_or_default key m2 in
               let offs1 = LBase.find_or_default key m1 in
               match offs1, offs2 with
               | `Bottom, _ | _, `Bottom ->
                 assert false (* cannot be invalid and bound *)
               | `Map offs1, `Map offs2 ->
               let unchanged = Offsetmap.equal offs2 offs1 in
                 (* Format.printf "key=%a, fixed=%b@."
                    Base.pretty key fixed; *)
                 if unchanged
                 then (widened, something_done)
                 else
                   let new_off = Offsetmap.widen (wh_hints key) offs1 offs2 in
                   (LBase.add key new_off widened, true)
            ) wh_key_set (m2, false)
        in
        if something_done then
          Map widened
        else
          let decide base off1 off2 = Offsetmap.widen (wh_hints base) off1 off2 in
          Map (LBase.join ~cache:Hptmap_sig.NoCache
                 ~symmetric:false ~idempotent:true ~decide m1 m2)

  let paste_offsetmap ~reducing ~from ~dst_loc ~size ~exact m =
    match m with
    | Bottom -> false, m
    | Top ->
      let loc = make_loc dst_loc (Int_Base.inject size) in
      (Locations.is_valid ~for_writing:true loc), m
    | Map m' ->
        let loc_dst = make_loc dst_loc (Int_Base.inject size) in
        assert (Int.lt Int.zero size);
        let exact = exact && cardinal_zero_or_one loc_dst in
        (* TODO: do we want to alter exact here? *)
        let had_non_bottom = ref false in
        let alarm = ref false in
        let treat_dst base_dst i_dst acc =
          let validity = for_writing_validity ~reducing base_dst in
          let offsetmap_dst = LBase.find_or_default base_dst m' in
          match offsetmap_dst with
          | `Bottom ->
            alarm := true;
            acc
          | `Map offsetmap_dst ->
            let this_alarm, new_offsetmap =
              Offsetmap.paste_slice ~validity ~exact
                ~from ~size ~offsets:i_dst offsetmap_dst
            in
            alarm := !alarm || this_alarm;
	    had_non_bottom := true;
            match new_offsetmap with
            | `Bottom -> acc
            | `Map new_offsetmap ->
              if offsetmap_dst != new_offsetmap then
                LBase.add base_dst new_offsetmap acc
              else acc
	in
        match dst_loc with
        | Location_Bits.Map _ ->
          let result = Location_Bits.fold_i treat_dst dst_loc m' in
          if !had_non_bottom then !alarm, Map result else true, bottom

        | Location_Bits.Top (top, orig) ->
          if not (Base.SetLattice.equal top Base.SetLattice.top) then
            Lattice_messages.emit_approximation msg_emitter
              "writing somewhere in @[%a@]@[%a@]."
              Base.SetLattice.pretty top
              Origin.pretty_as_reason orig;
          let validity = Base.Known (Int.zero, Int.pred size) in
          let v = Offsetmap.find_imprecise ~validity from in
          add_binding ~reducing:false ~exact:false m loc_dst v

  let copy_offsetmap src_loc size mm =
    match mm with
      | Bottom -> false, `Bottom
      | Top ->
        let loc = make_loc src_loc (Int_Base.inject size) in
        (Locations.is_valid ~for_writing:false loc), `Top
      | Map m ->
          let alarm = ref false in
          try
            begin
              let treat_src k_src i_src acc =
                let validity = Base.validity k_src in
                match LBase.find_or_default k_src m with
                | `Bottom ->
                  alarm := true;
                  acc
                | `Map offsetmap_src ->
                  let alarm_copy, copy = Offsetmap.copy_slice ~validity
                    ~offsets:i_src ~size offsetmap_src
                  in
                  if alarm_copy then alarm := true;
                  Offsetmap.join_top_bottom acc copy
              in
              let r = Location_Bits.fold_i treat_src src_loc `Bottom in
              !alarm, r
            end
          with
            | Location_Bits.Error_Top (* from Location_Bits.fold *) ->
              let loc = make_loc src_loc (Int_Base.inject size) in
              let alarm, v = find ~conflate_bottom:false mm loc in
              alarm, `Map (Offsetmap.create ~size ~size_v:size v)

  let fold f m acc =
    LBase.fold (fun k off acc -> f k off acc) m acc

  let iter = LBase.iter

  let cached_fold ~f ~cache_name ~temporary ~joiner ~empty =
    let cached_f = LBase.cached_fold ~f ~cache_name ~temporary ~joiner ~empty in
    fun m -> cached_f m

  let cached_map ~f ~cache ~temporary =
    let cached_f = LBase.cached_map ~f ~cache ~temporary in
    function
      | Top -> Top
      | Bottom -> Bottom
      | Map mm -> Map (cached_f mm)

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
