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

open Abstract_interp
open Locations
open CilE


module Make_LOffset
  (V: module type of Offsetmap_lattice_with_isotropy)
  (Offsetmap: module type of Offsetmap_sig
              with type v = V.t
              and type widen_hint = V.widen_hint)
  (Default_offsetmap: sig val default_offsetmap : Base.t -> Offsetmap.t end)
  =
struct

  type v = V.t
  type offsetmap = Offsetmap.t
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
        if Offsetmap.equal v (default_offsetmap b) then
          remove b m
        else add b v m

      let find_or_default b map =
        try find b map
        with Not_found -> default_offsetmap b
    end

    let clear_caches = LBase.clear_caches
      

    exception Found_prefix = LBase.Found_prefix

    type tt =
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
                (Offsetmap.pretty_typ typ) offs)
            fmt m
        | Top -> Format.fprintf fmt "@[NO INFORMATION@]"

    include Datatype.Make_with_collections
        (struct
          type t = tt
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

    exception Error_Bottom

    let add_base base offsetmap acc =
      match acc with
      | Map acc -> Map (LBase.add base offsetmap acc)
      | Bottom -> raise Error_Bottom
      | Top -> Top

    let is_empty_map = function
        Bottom -> assert false
      | Top -> assert false
      | Map m -> LBase.is_empty m

  let filter_base f m =
    match m with
      Top -> Top
    | Bottom -> assert false
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

  let find_base (vi:LBase.key) (m:t) =
    match m with
      | Bottom -> raise Not_found
      | Map m -> LBase.find vi m
      | Top -> Offsetmap.empty

  let remove_base (vi:LBase.key) (m:t) =
    match m with
    | Bottom -> m
    | Map m -> Map (LBase.remove vi m)
    | Top -> assert false

  let is_reachable t =
    match t with
      Bottom -> false
    | Top | Map _ -> true

  let all_bottom m =
    let f v _ =
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
          let offsm = LBase.find_or_default base m in
	  if not (all_bottom offsm)
	  then begin
            if !first then first := false else Format.fprintf fmt "@ ";
            Format.fprintf fmt "@[%a%a@]"
	      Base.pretty base (Offsetmap.pretty_typ (Base.typeof base)) offsm
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

  let add_binding_offsetmap ~with_alarms ~reducing ~exact varid offsets size v map =
    let validity = for_writing_validity ~reducing varid in
    if validity = Base.Invalid then raise Offsetmap.Result_is_bottom;
    match size with
      | Int_Base.Top ->
          let offsm = LBase.find_or_default varid map in
          let orig = Origin.current Origin.K_Arith in
          let new_offsm =
            Offsetmap.update_imprecise_everywhere ~validity orig v offsm
          in
          if offsm == new_offsm then map
          else LBase.add varid new_offsm map

      | Int_Base.Value size ->
          assert (Int.gt size Int.zero);
          let offsetmap_orig = LBase.find_or_default varid map in
          (*Format.printf "add_binding_offsetmap varid:%a offset:%a@\n"
            Base.pretty varid Ival.pretty offsets;*)
          let new_offsetmap =
            Offsetmap.update ~with_alarms ~validity
              ~exact ~offsets ~size v offsetmap_orig
          in
          if offsetmap_orig == new_offsetmap then map
          else LBase.add varid new_offsetmap map

  let add_new_base base ~size v ~size_v state =
    match state with
    | Bottom -> state
    | Top -> state
    | Map mem ->
        Map (LBase.add base (Offsetmap.create ~size v ~size_v) mem)

  let add_binding ~with_alarms ~reducing ~exact initial_mem {loc=loc ; size=size } v =
    (*Format.printf "add_binding: loc:%a@\n" Location_Bits.pretty loc;*)
    if V.equal v V.bottom then Bottom else
      match initial_mem with
      | Top -> Top
      | Bottom -> Bottom
      | Map mem ->
          let result =
            (match loc with
             | Location_Bits.Top (Base.SetLattice.Top, orig) ->
               CilE.do_warn with_alarms.imprecision_tracing
		 (fun _ -> 
                   Kernel.warning ~current:true ~once:true
		     "writing at a completely unknown address @[%a@]@\n\
                        Aborting." Origin.pretty_as_reason orig
                 );
               warn_mem_write with_alarms;
                 (* Format.printf "dumping memory : %a@\n" pretty initial_mem;*)
               top (* the map where every location maps to top *)
             | Location_Bits.Top (Base.SetLattice.Set set, origin) ->
                 (* Always emit an alarm, we emit out-of-bounds anyway *)
                 warn_mem_write with_alarms;
                 let had_non_bottom = ref false in
                 let treat_base b acc =
                   let validity = for_writing_validity ~reducing b in
                   match validity with
                     | Base.Invalid -> acc
                     | Base.Unknown _ | Base.Known _ | Base.Periodic _ ->
                       let offsm = LBase.find_or_default b mem in
                       try
                         let offsetmap =
                           Offsetmap.update_imprecise_everywhere
                             ~validity origin v offsm
                         in
                         had_non_bottom := true;
                         LBase.add b offsetmap acc
                       with Offsetmap.Result_is_bottom -> acc
                 in
                 let result =
                   let after_null = treat_base Base.null mem in
                   let r = Base.Hptset.fold treat_base set after_null in
                   if !had_non_bottom then Map r else bottom
                 in
 (* Format.printf "debugging add_binding topset, loc =%a, result=%a@."
                                Location_Bits.pretty loc
                                pretty result; *)
                 result
             | Location_Bits.Map loc_map ->
                 (* Format.printf "add_binding size:%a@\n"
                    Int_Base.pretty size;*)
                 let had_non_bottom = ref false in
                 let result = Location_Bits.M.fold
                   (fun varid offsets map ->
                      try
                        let r =
                          add_binding_offsetmap
                            ~with_alarms
                            ~reducing
                            ~exact
                            varid
                            offsets
                            size
                            v
                            map
                        in
                        had_non_bottom := true;
                        r
                      with Offsetmap.Result_is_bottom ->
                        CilE.warn_mem_write with_alarms;
                        map)
                   loc_map
                   mem
                 in
                 if !had_non_bottom
                 then Map result
                 else begin
                   (do_warn with_alarms.imprecision_tracing
                    (* another field would be appropriate here TODO *)
		      (fun _ -> Kernel.warning ~current:true ~once:true
                        "all target addresses were invalid. This path is \
assumed to be dead."));
                   bottom
                 end)
          in
          result

  let find_base_or_default base mem =
    match mem with
      Map mem -> LBase.find_or_default base mem
    | Top -> Offsetmap.empty
    | Bottom -> assert false 

  let find ~with_alarms ~conflate_bottom mem { loc = loc ; size = size } =
    let result =
      match mem with
      | Bottom -> V.bottom
      | Top | Map _ ->
          let handle_imprecise_base base acc =
            let validity = Base.validity base in
            CilE.warn_mem_read with_alarms;
            let offsetmap = find_base_or_default base mem in
            let new_v = Offsetmap.find_imprecise ~validity offsetmap in
            V.join new_v acc
          in
          begin match loc with
          |  Location_Bits.Top (topparam,_orig) ->
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
                  Location_Bits.M.fold
                    (fun base offsets acc ->
                      let validity = Base.validity base in
                      let offsetmap = find_base_or_default base mem in
                      (*Format.printf "offsetmap(%a):%a@\noffsets:%a@\nsize:%a@\n"
                        Base.pretty base
                        (Offsetmap.pretty None) offsetmap
                        Ival.pretty offsets
                        Int.pretty size;*)
                      let new_v =
                        Offsetmap.find
                          ~conflate_bottom
                          ~validity
                          ~with_alarms
                          ~offsets
                          ~size
                          offsetmap
                      in
                      (* Format.printf "find got:%a@\n" V.pretty new_v; *)
                      V.join new_v acc)
                    loc_map
                    V.bottom
              end
          end
    in
    result

  let join_internal =
    let decide_none base v1 = Offsetmap.join v1 (default_offsetmap base) in
    let decide_some v1 v2 = Offsetmap.join v1 v2 in
    let symmetric_merge =
      LBase.symmetric_merge ~cache:("lmap", ()) ~decide_none ~decide_some
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

  let pretty_diff_aux fmt m1 m2 =
    let print base m1 m2 = match m1, m2 with
      | None, None -> ()
      | Some m, None ->
          let typ = Base.typeof base in
          Format.fprintf fmt "@[L %a@[%a@]@]@ "
            Base.pretty base (Offsetmap.pretty_typ typ) m
      | None, Some m ->
          let typ = Base.typeof base in
          Format.fprintf fmt "@[R %a@[%a@]@]@ "
            Base.pretty base (Offsetmap.pretty_typ typ) m
      | Some m1, Some m2 ->
          if not (Offsetmap.equal m1 m2) then
            let typ = Base.typeof base in
            let pp = Offsetmap.pretty_typ typ in
            Format.fprintf fmt "@[%a @[<v>L@[%a@]@,R@[%a@]@]@]@ "
              Base.pretty base pp m1 pp m2
    in
    (* The diff is implemented by performing a merge on the two offsetmaps.
       We _must not_ cache the result, as we are interested only in the side
       effects. *)
    let decide b m1 m2 = print b m1 m2; Offsetmap.empty in
    let aux = LBase.generic_merge ~idempotent:true ~cache:("", false) ~decide in
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
      Offsetmap.is_included v1 (default_offsetmap base)
    in
    let decide_snd base v2 =
      Offsetmap.is_included (default_offsetmap base) v2
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
        (LBase.PersistentCache name) LBase.UniversalPredicate
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
    | Top, _ | _, Top -> assert false
    | Bottom,Bottom -> Bottom
    | _,Bottom -> assert false (* thanks to precondition *)
    | Bottom, m -> m
    | Map m1,Map m2 ->
        let widened, something_done =
          Base.Set.fold
            (fun key (widened, something_done) ->
               let offs2 = LBase.find_or_default key m2 in
               let offs1 = LBase.find_or_default key m1 in
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
          let merge base v1 v2 = match v1, v2 with
            | None, None -> assert false (* generic_merge invariant *)
            | Some _, None -> assert false (* by precondition *)
            | None, Some off -> off
            | Some off1, Some off2 ->
              if off1 == off2 then off1
              else
                Offsetmap.widen (wh_hints base) off1 off2
          in
          Map (LBase.generic_merge
                 ~idempotent:true ~cache:("", false) ~decide:merge m1 m2)

  let paste_offsetmap ~with_alarms ~from ~dst_loc ~start ~size ~exact m =
    match m with
    | Bottom | Top -> m
    | Map m' ->
        let loc_dst = make_loc dst_loc (Int_Base.inject size) in
        assert (Int.lt Int.zero size);
        let loc_dst' = Locations.valid_part ~for_writing:true loc_dst in
        if not (Location.equal loc_dst loc_dst') then
          CilE.warn_mem_write with_alarms;
        let loc_dst = loc_dst' in
        let exact = exact && cardinal_zero_or_one loc_dst in
        (* TODO: do we want to alter exact here? *)
        let had_non_bottom = ref false in
        let treat_dst base_dst i_dst acc =
          let validity = for_writing_validity ~reducing:false base_dst in
          if validity <> Base.Invalid then
            let offsetmap_dst = LBase.find_or_default base_dst m' in
            try
              let new_offsetmap =
                Offsetmap.paste_slice ~with_alarms ~validity ~exact
                  (from, start) ~size ~offsets:i_dst offsetmap_dst
              in
	      had_non_bottom := true;
              if offsetmap_dst != new_offsetmap then
                LBase.add base_dst new_offsetmap acc
              else acc
            with Offsetmap.Result_is_bottom ->
	      CilE.warn_mem_write with_alarms;
              acc
          else (
	    CilE.warn_mem_write with_alarms;
            acc
          ) 
	in
        match dst_loc with
          | Location_Bits.Map _ ->
              let result = Location_Bits.fold_i treat_dst dst_loc m' in
              if !had_non_bottom then Map result
              else begin
                Kernel.warning ~once:true ~current:true
                  "all target addresses were invalid. This path is assumed to \
                    be dead.";
                bottom
              end

          | Location_Bits.Top (top, orig) ->
              if not (Base.SetLattice.equal top Base.SetLattice.top) then
                Kernel.result ~current:true ~once:true
                  "writing somewhere in @[%a@]@[%a@]."
                  Base.SetLattice.pretty top
                  Origin.pretty_as_reason orig;
              let src_end = Int.pred (Int.add start size) in
              let validity = Base.Known (start, src_end) in
              let v = Offsetmap.find ~with_alarms:CilE.warn_none_mode
                ~validity ~conflate_bottom:false
                ~offsets:(Ival.inject_singleton start) ~size from
              in
              add_binding ~with_alarms ~reducing:false ~exact:false m loc_dst v

  let copy_offsetmap ~with_alarms src_loc mm =
    match mm with
      | Bottom -> None
      | Top -> Some Offsetmap.empty
      | Map m ->
        try
          let size = Int_Base.project src_loc.size in
          try
            begin
              let treat_src k_src i_src (acc : Offsetmap.t option) =
                let validity = Base.validity k_src in
                let offsetmap_src = LBase.find_or_default k_src m in
                if Offsetmap.is_empty offsetmap_src then (
                  CilE.warn_mem_read with_alarms;
                  acc)
                else
                  let copy = Offsetmap.copy_slice ~with_alarms ~validity
                    ~offsets:i_src ~size offsetmap_src
                  in
                  if Offsetmap.is_empty copy then  (
                    CilE.warn_mem_read with_alarms;
                    acc)
                  else
                    match acc with
                      | None -> Some copy
                      | Some acc ->
                        Some ((Offsetmap.join copy acc))
              in
              Location_Bits.fold_i treat_src src_loc.loc None
            end
          with
            | Location_Bits.Error_Top (* from Location_Bits.fold *) ->
              let v = find ~conflate_bottom:false ~with_alarms mm src_loc in
              Some (Offsetmap.create ~size ~size_v:size v)
        with
          | Int_Base.Error_Top  (* from Int_Base.project *) ->
            Some Offsetmap.empty

  let fold_base f m acc =
    match m with
      | Bottom -> raise Error_Bottom
          | Top -> assert false
  | Map m ->
          LBase.fold
            (fun k _ acc -> f k acc)
            m
            acc

  let fold_base_offsetmap f m acc =
    match m with
      | Top -> assert false
      | Bottom -> raise Error_Bottom
      | Map m ->
          LBase.fold
            (fun k off acc -> f k off acc)
            m
            acc

  let cached_fold ~f ~cache_name ~temporary ~joiner ~empty =
    let cached_f = LBase.cached_fold ~f ~cache_name ~temporary ~joiner ~empty
    in
    function
      | Top -> assert false
      | Bottom -> raise Error_Bottom
      | Map mm ->
         (cached_f mm)


  let cached_map ~f ~cache ~temporary =
    let cached_f = LBase.cached_map ~f ~cache ~temporary
    in
    function
        Bottom -> Bottom
      | Top -> assert false
      | Map mm ->
         Map (cached_f mm)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
