(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
open Lattice_Interval_Set
open Locations

exception Bitwise_cannot_copy

module type Location_map_bitwise = sig

  type y
  include Datatype.S

  module LOffset: sig
    include Datatype.S
    val map: ((bool * y) -> (bool * y)) -> t -> t
    val fold :
      (Int_Intervals.t -> bool * y -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_fuse_same :
      (Int_Intervals.t -> bool * y -> 'a -> 'a) -> t -> 'a -> 'a
    val join: t -> t -> t
    val pretty_with_type:
      Cil_types.typ option -> Format.formatter -> t -> unit
    val collapse : t -> y
    val empty : t
    val degenerate: y -> t
    val is_empty: t->bool
    val add_iset : exact:bool -> Int_Intervals.t -> y -> t -> t
  end

  val empty : t
  val bottom: t
  val is_empty : t -> bool
  val is_bottom : t -> bool
  val top : t
  val join : t -> t -> t

  val is_included : t -> t -> bool
  val add_binding : exact:bool -> t -> Zone.t -> y -> t
  val map_and_merge : (y -> y) -> t -> t -> t
  val filter_base : (Base.t -> bool) -> t -> t
  val find : t -> Zone.t -> y
  val find_base: t -> Zone.t -> LOffset.t

  exception Cannot_fold

  val uninitialize: Cil_types.varinfo list -> t -> t

  val fold : (Zone.t -> bool * y -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_base : (Base.t -> LOffset.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_fuse_same : (Zone.t -> bool * y -> 'a -> 'a) -> t -> 'a -> 'a
  val map2 : ((bool * y) option -> (bool * y) option -> bool * y)
      -> t -> t -> t
  val copy_paste :
    with_alarms:CilE.warn_mode ->
    f:(bool * y -> bool * y) ->
    location -> location -> t -> t

  val clear_caches: unit -> unit
end

module type With_default = sig
  include Lattice
  val default : Base.t -> Int.t -> Int.t -> t
  val defaultall : Base.t ->  t
end

module Make_bitwise (V:With_default) = struct

  module LOffset = struct
    include Offsetmap_bitwise.Make(V)
    let real_copy = copy
    let copy = Datatype.undefined
  end

  module LBase = struct
    include Hptmap.Make(Base)(LOffset)(Hptmap.Comp_unused)(struct let v = [[]] end)(struct let l = [ Ast.self ] end)
    let () = Ast.add_monotonic_state self
    let find_or_default base m =
      try find base m with Not_found -> LOffset.empty
  end
  let clear_caches = LBase.clear_caches

  type tt = Top | Map of LBase.t | Bottom
  type y = V.t
  let empty = Map LBase.empty
  let bottom = Bottom

  exception Cannot_fold

  let hash = function
    | Top -> 0
    | Bottom -> 17
    | Map x -> LBase.hash x

  let equal a b = match a,b with
    | Top,Top -> true
    | Map m1, Map m2 -> LBase.equal m1 m2
    | Bottom, Bottom -> true
    | (Top | Bottom | Map _),  _ -> false

  let is_empty x = equal empty x
  let is_bottom x = x = Bottom

  let top = Top

  let pretty fmt m =
    match m with
    | Top -> Format.fprintf fmt "@[<v>FROMTOP@]"
    | Bottom -> Format.fprintf fmt "@[<v>UNREACHABLE_B@]"
    | Map m ->
        Format.fprintf fmt "@[<v>";
        (LBase.iter
          (fun base offs ->
             Format.fprintf fmt "%a@[<v>%a@]@,"
               Base.pretty base
               (LOffset.pretty_with_type (Base.typeof base))
               offs
          )
          m);
        Format.fprintf fmt "@]"

  include Datatype.Make
      (struct
        type t = tt
        let reprs = Top :: List.map (fun b -> Map b) LBase.reprs
        let structural_descr =
          Structural_descr.Structure
            (Structural_descr.Sum [| [| LBase.packed_descr |] |])
         let name = LOffset.name ^ " lmap_bitwise"
        let hash = hash
        let equal = equal
        let compare = Datatype.undefined
        let pretty = pretty
        let internal_pretty_code = Datatype.undefined
        let rehash = Datatype.identity
        let copy = Datatype.undefined
        let varname = Datatype.undefined
        let mem_project = Datatype.never_any_project
       end)

  let fold f m acc =
    match m with
      | Top
      | Bottom -> acc
      | Map m ->
          LBase.fold
            (fun k offsetmap acc ->
               LOffset.fold
                 (fun itvs v acc ->
                    let z = Zone.inject k itvs in
                      f z v acc)
                 offsetmap
                 acc)
            m
            acc

 let fold_base f m acc=
    match m with
    | Bottom
    | Top -> raise Cannot_fold
    | Map m -> LBase.fold f m acc

  let fold_fuse_same f m acc =
    let f' b offs acc =
      LOffset.fold_fuse_same
        (fun itvs v acc -> f (Zone.inject b itvs) v acc)
        offs acc
    in
    fold_base f' m acc

 let add_binding ~exact m (loc:Zone.t) v  =
   match loc, m with
   | Zone.Top (Base.SetLattice.Top, _),_|_,Top -> Top
   | Zone.Top (Base.SetLattice.Set s, _), Map m ->
       let result =
         let treat_base base acc =
           let offsetmap_orig =
             try
               LBase.find base m
             with Not_found ->
               LOffset.empty
           in
           let new_offsetmap =
             LOffset.add_iset ~exact Int_Intervals.top v offsetmap_orig
           in
           LBase.add base new_offsetmap acc
         in
         Base.Hptset.fold treat_base s (treat_base Base.null m)
       in Map result
   | Zone.Map _, Map m ->
       let result =
         let treat_offset varid offs m =
           let offsetmap_orig =
             try
               LBase.find varid m
             with Not_found ->
               LOffset.empty
           in
           let new_offsetmap =
             LOffset.add_iset ~exact offs v offsetmap_orig
           in LBase.add varid new_offsetmap m
         in
         Zone.fold_i treat_offset loc m
       in Map result
   | _, Bottom -> assert false

 let join m1 m2 =
   let result = match m1, m2 with
   | Top, _ | _, Top -> Top
   | Bottom, m | m, Bottom -> m
   | Map m1, Map m2 ->
       let treat_base varid offsmap1 acc =
         let offsmap =
         try
           let offsmap2 = LBase.find varid m2 in
           LOffset.join offsmap1 offsmap2
         with Not_found ->
           LOffset.joindefault offsmap1
         in
         LBase.add varid offsmap acc
       in
       let all_m1 = LBase.fold treat_base m1 LBase.empty in
       let result =
         LBase.fold
           (fun varid offsmap2 acc ->
              try
                ignore (LBase.find varid m1);
                acc
              with Not_found ->
                LBase.add
                  varid
                  (LOffset.joindefault offsmap2)
                  acc)
           m2
           all_m1
       in
       Map result
   in
   (*Format.printf "JoinBitWise: m1=%a@\nm2=%a@\nRESULT=%a@\n"
     pretty m1
     pretty m2
     pretty result;*)
   result

  let map2 f m1 m2 =
    match m1, m2 with
      | Top, _ | _, Top ->
          Top
      | Map m1, Map m2 ->
         let treat_base varid offsmap1 acc =
           let offsmap_result =
             try
               let offsmap2 = LBase.find varid m2 in
                 LOffset.map2 f offsmap1 offsmap2
             with Not_found ->
               LOffset.map (fun x -> f (Some x) None) offsmap1

           in
             LBase.add varid offsmap_result acc
         in
         let all_m1 = LBase.fold treat_base m1 LBase.empty in
         let result =
           LBase.fold
             (fun varid offsmap2 acc ->
                try
                  ignore (LBase.find varid m1);
                  acc
                with Not_found ->
                  let offsetmap =
                    LOffset.map (fun x -> f None (Some x)) offsmap2
                  in
                    LBase.add varid offsetmap acc)
             m2
             all_m1
         in
         Map result
      | Bottom, Bottom -> Bottom
      | Bottom, Map m ->
          Map (LBase.fold
                 (fun base offs acc ->
                    let offs = LOffset.map (fun x -> f None (Some x)) offs in
                    LBase.add base offs acc)
                 m LBase.empty)
      | Map m, Bottom ->
          Map (LBase.fold
                 (fun base offs acc ->
                    let offs = LOffset.map (fun x -> f (Some x) None) offs in
                    LBase.add base offs acc)
                 m LBase.empty)

          

 let is_included m1 m2 =
   match m1, m2 with
    | _, Top -> true
    | Top ,_ -> false
    | Bottom, _ -> true
    | _, Bottom -> false
    | Map m1, Map m2 ->
        let treat_offset1 varid offs1  =
        let offs2 =
          try
            LBase.find varid m2
          with Not_found -> LOffset.empty
        in
        LOffset.is_included_exn offs1 offs2
        in
       let treat_offset2 varid offs2  =
        try
          ignore (LBase.find varid m1); ()
        with Not_found ->
          LOffset.is_included_exn LOffset.empty offs2
       in
         try
           LBase.iter treat_offset1 m1;
           LBase.iter treat_offset2 m2;
           true
         with
             Is_not_included -> false
(*
  let join x y =
    let r1 = join x y in
    let r2 =
      map2
        (fun x y ->
           match x,y with
             | Some (bx, x), Some (by, y) -> bx || by, V.join x y
             | Some (_, x), None | None, Some (_, x) -> true, x
             | None, None -> assert false)
        x y
    in
    if not (is_included r1 r2 && is_included r2 r1)
    then begin
      Format.printf "Warning: Joining '%a' and '%a' to '%a' /// '%a'@."
        pretty x pretty y pretty r1 pretty r2;
    end;
    r1
*)

 let map_and_merge f (m_1:t) (m_2:t) =
   match m_1,m_2 with
   | Top,_ | _, Top -> Top
   | Bottom, Bottom -> Bottom
   | Bottom, Map _ -> m_2
   | Map m, Bottom ->
       Map
         (LBase.fold
            (fun b m acc ->
               let m = LOffset.map (fun (b, v) -> b, f v) m in
               LBase.add b m acc
            ) m LBase.empty)
   | Map m1, Map m2 ->
       let result = LBase.fold
         (fun k1 v1 acc ->
(*            Format.printf "HERE :%a %a@\n" Base.pretty k1 (LOffset.pretty) v1; *)
            let new_v = try
              let v2 = LBase.find k1 m2 in
              LOffset.map_and_merge f v1 v2
            with Not_found ->
              let result = LOffset.map (fun (d,v) -> d,f v) v1 in
              result
            in
(*            Format.printf "RESULT:%a %a@\n" Base.pretty k1 (LOffset.pretty) new_v; *)
            LBase.add k1 new_v acc)
         m1
         m2
       in
       let result = Map result in
(*       Format.printf "map_and_merge %a and %a RESULT:%a @."
         pretty m_1
         pretty m_2
         pretty result;
*)
       result

 let filter_base f m =
   match m with
   | Top -> Top
   | Bottom -> Bottom
   | Map m ->
       let result =
         LBase.fold (fun k v acc -> if f k then LBase.add k v acc else acc)
           m
           LBase.empty
       in
       Map result

 let uninitialize locals m =
   match m with
     | Top -> Top
     | Bottom -> Bottom
     | Map m ->
         let result =
           List.fold_left
             (fun acc v ->
                let base = Base.create_varinfo v in
                let (i1,i2) =
                  match Base.validity base with
                  | Base.Invalid -> assert false (* map should be empty *)
                  | Base.Periodic(i1, _, p) ->
                      assert (Int.is_zero i1);
                      i1, Int.pred p
                  | Base.Unknown (i1,_,i2) | Base.Known(i1,i2) -> (i1,i2)
                in
                if Int.lt i2 i1 then assert false (* not supposed to happen
                                                     for a local *)
                else
                  let offset = LOffset.add (i1,i2) V.bottom LOffset.empty
                  in LBase.add base offset acc)
             m locals
         in Map result


 let find_base m loc =
   match loc, m with
     | Zone.Top _, _ | _, (Top | Bottom) -> LOffset.empty
     | Zone.Map _, Map m ->
         let treat_offset varid offs acc =
           let default = V.default varid in
           let offsetmap =
             try
               LBase.find varid m
             with Not_found -> LOffset.empty
           in
           LOffset.add_iset ~exact:true
             offs
             (LOffset.find_iset default (V.defaultall varid) offs offsetmap)
             acc
         in  Zone.fold_i treat_offset loc LOffset.empty

 let find m loc =
   match loc, m with
     | Zone.Top _, _ | _, Top -> V.top
     | _, Bottom -> V.bottom
     | Zone.Map _, Map m ->
         let treat_offset varid offs acc =
           let default = V.default varid in
           let offsetmap =
             try
               LBase.find varid m
             with Not_found ->
               LOffset.empty
           in
             V.join
               (LOffset.find_iset default (V.defaultall varid) offs offsetmap)
               acc
         in
         Zone.fold_i treat_offset loc V.bottom

  let copy_offsetmap ~f src_loc m =
    let result =
      begin
        begin
          try
            let size = Int_Base.project src_loc.size in
            begin
              let treat_src k_src i_src (acc : LOffset.t option) =
                let validity = Base.validity k_src in
                try
                  let offsetmap_src = LBase.find_or_default k_src m in
(*                  Format.printf
                    "copy_offsetmap/treat_src k_src:%a i_src:%a@\n"
                    Base.pretty k_src
                    Ival.pretty i_src;*)
                  ignore (Ival.cardinal_less_than i_src 100);
                  Ival.fold
                    (fun start acc ->
                      let stop = Int.pred (Int.add start size) in
                      match validity with
                      | Base.Periodic _ ->
                          raise Bitwise_cannot_copy
                      | Base.Invalid -> acc
                      | (Base.Known (b,e) | Base.Unknown (b,_,e)) when Int.lt start b
                            || Int.gt stop e ->
                          acc
                      | Base.Known _ | Base.Unknown _ ->
                          let default = V.default k_src in
                          let copy =
                            LOffset.real_copy ~f:(Some (f, default))
                              offsetmap_src start stop
                          in
                          let r = match acc with
                          | None -> Some copy
                          | Some acc -> let r = LOffset.join copy acc in
                                        if LOffset.is_empty r then
                                          raise Not_found;
                            Some r
                          in r)
                    i_src
                    acc
                with
                | Not_found  (* from [LOffset.is_empty] *)
                  ->
                    (*CilE.warn_once "reading top in @[%a@]. Look above for origin."
                               Location_Bits.pretty src_loc.loc;*)
                    Some LOffset.empty
                | Not_less_than (* from [Ival.cardinal_less_than] *)->
                    (*ignore (CilE.warn_once "approximating lval assignment");*)
                    raise Bitwise_cannot_copy
              in
              try
                Extlib.the (Location_Bits.fold_i treat_src src_loc.loc None)
              with Location_Bits.Error_Top ->
                (*CilE.warn_once "reading unknown location(2)@ @[%a@]"
                           Location_Bits.pretty src_loc.loc;*)
                LOffset.empty
            end
          with
          | Location_Bits.Error_Top (* from Location_Bits.fold *)
          | Not_less_than (* from Ival.cardinal_less_than *)
          | Int_Base.Error_Top  (* from Int_Base.project *)
          | Ival.Error_Top (* from Ival.fold *) ->
              LOffset.empty

        end
      end
    in
(*    Format.printf "copy_offsetmap: m:%a src:%a result:%a@\n"
      pretty m
      Locations.pretty src_loc
      pretty result;*)
    result


  let paste_offsetmap ~with_alarms map_to_copy dst_loc start size m =
    let dst_is_exact =
      Locations.valid_cardinal_zero_or_one ~for_writing:true
        (Locations.make_loc dst_loc (Int_Base.inject size))
    in
    let stop = Int.pred (Int.add start size) in
    let had_non_bottom = ref false in
    let plevel = !Lattice_Interval_Set.plevel in
    let treat_dst k_dst i_dst (acc_lmap : LBase.t) =
      if Base.is_read_only k_dst
      then acc_lmap
      else
        let validity = Base.validity k_dst in
        let offsetmap_dst = LBase.find_or_default k_dst m in
        let new_offsetmap =
          try
            ignore
              (Ival.cardinal_less_than i_dst plevel);
            Ival.fold
              (fun start_to acc ->
                let stop_to = Int.pred (Int.add start_to size) in
                match validity with
                | Base.Periodic _ ->
                    raise Bitwise_cannot_copy
                | Base.Known (b,e) | Base.Unknown (b,_,e) 
		      when Int.lt start_to b || Int.gt stop_to e ->
                    CilE.warn_mem_write with_alarms;
                    acc
                | Base.Invalid ->
                    CilE.warn_mem_write with_alarms;
                    acc
                | Base.Known _ | Base.Unknown _ ->
                    had_non_bottom := true;
		    (match validity with
		      | Base.Unknown (_, None, _) ->
                          CilE.warn_mem_write with_alarms
                      | Base.Unknown (_, Some k, _) when Int.gt stop_to k ->
                          CilE.warn_mem_write with_alarms
		    | _ -> ());
                    (if dst_is_exact
                      then LOffset.copy_paste ~f:None
                      else LOffset.copy_merge)
                      map_to_copy
                      start
                      stop
                      start_to
                      acc)
              i_dst
              offsetmap_dst
          with Not_less_than ->
            raise Bitwise_cannot_copy
        in
        LBase.add k_dst new_offsetmap acc_lmap
    in
    try
      let result = Location_Bits.fold_i treat_dst dst_loc m in
      if !had_non_bottom then result
      else begin
          Kernel.warning ~once:true ~current:true
            "all target addresses were invalid. This path is assumed to be dead.";
          assert false
        end
    with Location_Bits.Error_Top -> (* from Location_Bits.fold_i *)
      raise Bitwise_cannot_copy


  let copy_paste_map ~f src_loc dst_loc mm =
    assert (Int_Base.equal src_loc.size dst_loc.size );

(* temporary fix *)
      if not (Locations.is_valid ~for_writing:false src_loc
                  && Locations.is_valid ~for_writing:true dst_loc)
          then raise Bitwise_cannot_copy;

    try
      let size = Int_Base.project src_loc.size in
      let result =
        copy_offsetmap ~f src_loc mm
      in
      paste_offsetmap result dst_loc.loc Int.zero size mm
    with
    | Int_Base.Error_Top  (* from Int_Base.project *) ->
        raise Bitwise_cannot_copy

  let copy_paste ~with_alarms ~f src_loc dst_loc mm =
    let res =
      match mm with
      | Top -> Top
      | Bottom -> Bottom
      | Map mm -> Map (copy_paste_map ~with_alarms ~f src_loc dst_loc mm)
    in
(*    Format.printf "Lmap.copy_paste orig: %a from src:%a to dst:%a result:%a@\n"
      pretty mm
      Locations.pretty src_loc
      Locations.pretty dst_loc
      pretty res;*)
    res

end

module From_Model = struct 
  include Make_bitwise(Locations.Zone)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
