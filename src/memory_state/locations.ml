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

open Cil_types
open Cil
open Abstract_interp
open Lattice_Interval_Set

module Initial_Values = struct
  let v = [ [Base.null,Ival.singleton_zero];
            [Base.null,Ival.singleton_one];
            [Base.null,Ival.zero_or_one];
            [Base.null,Ival.top];
            [Base.null,Ival.top_float];
            [Base.null,Ival.top_single_precision_float];
            [] ]
end

(* Store the information that the location has at most cardinal 1 *)
module Comp_cardinal_0_1 = struct
  let e = true
  let compose _ _ = false (* Keys cannot be bound to Bottom (see MapLattice).
                             Hence, two subtrees have cardinal one. *)
  let f _k v = Ival.cardinal_zero_or_one v
  let default = true
end

module MapLatticeIval =
  Map_Lattice.Make
    (Base.Base)(Base.SetLattice)(Ival)(Comp_cardinal_0_1)(Initial_Values)(
      struct
        let zone = false
        let null = Base.null
        let is_null = Base.is_null
      end)

module Location_Bytes = struct

  include MapLatticeIval

  type z = tt =
    | Top of Base.SetLattice.t * Origin.t
    | Map of M.t

  let inject_ival i = inject Base.null i

  let inject_float f = 
    inject_ival 
      (Ival.inject_float
	  (Ival.Float_abstract.inject_singleton f))

  let top_float = inject_ival Ival.top_float

  let top_single_precision_float = inject_ival Ival.top_single_precision_float

  let singleton_zero = inject_ival Ival.singleton_zero
  let singleton_one = inject_ival Ival.singleton_one
  let zero_or_one = inject_ival Ival.zero_or_one

  (* true iff [v] is exactly 0 *)
  let is_zero v = equal v singleton_zero

  (* [shift offset l] is the location [l] shifted by [offset] *)
  let shift offset l =
    try
      map_offsets (Ival.add_int offset) l
    with Error_Top -> l

  (* Override the function coming from MapLattice, we can do better *)
  let cardinal_zero_or_one = function
    | Top _ -> false
    | Map m -> M.compositional_bool m

  let cardinal = function
    | Top _ -> None
    | Map m ->
      M.fold (fun _ v card ->
        match card, Ival.cardinal v with
          | None, _ | _, None -> None
          | Some c1, Some c2 -> Some (Int.add c1 c2)
      ) m (Some Int.zero)

  let top_with_origin origin =
    Top(Base.SetLattice.top, origin)

 let topify_with_origin o v =
   match v with
   | Top (s,a) ->
       Top (s, Origin.join a o)
   | v when is_zero v -> v
   | Map m ->
       if is_bottom v then v
       else inject_top_origin o (get_bases m)

 let topify_with_origin_kind ok v =
   let o = Origin.current ok in
   topify_with_origin o v

 let get_bases m =
   match m with
   | Top(top_param,_) -> top_param
   | Map m -> Base.SetLattice.inject (get_bases m)

 let is_relationable m =
   try
     let b,_ = find_lonely_binding m in
     match Base.validity b with
     | Base.Periodic _ -> false
     | Base.Known _ | Base.Unknown _ | Base.Invalid -> true
   with Not_found -> false

 let iter_on_strings =
   let z = "\000" in
   fun ~skip f l ->
     match l with
     | Top _ ->
         assert false
     | Map m ->
         M.iter
           (fun base offs ->
             match skip with
               Some base_to_skip when Base.equal base base_to_skip -> ()
             | _ ->
                 match base with
                   Base.String (_, strid) ->
                     let str = 
		       match strid with
		       | Base.CSString s -> s
		       | Base.CSWstring _ -> 
			   failwith "Unimplemented: wide strings"
		     in
                     let strz = str ^ z in
                     let len = String.length str in
                     let range =
                       Ival.inject_range
                         (Some Int.zero)
                         (Some (Int.of_int len))
                     in
                     let roffs = Ival.narrow range offs in
                     Ival.fold_int
                       (fun i () -> f base strz (Int.to_int i) len)
                       roffs
                       ()
                 | _ -> ())
           m

 let topify_merge_origin v =
   topify_with_origin_kind Origin.K_Merge v

 let topify_misaligned_read_origin v =
   topify_with_origin_kind Origin.K_Misalign_read v

 let topify_arith_origin v =
   topify_with_origin_kind Origin.K_Arith v

 let topify_leaf_origin v =
   topify_with_origin_kind Origin.K_Leaf v

 let may_reach base loc =
   if Base.is_null base then true
   else
     match loc with
     | Top (toparam,_) ->
         Base.SetLattice.is_included (Base.SetLattice.inject_singleton base) toparam
     | Map m -> try
         ignore (M.find base m);
         true
       with Not_found -> false

 let contains_addresses_of_locals is_local =
   let f base _offsets = is_local base in
   let projection _base = Ival.top in
   let cached_f =
     cached_fold
       ~cache_name:"loc_top_locals"
       ~temporary:true
       ~f
       ~projection
       ~joiner:(||)
       ~empty:false
   in
   fun loc ->
     try
       cached_f loc
     with Error_Top ->
       assert (match loc with
       | Top (Base.SetLattice.Top,_) -> true
       | Top (Base.SetLattice.Set _top_param,_orig) ->
           false
       | Map _ -> false);
       true

 (**  TODO: merge with above function *)
   let remove_escaping_locals is_local v =
    match v with
    | Top (Base.SetLattice.Top as t,_) -> t, v
    | Top (Base.SetLattice.Set garble, orig) ->
        let locals, nonlocals = Base.Hptset.partition is_local garble in
        (Base.SetLattice.inject locals), inject_top_origin orig nonlocals
    | Map m ->
        let locals, clean_map =
          M.fold
            (fun base _ (locals, m as acc) ->
              if is_local base
              then (Base.Hptset.add base locals), (M.remove base m)
              else acc)
            m
            (Base.Hptset.empty, m)
        in
        (Base.SetLattice.inject locals), Map clean_map

 let contains_addresses_of_any_locals =
   let f base _offsets = Base.is_any_formal_or_local base in
   let projection _base = Ival.top in
   let cached_f =
     cached_fold
       ~cache_name:"loc_top_any_locals"
       ~temporary:false
       ~f
       ~projection
       ~joiner:(||)
       ~empty:false
   in
   fun loc ->
     try
       cached_f loc
     with Error_Top ->
       assert (match loc with
       | Top (Base.SetLattice.Top,_) -> true
       | Top (Base.SetLattice.Set _top_param,_orig) ->
           false
       | Map _ -> false);
       true

 type overlaps = Overlaps of (M.t -> M.t -> bool)

 module DatatypeOverlap = Datatype.Make(struct
   include Datatype.Undefined (* Closures: cannot be marshalled *)
   type t = overlaps
   let name = "Locations.DatatypeOverlap.t"
   let reprs = [Overlaps (fun _ _ -> true)]
   let mem_project = Datatype.never_any_project
 end)

 module PartiallyOverlaps =
   State_builder.Int_hashtbl(DatatypeOverlap)(struct
     let size = 7
     let dependencies = [Ast.self]
     let name = "Locations.PartiallyOverlap"
   end)

 let partially_overlaps ~size mm1 mm2 =
   match mm1, mm2 with
     | Top _, _ | _, Top _ -> intersects mm1 mm2
     | Map m1, Map m2 ->
       let size_int = Int.to_int size in
       let map_partially_overlaps =
	 try
           (match PartiallyOverlaps.find size_int with Overlaps f -> f)
	 with Not_found ->
           let name = Pretty_utils.sfprintf "Locations.Overlap(%d)" size_int in
	   let f = 
	     M.symmetric_binary_predicate
               (M.TemporaryCache name) M.ExistentialPredicate
	       ~decide_fast:(fun _ _ -> M.PUnknown)
	       ~decide_one:(fun _ _ -> false)
	       ~decide_both:(fun _ x y -> Ival.partially_overlaps size x y)
	   in
           PartiallyOverlaps.add size_int (Overlaps f);
	   f
       in
       map_partially_overlaps m1 m2
end

module Location_Bits = Location_Bytes

module Zone = struct

  module Initial_Values = struct let v = [ [] ] end

  include Map_Lattice.Make_without_cardinal
  (Base.Base)
  (Base.SetLattice)
  (Int_Intervals)
  (Hptmap.Comp_unused)
  (Initial_Values)
  (struct
    let zone = true
    let null = Base.null
    let is_null = Base.is_null
   end)

  let default base bi ei = inject base (Int_Intervals.inject [bi,ei])
  let defaultall base = inject base Int_Intervals.top

  let pretty fmt m =
    match m with
    | Top (Base.SetLattice.Top,a) ->
        Format.fprintf fmt "ANYTHING(origin:%a)"
          Origin.pretty a
    | Top (s,a) ->
        Format.fprintf fmt "Unknown(%a, origin:%a)"
          Base.SetLattice.pretty s
          Origin.pretty a
    | Map _ when equal m bottom ->
        Format.fprintf fmt "\\nothing"
    | Map off ->
        let print_binding fmt (k, v) =
          Format.fprintf fmt "@[<h>%a%a@]"
            Base.pretty k
            (Int_Intervals.pretty_typ (Base.typeof k)) v
        in
        Pretty_utils.pp_iter ~pre:"" ~suf:"" ~sep:";@,@ "
          (fun f -> M.iter (fun k v -> f (k, v))) print_binding fmt off

  let valid_intersects = intersects

 let mem_base b = function
   | Top (top_param, _) ->
       Base.SetLattice.mem b top_param
   | Map m -> M.mem b m

 let shape = M.shape

end


type location =
    { loc : Location_Bits.t;
      size : Int_Base.t }

let is_valid_aux is_valid_offset {loc=loc;size=size} =
  try
    let size = Int_Base.project size in
    let is_valid_offset = is_valid_offset size in
    match loc with
      | Location_Bits.Top _ -> false
      | Location_Bits.Map m ->
          Location_Bits.M.iter is_valid_offset m;
          true
  with
  | Int_Base.Error_Top | Base.Not_valid_offset -> false


let is_valid ~for_writing = is_valid_aux (Base.is_valid_offset ~for_writing)

let is_valid_or_function =
  is_valid_aux
    (fun size base offs ->
      if Base.is_function base
      then (if Ival.is_zero offs then () else raise Base.Not_valid_offset)
      else Base.is_valid_offset ~for_writing:false size base offs)

exception Found_two

(* Reduce [offsets] so that reading [size] from [offsets] fits within
   the validity of [base] *)
let reduce_offset_by_validity ~for_writing base offsets size =
  if for_writing && Base.is_read_only base then
    Ival.bottom
  else
    match Base.validity base, size with
      | Base.Invalid, _ -> Ival.bottom
      | _, Int_Base.Top -> offsets
      | ( Base.Known (minv,maxv)
        | Base.Unknown (minv,_,maxv)
        | Base.Periodic (minv, maxv, _)),
        Int_Base.Value size ->
          let maxv = Int.succ (Int.sub maxv size) in
          let range = Ival.inject_range (Some minv) (Some maxv) in
          Ival.narrow range offsets


let valid_cardinal_zero_or_one ~for_writing {loc=loc;size=size} =
  Location_Bits.equal Location_Bits.bottom loc ||
    let found_one =
      let already = ref false in
      function () ->
        if !already then raise Found_two;
        already := true
    in
    try
    match loc with
      | Location_Bits.Top _ -> false
      | Location_Bits.Map m ->
          Location_Bits.M.iter
            (fun base offsets ->
               let valid_offsets =
                 reduce_offset_by_validity ~for_writing base offsets size
               in
               if Ival.cardinal_zero_or_one valid_offsets
               then begin
                 if not (Ival.is_bottom valid_offsets)
                 then found_one ()
               end
               else raise Found_two
            ) m;
          true
    with
      | Int_Base.Error_Top | Found_two -> false


let loc_bytes_to_loc_bits x =
  match x with
    | Location_Bytes.Map _ ->
        Location_Bytes.map_offsets
          (Ival.scale (Bit_utils.sizeofchar()))
          x
    | Location_Bytes.Top _ -> x

let loc_bits_to_loc_bytes x =
    match x with
      | Location_Bits.Map _ ->
          Location_Bits.map_offsets
            (Ival.scale_div ~pos:true (Bit_utils.sizeofchar())) x
      | Location_Bits.Top _ -> x

let loc_to_loc_without_size {loc = loc} = loc_bits_to_loc_bytes loc
let loc_size { size = size } = size

let make_loc loc_bits size =
  if (match size with
            | Int_Base.Value v -> Int.gt v Int.zero
            | _ -> true)
  then
    { loc = loc_bits; size = size }
  else
    { loc = loc_bits; size = Int_Base.top }

let filter_base f loc =
  { loc with loc = Location_Bits.filter_base f loc.loc }

let int_base_size_of_varinfo v =
  try
    let s = bitsSizeOf v.vtype in
    let s = Int.of_int s in
    Int_Base.inject s
  with Cil.SizeOfError _ ->
    Kernel.debug ~once:true "Variable %a has no size" Printer.pp_varinfo v;
    Int_Base.top

let loc_of_varinfo v =
  let base = Base.of_varinfo v in
  make_loc (Location_Bits.inject base Ival.zero) (int_base_size_of_varinfo v)

let loc_of_base v =
  make_loc (Location_Bits.inject v Ival.zero) (Base.bits_sizeof v)

let loc_of_typoffset v typ offset =
  try
    let offs, size = bitsOffset typ offset in
    let size = if size = 0 then
      Int_Base.top
    else Int_Base.inject (Int.of_int size)
    in
    make_loc
      (Location_Bits.inject v (Ival.of_int offs))
      size
  with SizeOfError _ ->
    make_loc
      (Location_Bits.inject v Ival.top)
      Int_Base.top

let loc_bottom = make_loc Location_Bits.bottom Int_Base.top
let is_bottom_loc l = Location_Bits.is_bottom l.loc

let cardinal_zero_or_one { loc = loc ; size = size } =
  Location_Bits.cardinal_zero_or_one loc &&
    Int_Base.cardinal_zero_or_one size

let loc_equal { loc = loc1 ; size = size1 } { loc = loc2 ; size = size2 } =
  Int_Base.equal size1 size2 &&
  Location_Bits.equal loc1 loc2

let loc_hash { loc = loc; size = size } =
  Int_Base.hash size + 317 * Location_Bits.hash loc

let loc_compare { loc = loc1 ; size = size1 } { loc = loc2 ; size = size2 } =
  let c1 = Int_Base.compare size1 size2 in
  if c1 <> 0 then c1
  else Location_Bits.compare loc1 loc2

let pretty fmt { loc = loc ; size = size } =
  Format.fprintf fmt "%a (size:%a)"
    Location_Bits.pretty loc
    Int_Base.pretty size
let pretty_loc = pretty

let pretty_english ~prefix fmt { loc = m ; size = size } =
  match m with
  | Location_Bits.Top (Base.SetLattice.Top,a) ->
      Format.fprintf fmt "somewhere unknown (origin:%a)"
        Origin.pretty a
  | Location_Bits.Top (s,a) ->
      Format.fprintf fmt "somewhere in %a (origin:%a)"
        Base.SetLattice.pretty s
        Origin.pretty a
  | Location_Bits.Map _ when Location_Bits.is_bottom m ->
      Format.fprintf fmt "nowhere"
  | Location_Bits.Map off ->
      let print_binding fmt (k, v) =
	( match Ival.is_zero v, Base.validity k, size with
	  true, Base.Known (_,s1), Int_Base.Value s2 when 
	      Int.equal (Int.succ s1) s2 ->
	    Format.fprintf fmt "@[<h>%a@]" Base.pretty k
        | _ ->
            Format.fprintf fmt "@[<h>%a with offsets %a@]"
	      Base.pretty k
	      Ival.pretty v)
      in
      Pretty_utils.pp_iter
        ~pre:(if prefix then format_of_string "in " else "") ~suf:"" ~sep:";@,@ "
	(fun f -> Location_Bits.M.iter (fun k v -> f (k, v)))
        print_binding fmt off

let enumerate_valid_bits ~for_writing {loc = loc_bits; size = size}=
  let compute_offset base offs acc =
    let valid_offset = reduce_offset_by_validity ~for_writing base offs size in
    if Ival.is_bottom valid_offset then
      acc
    else
      let valid_itvs = Int_Intervals.from_ival_size valid_offset size in
      Zone.M.add base valid_itvs acc
  in
  match loc_bits with
  | Location_Bits.Top (Base.SetLattice.Top, _) -> Zone.top
  | _ ->
    Zone.Map (Location_Bits.fold_topset_ok compute_offset loc_bits Zone.M.empty)

(** [valid_part l] is an over-approximation of the valid part
   of the location [l] *)
let valid_part ~for_writing {loc = loc; size = size } =
  let compute_loc base offs acc =
    let valid_offset = reduce_offset_by_validity ~for_writing base offs size in
    if Ival.is_bottom valid_offset then
      acc
    else
      Location_Bits.M.add base valid_offset acc
  in
  let locbits = 
    match loc with
      | Location_Bits.Top (Base.SetLattice.Top, _) -> loc
      | Location_Bits.Top (Base.SetLattice.Set _, _) ->
          (* We do not reduce garbled mixes. This makes them disappear after
             one memory access. *)
          if false then
            Location_Bits.(Map (fold_topset_ok compute_loc loc M.empty))
          else
            loc
      | Location_Bits.Map m ->
          Location_Bits.inject_map
            (Location_Bits.M.fold compute_loc m Location_Bits.M.empty)
  in
  make_loc locbits size

let enumerate_bits ({loc = loc_bits; size = size} as _arg)=
  let compute_offset base offs acc =
    let valid_offset = Int_Intervals.from_ival_size offs size in
    Zone.M.add base valid_offset acc
  in
  match loc_bits with
  | Location_Bits.Top (Base.SetLattice.Top, _) -> Zone.top
  | _ ->
    Zone.Map (Location_Bits.fold_topset_ok compute_offset loc_bits Zone.M.empty)

let zone_of_varinfo var = enumerate_bits (loc_of_varinfo var)

(** [invalid_part l] is an over-approximation of the invalid part
   of the location [l] *)
let invalid_part l = l (* TODO (but rarely useful) *)


let filter_loc ({loc = loc; size = size } as initial) zone =
  try
    let result = Location_Bits.fold_i
      (fun base ival acc ->
         let result_ival =
           match zone,size with
           | Zone.Top _, _ | _, Int_Base.Top -> ival
           | Zone.Map zone_m,Int_Base.Value size ->
               Int_Intervals.fold
                 (fun (bi,ei) acc ->
                    let width = Int.length bi ei in
                    if Int.lt width size
                    then acc
                    else
                      Ival.inject_range (Some bi) (Some (Int.length size ei)))
                 (Zone.find_or_bottom base zone_m)
                 Ival.bottom
         in
         Location_Bits.join acc (Location_Bits.inject base result_ival))
      loc
      Location_Bits.bottom
    in
    make_loc result size
  with Location_Bits.Error_Top -> initial

module Location =
  Datatype.Make
      (struct
        include Datatype.Serializable_undefined
        type t = location
        let structural_descr =
          Structural_descr.t_record
            [| Location_Bits.packed_descr; Int_Base.packed_descr |]
        let reprs =
          List.fold_left
            (fun acc l ->
              List.fold_left
                (fun acc n -> { loc = l; size = n } :: acc)
                acc
                Int_Base.reprs)
            []
            Location_Bits.reprs
        let name = "Locations.Location"
        let mem_project = Datatype.never_any_project
        let equal = loc_equal
        let compare = loc_compare
        let hash = loc_hash
        let pretty = pretty_loc
      end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
