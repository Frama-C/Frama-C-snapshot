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

open Cil_types
open Cil
open Abstract_interp
open Abstract_value

module Initial_Values = struct
  let v = [ [Base.null,Ival.singleton_zero];
            [Base.null,Ival.singleton_one];
            [Base.null,Ival.zero_or_one];
            [Base.null,Ival.top];
            [Base.null,Ival.top_float];
            [Base.null,Ival.top_single_precision_float];
            [] ]
end

module BaseSetLattice = Make_Hashconsed_Lattice_Set(Base)(Base.Hptset)

module MapLattice =
  Map_Lattice.Make
    (Base)(BaseSetLattice)(Ival)(Initial_Values)(struct let zone = false end)

module HT = Hashtbl

module Location_Bytes = struct

  include MapLattice

  type z = tt =
    | Top of Top_Param.t * Origin.t
    | Map of M.t

  let inject_ival i = inject Base.null i

  let top_float = inject_ival Ival.top_float

  let top_single_precision_float = inject_ival Ival.top_single_precision_float

  let singleton_zero = inject_ival Ival.singleton_zero
  let singleton_one = inject_ival Ival.singleton_one
  let zero_or_one = inject_ival Ival.zero_or_one

  (* true iff [v] is exactly 0 *)
  let is_zero v = equal v singleton_zero

  (* [location_shift offset l] is the location [l] shifted by [offset] *)
  let location_shift offset l =
    try
      map_offsets (Ival.add offset) l
    with Error_Top -> l

  let top_leaf_origin () =
    Top(Top_Param.top,
        (Origin.Leaf (LocationSetLattice.currentloc_singleton())))

 let topify_with_origin o v =
   match v with
   | Top (s,a) ->
       Top (s, Origin.join a o)
   | v when is_zero v -> v
   | Map m ->
       if is_bottom v then v
       else inject_top_origin o (get_bases m)

 let get_bases m =
   match m with
   | Top(top_param,_) -> top_param
   | Map m -> Top_Param.inject (get_bases m)

 let is_relationable m =
   try
     let b,_ = find_lonely_binding m in
     match Base.validity b with
       Base.Periodic _ -> false
     | Base.Known _ | Base.Unknown _ | Base.All -> true
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
		       match Base.get_string strid with
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
                     Ival.fold
                       (fun i () -> f base strz (Int.to_int i) len)
                       roffs
                       ()
                 | _ -> ())
           m

 let under_topify v =
     match v with
   | Top _ -> v
   | Map _ ->
       if is_included singleton_zero v
       then singleton_zero
       else bottom

 let topify_merge_origin v =
   topify_with_origin
     (Origin.Merge (LocationSetLattice.currentloc_singleton())) v

 let topify_misaligned_read_origin v =
   topify_with_origin
     (Origin.Misalign_read (LocationSetLattice.currentloc_singleton())) v

 let topify_arith_origin v =
   topify_with_origin
     (Origin.Arith (LocationSetLattice.currentloc_singleton())) v

 let may_reach base loc =
   if Base.is_null base then true else
     match loc with
     | Top (toparam,_) ->
         Top_Param.is_included (Top_Param.inject_singleton base) toparam
     | Map m -> try
         ignore (M.find base m);
         true
       with Not_found -> false

 exception Contains_local

 let contains_addresses_of_locals is_local =
   let f base _offsets = is_local base
   in
   let projection _base = Ival.top in
   let cached_f =
     cached_fold
       ~cache:("loc_top_locals", 653)
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
       | Top (Top_Param.Top,_) -> true
       | Top (Top_Param.Set _top_param,_orig) ->
           false
       | Map _ -> false);
       true

 (**  TODO: merge with above function *)
   let remove_escaping_locals is_local v =
    match v with
    | Top (Top_Param.Top as t,_) -> t, v
    | Top (Top_Param.Set topparam,orig) ->
        let locals, nonlocals =
          Top_Param.O.partition
            is_local
            topparam
        in
        (Top_Param.inject locals), inject_top_origin orig nonlocals
    | Map m ->
        let locals, clean_map =
          M.fold
            (fun base _ (locals, m as acc) ->
              if is_local base
              then
                (Top_Param.O.add base locals), M.remove base m
              else acc)
            m
            (Top_Param.O.empty, m)
        in
        (Top_Param.inject locals), Map clean_map

 let contains_addresses_of_any_locals =
   let f base _offsets = Base.is_any_formal_or_local base in
   let projection _base = Ival.top in
   let cached_f =
     cached_fold
       ~cache:("loc_top_any_locals", 777)
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
       | Top (Top_Param.Top,_) -> true
       | Top (Top_Param.Set _top_param,_orig) ->
           false
       | Map _ -> false);
       true

 exception Found_overlap

 let partially_overlaps_table = HT.create 7
 let () = 
   Project.register_todo_before_clear 
     (fun _ -> HT.clear partially_overlaps_table)

 let partially_overlaps size mm1 mm2 =
      match mm1, mm2 with
      | Top (_,_), Top (_,_) -> true
      | Top _, (Map _ as m) | (Map _ as m), Top _ -> not (equal m bottom)
      | Map m1, Map m2 ->
	  let size_int = Int.to_int size in
	  try
	    let map_partially_overlaps =
	      try
		HT.find partially_overlaps_table size_int
	      with Not_found ->
		let f = 
		  M.generic_symetric_existential_predicate 
		    Found_overlap
		    ~decide_one:(fun _ _ -> ())
		    ~decide_both:
		    (fun x y -> 
		      if Ival.partially_overlaps size x y
		      then raise Found_overlap)
		in
		HT.add partially_overlaps_table size_int f;
		f
	    in    
	    map_partially_overlaps m1 m2;
	    false
          with
            Found_overlap -> true

end

module Location_Bits = Location_Bytes

module Zone = struct

  module Initial_Values = struct let v = [ [] ] end

  include Map_Lattice.Make
  (Base)
  (BaseSetLattice)
  (Int_Intervals)
  (Initial_Values)
  (struct let zone = true end)

  let default base bi ei = inject base (Int_Intervals.inject [bi,ei])
  let defaultall base = inject base Int_Intervals.top

  let pretty fmt m =
    match m with
    | Top (Top_Param.Top,a) ->
        Format.fprintf fmt "ANYTHING(origin:%a)"
          Origin.pretty a
    | Top (s,a) ->
        Format.fprintf fmt "Unknown(%a, origin:%a)"
          Top_Param.pretty s
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
(*
  let pretty_caml fmt m =
    match m with
    | Top (Top_Param.Top,a) ->
        assert false (* TODO *)
    | Top (s,a) ->
        assert false (* TODO *)
    | Map _ when equal m bottom ->
        Format.fprintf "Locations.Zone.bottom"
    | Map off ->
        Format.fprintf "Locations.Zone.inject_list [";
        let print_binding k v =
          Format.fprintf fmt "@[%a,@ %a;@,]@ "
            Base.pretty_caml k
            Int_Intervals.pretty_caml v
        in
        (M.iter print_binding) off
*)
  let out_some_or_bottom zone =
    match zone with
      | None -> bottom
      | Some l -> l

  let id = "Zone"

  let tag = hash


  exception Found_inter

  let valid_intersects m1 m2 =
    let result =
      match m1,m2 with
      | Map _, Map _ ->
          intersects m1 m2
      | Top (toparam, _), m | m, Top (toparam, _) ->
          (equal m bottom) ||
            let f base () =
              if Top_Param.is_included (Top_Param.inject_singleton base) toparam
              then raise Found_inter
            in
            try
              fold_bases f m ();
              false
            with Found_inter | Error_Top -> true
    in
    result

 let get_bases m =
   match m with
   | Top(top_param,_) -> top_param
   | Map m -> Top_Param.inject (get_bases m)

 let mem_base b = function
   | Top (top_param, _) ->
       Top_Param.mem b top_param
   | Map m -> M.mem b m


end

exception Not_valid

type location =
    { loc : Location_Bits.t;
      size : Int_Base.t }

let can_be_accessed {loc=loc;size=size} =
  try
    let size = Int_Base.project size in
    match loc with
      | Location_Bits.Top _ -> false
      | Location_Bits.Map m ->
          Location_Bits.M.iter
            (fun varid offset ->
               match Base.validity varid with
               | Base.Known (min_valid,max_valid)
               | Base.Unknown (min_valid,max_valid)
               | Base.Periodic (min_valid, max_valid, _)
                 ->
                   let min = Ival.min_int offset in
                   begin match min with
                   | None -> raise Not_valid
                   | Some v -> if Int.lt v min_valid then raise Not_valid
                   end;
                   let max = Ival.max_int offset in
                   begin match max with
                   | None -> raise Not_valid
                   | Some v ->
                       if Int.gt (Int.pred (Int.add v size)) max_valid then
                         raise Not_valid
                   end
               | Base.All -> ())
            m;
          true
  with
    | Int_Base.Error_Top | Int_Base.Error_Bottom | Not_valid -> false

let is_valid_param is_valid_offset {loc=loc;size=size} =
  try
    let size = Int_Base.project size in
    let is_valid_offset = is_valid_offset size in
    match loc with
      | Location_Bits.Top _ -> false
      | Location_Bits.Map m ->
          Location_Bits.M.iter is_valid_offset m;
          true
  with
  | Int_Base.Error_Top | Int_Base.Error_Bottom
  | Base.Not_valid_offset -> false


let is_valid ~for_writing = is_valid_param (Base.is_valid_offset ~for_writing)

let is_valid_or_function =
  is_valid_param
    (fun size base offs ->
      if Base.is_function base
      then (if Ival.is_zero offs then () else raise Base.Not_valid_offset)
      else Base.is_valid_offset ~for_writing:false size base offs)

exception Found_two

let valid_cardinal_zero_or_one ~for_writing {loc=loc;size=size} =
  Location_Bits.equal Location_Bits.bottom loc ||
    let found_one =
      let already = ref false in
      function () ->
        if !already then raise Found_two;
        already := true
    in
    try
    let size = Int_Base.project size in
    match loc with
      | Location_Bits.Top _ -> false
      | Location_Bits.Map m ->
          Location_Bits.M.iter
            (fun base offset ->
              if not (Base.is_read_only base && for_writing)
              then
               ( let inter =
                 match Base.validity base with
                 | Base.Known (min_valid,max_valid)
                 | Base.Unknown (min_valid,max_valid)
                 | Base.Periodic (min_valid,max_valid, _)
                   ->
                     let itv =
                       Ival.inject_range
                         (Some min_valid)
                         (Some (Int.succ (Int.sub max_valid size)))
                     in
                     Ival.narrow itv offset
                 | Base.All -> offset
               in
               if Ival.cardinal_zero_or_one inter
               then begin
                 if not (Ival.is_bottom inter)
                 then found_one ()
               end
               else raise Found_two))
            m;
          true
  with
    | Int_Base.Error_Top -> false
    | Found_two -> false
    | Int_Base.Error_Bottom ->
        Format.printf "Bottom size for:%a@\n" Location_Bits.pretty loc;
        assert false



let loc_bytes_to_loc_bits x =
  match x with
    | Location_Bytes.Map _ ->
        begin try
            Location_Bytes.map_offsets
              (Ival.scale (Bit_utils.sizeofchar()))
              x
        with Location_Bytes.Error_Top -> assert false
        end
    | Location_Bytes.Top _ -> x

let loc_bits_to_loc_bytes x =
    match x with
      | Location_Bits.Map _ ->
          begin try
            Location_Bits.map_offsets
              (Ival.scale_div ~pos:true (Bit_utils.sizeofchar())) x
          with Location_Bits.Error_Top -> assert false
          end
      | Location_Bits.Top _ -> x

let loc_to_loc_without_size {loc = loc} =
  loc_bits_to_loc_bytes loc
let loc_size { size = size } = size

let make_loc loc_bits size =
  if (match size with
            | Int_Base.Value v -> Int.gt v Int.zero
            | _ -> true)
  then
    { loc = loc_bits; size = size }
  else
    { loc = loc_bits; size = Int_Base.top }

let loc_bits_to_loc lv (loc_bits:Location_Bits.t) =
  let size = Bit_utils.sizeof_lval lv in
  make_loc loc_bits size

let size_of_varinfo v =
  try
    let s = bitsSizeOf v.vtype in
    let s = Int.of_int s in
    s
  with Cil.SizeOfError _ as e ->
    Kernel.debug ~once:true "Variable %a has no size" !Ast_printer.d_var v;
    raise e

let int_base_size_of_varinfo v = Int_Base.inject (size_of_varinfo v)

let loc_of_varinfo v =
  let base = Base.find v in
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

let loc_without_size_to_loc
    lv
    loc_without_size =
  loc_bits_to_loc
    lv
    (loc_bytes_to_loc_bits loc_without_size)

let loc_bottom = make_loc Location_Bits.bottom Int_Base.bottom

let cardinal_zero_or_one { loc = loc ; size = size } =
  Location_Bits.cardinal_zero_or_one loc &&
    Int_Base.cardinal_zero_or_one size

let loc_equal { loc = loc1 ; size = size1 } { loc = loc2 ; size = size2 } =
  Int_Base.equal size1 size2 &&
  Location_Bits.equal loc1 loc2

let pretty fmt { loc = loc ; size = size } =
  Format.fprintf fmt "%a (size:%a)"
    Location_Bits.pretty loc
    Int_Base.pretty size
let pretty_loc = pretty

let valid_enumerate_bits ~for_writing ({loc = loc_bits; size = size} as _arg)=
  (*  Format.printf "valid_enumerate_bits:%a@\n" pretty _arg; *)
  let result = match loc_bits with
  | Location_Bits.Top (Location_Bits.Top_Param.Top, _) -> Zone.top
  | Location_Bits.Top (Location_Bits.Top_Param.Set s, _) ->
      let compute_offset base acc =
        if for_writing && Base.is_read_only base
        then acc
        else
          let valid_offset =
            match Base.validity base, size with
            | (Base.Known (min_valid,max_valid)
              | Base.Unknown (min_valid,max_valid)
              | Base.Periodic (min_valid, max_valid, _)),
              Int_Base.Value size ->
                (*              Format.printf "min_valid:%a@\nmax_valid:%a@."
                                Int.pretty min_valid
                                Int.pretty max_valid; *)
                let max_valid = Int.succ (Int.sub max_valid size) in
                Ival.inject_range (Some min_valid) (Some max_valid)
            | _,Int_Base.Bottom -> assert false
            | Base.All,_ | _,Int_Base.Top -> Ival.top
          in
          if Ival.is_bottom valid_offset
          then acc
          else
            let valid_offset =
              Int_Intervals.from_ival_size valid_offset size
            in
            Zone.M.add base valid_offset acc
      in
      Zone.inject_map
        (Location_Bits.Top_Param.O.fold compute_offset s Zone.M.empty)
  | Location_Bits.Map m ->
      let compute_offset base offs acc =
        if for_writing && Base.is_read_only base
        then acc
        else
          let valid_offset =
            match Base.validity base, size with
            | (Base.Known (min_valid,max_valid)
              |Base.Unknown (min_valid,max_valid)
              |Base.Periodic (min_valid, max_valid, _)),
              Int_Base.Value size ->
                let max_valid = Int.succ (Int.sub max_valid size) in
                (*              Format.printf "min_valid:%a@\nmax_valid:%a@."
                                Int.pretty min_valid
                                Int.pretty max_valid; *)
                Ival.meet
                  (Ival.inject_range (Some min_valid) (Some max_valid))
                  offs
            | (Base.All|Base.Unknown _|Base.Known _|Base.Periodic _),_  -> offs
          in
          if Ival.is_bottom valid_offset
          then acc
          else
            let valid_offset = Int_Intervals.from_ival_size valid_offset size
            in
            Zone.M.add base valid_offset acc
      in
      Zone.inject_map
        (Location_Bits.M.fold compute_offset m Zone.M.empty)
  in
  (*      Format.printf "valid_enumerate_bits leads to %a@\n" Zone.pretty result; *)
  result

let zone_of_varinfo var =
  valid_enumerate_bits ~for_writing:false (loc_of_varinfo var)

(** [valid_part l] is an over-approximation of the valid part
   of the location [l] *)
let valid_part ~for_writing ({loc = loc; size = size } as l) =
(*  Format.printf "valid_part: loc=%a@." pretty l;*)
  match loc with
    | Location_Bits.Top _ -> l
    | Location_Bits.Map m ->
        let compute_offset base offs acc =
          let valid_offset =
            if for_writing && (Base.is_read_only base)
            then Ival.bottom
            else
              (  match Base.validity base, size with
              | (Base.Known (min_valid,max_valid)
                |Base.Unknown (min_valid,max_valid)
                |Base.Periodic (min_valid, max_valid, _)),
                Int_Base.Value size ->
                  let max_valid = Int.succ (Int.sub max_valid size) in
                  (*            Format.printf "min_valid:%a@\nmax_valid:%a@."
                                Int.pretty min_valid
                                Int.pretty max_valid; *)
                  let valid_ival =
                    Ival.inject_range (Some min_valid) (Some max_valid)
                  in
                  let result = Ival.narrow offs valid_ival in
                  (* Format.printf "base:%a offs:%a valid:%a result:%a@."
                                Base.pretty base
                                Ival.pretty offs
                                Ival.pretty valid_ival
                                Ival.pretty result; *)
                  result

              | (Base.All|Base.Unknown _|Base.Known _|Base.Periodic _),_  ->
                  offs)
          in
          if Ival.is_bottom valid_offset
          then acc
          else
            Location_Bits.M.add base valid_offset acc
        in
        let loc =
          Location_Bits.inject_map
            (Location_Bits.M.fold compute_offset m Location_Bits.M.empty)
        in
        make_loc loc size

(** [invalid_part l] is an over-approximation of the invalid part
   of the location [l] *)
let invalid_part l = l
(*  this implementation sucks, the complexity is too high.
   The function should fold on the bases instead of folding on the atomic
   locations
let invalid_part ({loc = loc; size = size } as l) =
    try
      let result =
        Location_Bits.fold_enum
          (fun loc_bits acc ->
            if not (can_be_accessed (make_loc loc_bits size))
            then Location_Bits.join loc_bits acc
            else acc)
          loc
          Location_Bits.bottom
      in
      make_loc result size
    with Location_Bits.Error_Top -> l *)

let filter_loc ({loc = loc; size = size } as initial) zone =
  try
    let result = Location_Bits.fold_i
      (fun base ival acc ->
         let result_ival =
           match zone,size with
           | Zone.Top _, _ | _,(Int_Base.Top|Int_Base.Bottom) -> ival
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
        let pretty = pretty_loc
      end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
