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

open Abstract_interp
open Abstract_value
open Locations
open CilE
exception Cannot_copy

module type Location_map = sig

  type y (* type of the values associated to the locations *)
  type widen_hint_offsetmap
  type loffset

  module Make
    (Default_offsetmap : sig val default_offsetmap : Base.t -> loffset end) :
  sig
    module LBase : 
    sig 
      type t
      val iter : (Base.base -> loffset -> unit) -> t -> unit
    end
    type tt = private Bottom | Top | Map of LBase.t
    include Datatype.S with type t = tt 

    type widen_hint = bool * Base.Set.t * (Base.t -> widen_hint_offsetmap)
    type instanciation = Location_Bytes.t Base.Map.t

    val inject : Base.t -> loffset -> t

    val add_offsetmap :  Base.t -> loffset -> t -> t

    val pretty_without_null : Format.formatter -> t -> unit
    val pretty_filter :
      Format.formatter -> t -> Zone.t -> (Base.t -> bool) -> unit

    val add_binding :
      with_alarms:CilE.warn_mode -> exact:bool -> t -> location -> y -> t

    val find :
      conflate_bottom:bool -> with_alarms:CilE.warn_mode -> t -> location -> y

    val join : t -> t -> location list *  t
    val is_included : t -> t -> bool
    val is_included_actual_generic :
      Zone.t -> t -> t -> instanciation

    val top: t

    (** Empty map. Casual users do not need this.*)
    val empty_map : t
    val is_empty_map : t -> bool

    (** Every location is associated to [VALUE.bottom] in [bottom].
	This state can be reached only in dead code. *)
    val bottom : t
    val is_reachable : t -> bool

    val widen : widen_hint-> t -> t -> (bool * t)

    val filter_base : (Base.t -> bool) -> t -> t

    (** @raise [Not_found] if the varid is not present in the map *)
    val find_base : Base.t -> t -> loffset

    val remove_base : Base.t -> t -> t

    val reduce_binding : with_alarms:CilE.warn_mode ->
      t -> location -> y -> t

    val copy_paste : location -> location -> t -> t
    val paste_offsetmap :
      loffset -> Location_Bits.t -> Int.t -> Int.t -> t -> t

    val copy_offsetmap :
      with_alarms:CilE.warn_mode -> location -> t -> loffset option
    val compute_actual_final_from_generic :
      t -> t -> Zone.t -> instanciation -> t*Location_Bits.Top_Param.t

    val is_included_by_location_enum :  t -> t -> Zone.t -> bool

    (* @raise [Invalid_argument "Lmap.fold"] if one location is not aligned
       or of size different of [size]. *)
    val fold: size:Int.t -> (location -> y -> 'a -> 'a) -> t -> 'a -> 'a

    (** @raise [Invalid_argument "Lmap.fold"] if one location is not aligned
	or of size different of [size]. *)
    val fold_single_bindings:
      size:Int.t -> (location -> y -> 'a -> 'a) -> t -> 'a -> 'a

    (** [fold_base f m] calls [f] on all bases bound to non top offsetmaps in
	the non bottom map [m].
	@raise [Error_Bottom] if [m] is bottom.*)
    val fold_base : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a

    (** [fold_base_offsetmap f m] calls [f] on all bases bound to non top
	offsetmaps in the non bottom map [m].
	@raise [Error_Bottom] if [m] is bottom.*)
    val fold_base_offsetmap : (Base.t -> loffset -> 'a -> 'a) -> t -> 'a -> 'a

    val find_offsetmap_for_location : Location_Bits.t -> t -> loffset
    val add_whole: location -> y -> t -> t
    val remove_whole: location -> t -> t

    val comp_prefixes: t -> t -> unit
    type subtree
    val find_prefix : t -> Hptmap.prefix -> subtree option
    val hash_subtree : subtree -> int
    val equal_subtree : subtree -> subtree -> bool

    (** [reciprocal_image m b] is the set of bits in the map [m] that may lead
	to Top([b]) and  the location in [m] where one may read an address
	[b]+_ *)
    val reciprocal_image : Base.t -> t -> Zone.t*Location_Bits.t

  (*  val create_initialized_var :
      Cil_types.varinfo -> Base.validity -> loffset -> Base.t
   *)
    val create_initial :
      base:Base.t ->
      v:y ->
      modu:Int.t ->
      state:t -> t

    exception Error_Bottom

    val cached_fold :
      f:(Base.t -> loffset -> 'a) ->
      cache:string * int ->    temporary:bool ->
      joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a

    val cached_map :
      f:(Base.t -> loffset -> loffset) ->
      cache:string * int -> temporary:bool ->
      t -> t

    exception Found_prefix of Hptmap.prefix * subtree * subtree

  end

end

module Make_LOffset
  (V: Lattice_With_Isotropy.S)
  (LOffset: Offsetmap.S with type y = V.t and type widen_hint = V.widen_hint)
  =
struct

  type y = V.t
  type loffset = LOffset.t
  type widen_hint_offsetmap = V.widen_hint
(*   module LOffset = Offsetmap.Make(V) *)

  module Make
    (Default_offsetmap: sig val default_offsetmap : Base.t -> LOffset.t end) =
  struct

    open Default_offsetmap

    module LBase =  struct

      module Comp =
	struct
	  let f base offsetmap =
	    match Base.validity base with
	      Base.Known (b, e) | Base.Unknown (b, e) when
		  Int.lt (Int.sub e b) Int.onethousand ->
		    LOffset.cardinal_zero_or_one (Base.validity base) offsetmap
	    | Base.Known _ | Base.Unknown _
	    | Base.Periodic _ | Base.All -> false

	  let compose a b = a && b
	  let e = true
	  let default = true
	end

      module Initial_Values = struct let v = [ [] ] end

      include Hptmap.Make
      (Base)
      (LOffset)
      (Comp)
      (Initial_Values)

      let add k v m =
	if LOffset.equal v (default_offsetmap k) then
          remove k m
	else add k v m

      let find_or_default varid map =
	try
	  find varid map
	with Not_found -> default_offsetmap varid
    end

    exception Found_prefix = LBase.Found_prefix

    type tt =
      |	Bottom
      | Top
      | Map of LBase.t

    let equal m1 m2 =
      match m1, m2 with
      |	Bottom, Bottom -> true
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

    type instanciation = Location_Bytes.t Base.Map.t

    let empty_map = Map LBase.empty

    let hash = function
      | Bottom -> 457
      | Top -> 458
      | Map m -> LBase.tag m

    let pretty fmt m =
      Format.fprintf fmt "@[";
      (match m with
	Bottom -> Format.fprintf fmt "NOT ACCESSIBLE"
       | Map m ->
           LBase.iter
             (fun base offs ->
		let typ = Base.typeof base in
		  Format.fprintf fmt "@[%a@[%a@]@\n@]" Base.pretty base
                    (LOffset.pretty_typ typ) offs)
             m
       | Top -> Format.fprintf fmt "NO INFORMATION");
      Format.fprintf fmt "@]"

    include Datatype.Make
	(struct
	  type t = tt
	  let structural_descr =
	    Structural_descr.Structure
	      (Structural_descr.Sum [| [| LBase.packed_descr |] |])
	  let name = LOffset.name ^ " lmap"
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

    exception Error_Bottom

    let add_offsetmap base offsetmap acc =
      match acc with
      | Map acc -> Map (LBase.add base offsetmap acc)
      | Bottom -> raise Error_Bottom
      | Top -> Top

    let inject base offsetmap =
      add_offsetmap base offsetmap empty_map

    let is_empty_map = function
	Bottom -> assert false
      |	Top -> assert false
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

  let find_base (vi:LBase.key) (m:t) =
    match m with
      | Bottom -> raise Not_found
      | Map m -> LBase.find vi m
      | Top -> LOffset.empty

  let remove_base (vi:LBase.key) (m:t) =
    match m with
    | Bottom -> m
    | Map m -> Map (LBase.remove vi m)
    | Top -> assert false

  let is_reachable t =
    match t with
      Bottom -> false
    | Top | Map _ -> true

  let pretty_without_null fmt m =
    Format.fprintf fmt "@[";
    (match m with
      Bottom -> Format.fprintf fmt "NOT ACCESSIBLE"
    | Top -> Format.fprintf fmt "NO INFORMATION"
    | Map m ->
         LBase.iter
           (fun base offs ->
              if not (Base.is_null base) then
                Format.fprintf fmt "@[%a@[%a@]@\n@]" Base.pretty base
                  (LOffset.pretty_typ (Base.typeof base)) offs)
           m);
    Format.fprintf fmt "@]"


  (* Display only locations in [filter]. Enforce the display of Top for
     bases not in [m] but in [filter] *)
  let pretty_filter fmt mm filter refilter =
    Format.fprintf fmt "@[";
    (match mm with
     | Bottom -> Format.fprintf fmt "NON TERMINATING FUNCTION"
     | Top -> Format.fprintf fmt "NO INFORMATION"
     | Map m ->
         let filter_it base _itvs () =
	   if refilter base
	   then
             let offs = LBase.find_or_default base m in
             Format.fprintf fmt "@[%a@[%a@]@\n@]"
               Base.pretty base
               (LOffset.pretty_typ (Base.typeof base)) offs
         in
	 try
	   Zone.fold_topset_ok filter_it filter ()
	 with Zone.Error_Top ->
             Format.fprintf fmt
               "Cannot filter: dumping raw memory (including unchanged variables)@\n%a@\n"
               pretty mm
         );
    Format.fprintf fmt "@]"


  exception Not_a_proper_location

  let add_whole loc v map =
    match map with
      Bottom -> assert false
    | Top -> assert false
    | Map map ->
	try
	  let varid, ival = Location_Bits.find_lonely_binding loc.loc in
	  let b = Ival.project_int ival in
	  let size = Int_Base.project loc.size in
	  let offsetmap_orig = LBase.find_or_default varid map in
	  Map
	    (LBase.add
	       varid
	       (LOffset.add_whole (b, Int.pred(Int.add b size)) v offsetmap_orig)
	       map)
	with
	  Ival.Not_Singleton_Int -> assert false
	| Not_found ->
	    Format.printf "add_whole:Not_found; loc=%a@."
	    Locations.pretty loc;
	    raise Not_a_proper_location
	| Int_Base.Error_Top ->  Format.printf "add_whole:Int_Base" ;
	    raise Not_a_proper_location

  let remove_whole loc map =
    match map with
      Bottom -> assert false
    | Top -> assert false
    | Map map ->
	try
	  let size = Int_Base.project loc.size in
	  let treat_base base ival acc =
	    let offsetmap_orig = LBase.find_or_default base map in
	    match ival with
	    | Ival.Set o ->
		let offsetmap =
		  Ival.O.fold
		    (fun offs acc ->
		       LOffset.remove_whole
			 (offs, Int.pred(Int.add offs size))
			 acc)
		    o
		    offsetmap_orig
		in
		LBase.add base offsetmap acc
	    | Ival.Top (Some min,Some max,_r,_modu) ->
		let offsetmap =
		  LOffset.remove_whole
		    (min, Int.pred(Int.add max size))
		    offsetmap_orig
		in
		LBase.add base offsetmap acc
	    | Ival.Top (_,_,_,_) ->
		LBase.remove base acc
	    | Ival.Float _ -> assert false
	  in
	  let new_map =
	    Location_Bits.fold_i treat_base loc.loc map
	  in
	  Map new_map
	with
	  Int_Base.Error_Top -> assert false

  let add_binding_offsetmap ~with_alarms ~exact varid offsets size v map =
    match size with
      | Int_Base.Top ->
          let offsetmap_orig = LBase.find_or_default varid map in
          let new_offsetmap =
            LOffset.overwrite offsetmap_orig v
              (Origin.Arith (LocationSetLattice.currentloc_singleton()))
          in
          LBase.add varid new_offsetmap map

      | Int_Base.Bottom -> assert false
      | Int_Base.Value size ->
          assert (Int.gt size Int.zero);
          let offsetmap_orig = LBase.find_or_default varid map in
          (*Format.printf "add_binding_offsetmap varid:%a offset:%a@\n"
            Base.pretty varid
            Ival.pretty
            offsets;*)
          let validity = Base.validity varid in
          begin
	    match validity with
            | Base.Unknown _ -> CilE.warn_mem_write with_alarms
            | _ -> ()
          end;
          let new_offsetmap =
            LOffset.update_ival ~with_alarms ~validity
	      ~exact ~offsets ~size offsetmap_orig v
          in
          LBase.add varid new_offsetmap map

  let create_initial ~base ~v ~modu ~state =
    match state with
    | Bottom -> state
    | Top -> state
    | Map mem ->
        Map (LBase.add base (LOffset.create_initial ~v ~modu) mem)

  let add_binding ~with_alarms ~exact initial_mem {loc=loc ; size=size } v =
    (*Format.printf "add_binding: loc:%a@\n" Location_Bits.pretty loc;*)
    if V.equal v V.bottom then Bottom else
      match initial_mem with
      | Top -> initial_mem
      | Bottom -> assert false
      | Map mem ->
          let result =
            (match loc with
             | Location_Bits.Top (Location_Bits.Top_Param.Top, orig) ->
		 (match with_alarms.imprecision_tracing with
                  | Aignore -> ()
                  | Acall f -> f ()
                  | Alog -> warn_once
	              "writing at a completely unknown address because of @[%a@]@\nAborting."
	                Origin.pretty orig);
		 warn_mem_write with_alarms;
		 (* Format.printf "dumping memory : %a@\n" pretty initial_mem;*)
		 top (* the map where every location maps to top *)
             | Location_Bits.Top (Location_Bits.Top_Param.Set set, origin) ->
		 warn_mem_write with_alarms;
		 let treat_base varid acc =
		   match Base.validity varid with
		   | (Base.Known (b,e)|Base.Unknown (b,e)) when Int.lt e b ->
		       acc
		   | Base.Unknown _ | Base.Known _ | Base.Periodic _
		   | Base.All ->
		       let offsetmap = LBase.find_or_default varid mem in
		       let offsetmap =
			 LOffset.overwrite offsetmap v origin
		       in
		       LBase.add varid offsetmap acc
		 in
		 let result =
		   Map (Location_Bits.Top_Param.O.fold treat_base set
			   (treat_base Base.null mem))
		 in
		 (*		Format.printf "debugging add_binding topset, loc =%a, result=%a@."
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
                   (match with_alarms.imprecision_tracing with
		      (* another field would be appropriate here TODO *)
                    | Aignore -> ()
                    | Acall f -> f ()
                    | Alog -> warn_once
		        "all target addresses were invalid. This path is assumed to be dead.");
                   bottom
		 end)
          in
          result

  let find ~conflate_bottom ~with_alarms mem { loc = loc ; size = size } =
    let result =
      match mem with
      | Bottom -> V.bottom
      | Top | Map _ ->
	  let find_base base =
	    ( match mem with
	      Map mem -> LBase.find_or_default base mem
	    | Top -> LOffset.empty
	    | Bottom -> assert false )
	  in
	  let handle_imprecise_base base acc =
	    let validity = Base.validity base in
	    begin
	      match validity with
	      | Base.All -> ()
	      | Base.Known _ | Base.Unknown _ | Base.Periodic _ ->
		  CilE.warn_mem_read with_alarms
	    end;
	    let offsetmap = find_base base in
	    let new_v =
	      LOffset.find_imprecise_entire_offsetmap
		~validity
		offsetmap
	    in
	    V.join new_v acc
	  in
          begin match loc with
	  |  Location_Bits.Top (topparam,_orig) ->
	       assert (size <> Int_Base.bottom);
	       begin try
		   Location_Bits.Top_Param.fold
		     handle_imprecise_base
		     topparam
		     (handle_imprecise_base Base.null V.bottom)
		 with Location_Bits.Top_Param.Error_Top -> V.top
	       end
	  | Location_Bits.Map loc_map ->
	      begin match size with
	      | Int_Base.Bottom -> V.bottom
	      | Int_Base.Top ->
		  begin try
		      Location_Bits.M.fold
			(fun base _offsetmap acc ->
			  handle_imprecise_base base acc)
			loc_map
			V.bottom
		    with Location_Bits.Top_Param.Error_Top -> V.top
		  end
	      | Int_Base.Value size ->
		  Location_Bits.M.fold
		    (fun base offsets acc ->
		      let validity = Base.validity base in
		      begin
			match validity with
			| Base.Unknown _ -> CilE.warn_mem_read with_alarms
			| _ -> ()
		      end;
		      let offsetmap = find_base base in
		      (*Format.printf "offsetmap(%a):%a@\noffsets:%a@\nsize:%a@\n"
			Base.pretty base
			(LOffset.pretty None) offsetmap
			Ival.pretty offsets
			Int.pretty size;*)
		      let new_v =
			LOffset.find_ival
			  ~conflate_bottom
			  ~validity
			  ~with_alarms
			  offsets
			  offsetmap
			  size
		      in
		      (* Format.printf "find got:%a@\n" V.pretty new_v; *)
		      V.join new_v acc)
		    loc_map
		    V.bottom
	      end
	  end
    in
    result


(* XXXXXXXXX bug with uninitialized values *)
  let reduce_binding ~with_alarms  initial_mem
      ({loc=_loc ; size=_size } as l) v =
    assert
      (if not (Locations.valid_cardinal_zero_or_one l)
	then begin
	    Format.printf "Internal error 835; debug info:@\n%a@."
	      Locations.pretty l;
	    false
	  end
	else
	  true);
    let v_old = find ~conflate_bottom:true ~with_alarms initial_mem l in
    let v = V.narrow v_old v in
    add_binding ~exact:true
      ~with_alarms
      initial_mem l v
(*
    Format.printf "reduce_binding: loc:%a@\n" Location_Bits.pretty loc;
    if V.equal v V.bottom   then None else
      match initial_mem with
      | None -> initial_mem
      | Some mem ->
          (match loc, size with
          | Location_Bits.Map loc_map, Int_Base.Value size ->
	      Format.printf "reduce_bindi@.";
	      assert (Location_Bits.cardinal_zero_or_one loc);
	      begin
		try
		  let map =
		    Location_Bits.M.fold
		      (fun varid offsets map ->
			let old_offsetmap = LBase.find varid map in
			let new_offsetmap =
			  LOffset.reduce
			    offsets
			    ~size
			    v
			    old_offsetmap
			in
			Format.printf "reduce_binding: %a =====> %a@."
			  LOffset.pretty old_offsetmap
			  LOffset.pretty new_offsetmap;
			LBase.add varid new_offsetmap map
		      )
	              loc_map
	              mem
		  in
		  Some map
		with Offsetmap.Result_is_bottom ->
                  (match with_alarms.imprecision_tracing with
		    (* another field would be appropriate here TODO *)
                  | Aignore -> ()
                  | Acall f -> f ()
                  | Alog -> warn_once
		      "Reducing state to bottom. This path is assumed to be dead.");
                  bottom
		| Offsetmap.Result_is_same -> initial_mem

	      end
	  | Location_Bits.Top _,_ | _, (Int_Base.Top | Int_Base.Bottom) ->
	      assert false)

*)





(*
  let concerned_bindings mem { loc = loc ; size = size } =
    let result =
      match mem with
      | None -> []
      | Some mem ->
          match loc with
          |  Location_Bits.Top _ ->
	       LBase.fold
                 (fun _varid offsetmap acc ->
                    LOffset.concerned_bindings_ival
                      ~offsetmap ~offsets:Ival.top ~size:Int.one acc)
                 mem
                 []
          | Location_Bits.Map loc_map ->
              Location_Bits.M.fold
	        (fun varid offsets acc ->
                   let offsets,size =
                     match size with
                     | Int_Base.Top -> Ival.top,Int.one
                     | Int_Base.Bottom -> assert false
                     | Int_Base.Value size -> offsets,size
                   in
                     let offsetmap = LBase.find_or_default varid mem in
                     LOffset.concerned_bindings_ival
                       ~offsetmap ~offsets ~size acc)
	        loc_map
	        []
    in result
*)
  let join_internal =
    let decide_none base v1 =
      snd (LOffset.join v1 (default_offsetmap base))
    in
    let decide_some v1 v2 =
	snd (LOffset.join v1 v2)
    in
    let symetric_merge =
      LBase.symetric_merge ~cache:("lmap",65536) ~decide_none ~decide_some in
    fun m1 m2 ->
      [], Map (symetric_merge m1 m2)

  let join  mm1 mm2 =
 (*   Format.printf "lmap join@." ; *)
    let result =
      match mm1, mm2 with
	Bottom,m | m,Bottom -> [], m
      | Top, _ | _, Top -> [], Top
      | Map m1, Map m2 ->
	  if m1 == m2
	  then [], mm1
	  else
	    let r = join_internal m1 m2 in
	    r
    in
(*    Format.printf "lmap.join %a %a -ZZZZ-> %a@."
      pretty mm1
      pretty mm2
      pretty result;*)
    result

let is_included =
  let decide_fst base v1 =
    LOffset.is_included_exn v1 (default_offsetmap base)
  in
  let decide_snd base v2 =
    LOffset.is_included_exn (default_offsetmap base) v2
  in
  let decide_both = LOffset.is_included_exn
  in
  let generic_is_included =
    LBase.generic_is_included Abstract_interp.Is_not_included
      ~cache:("lmap", 16384)
      ~decide_fst ~decide_snd ~decide_both
  in
  fun (m1:t) (m2:t) ->
    match m1,m2 with
      Bottom,_ -> true | _,Bottom -> false
    |	_, Top -> true | Top, _ -> false
    | Map m1,Map m2 ->
	try
          generic_is_included m1 m2;
          true
	with
          Is_not_included -> false

  let find_offsetmap_for_location loc m =
    let result = try
      match m with
        | Bottom -> assert false
        | Top -> (* TODO *) assert false
        | Map m ->
            Cilutil.out_some
              (Location_Bits.fold_i
                 (fun varid offsets acc ->
                    LOffset.shift_ival
                      (Ival.neg offsets)
                      (LBase.find_or_default varid m)
                      acc)
                 loc
                 None)
    with
      | Location_Bits.Error_Top (* from [LocBits.fold] *)
      | Offsetmap.Found_Top (* from [LOffset.shift_ival] *)
        -> LOffset.empty
    in
    (*Format.printf "find_offsetmap_for_location:%a@\nLEADS TO %a@\n"
      Location_Bits.pretty loc
      (LOffset.pretty None) result;*)
    result

  (* [is_included_actual_generic bases actual generic]
     returns [i] if the hidden variables of [generic] can
     be instanciated with an instanciation [i] so that [actual]
     is included in "[i(generic)]". Raises [Is_not_included]
     if the instanciation was not found. *)
  let is_included_actual_generic inouts (actual:t) (generic:t) =
    match actual, generic with
    | Bottom, _ -> Base.Map.empty
    | _, Bottom -> raise Is_not_included
    | Top, _ | _, Top -> assert false
    | Map actual_m, Map generic_m ->
	let bases =
	  try
	    Zone.fold_bases
	      Base.Set.add
	      inouts
	      Base.Set.empty
	  with Zone.Error_Top -> raise Is_not_included
	in
	let q = ref bases in
	let instanciation = ref Base.Map.empty in
	while not (Base.Set.is_empty !q)
	do
(*	  Format.printf
	    "Lmap.is_included_actual_generic queue: %a@\n inst %a@\n"
	    Base.Set.pretty !q
	    (Base.Poly_map.pretty Location_Bytes.pretty) !instanciation; *)
	  try
	    let base = Base.Set.choose !q in
(*	    Format.printf "Lmap.is_included_actual_generic elt: %a@\n"
	      Base.pretty base;*)
	    q := Base.Set.remove base !q;
	    let unreduced_actual =
	      if Base.is_hidden_variable base then
		let instance =
		  Base.Map.find base !instanciation
		in
		let instance_bits = loc_bytes_to_loc_bits instance in
		find_offsetmap_for_location
		  instance_bits
		  actual
	      else
		  LBase.find_or_default base actual_m
	    in
	    let unreduced_generic = LBase.find_or_default base generic_m in
	    let offsmap_actual, offsmap_generic =
	      match inouts with
	      |	Zone.Map m ->
		  let int_intervals = Zone.find_or_bottom base m in
		  LOffset.reduce_by_int_intervals
		    unreduced_actual
		    int_intervals,
		  LOffset.reduce_by_int_intervals
		    unreduced_generic
		    int_intervals
	      | Zone.Top _ ->
		  unreduced_actual, unreduced_generic
	    in

(*	    Format.printf
	      "Lmap.is_included_actual_generic offsmap_actual: %a@\n"
	      (LOffset.pretty_debug) offsmap_actual; *)
(*	    Format.printf
	      "Lmap.is_included_actual_generic offsmap_generic: %a@\n"
	      (LOffset.pretty_debug) offsmap_generic;*)
	    LOffset.is_included_actual_generic
              bases
	      q
	      instanciation
	      offsmap_actual
	      offsmap_generic;
(*	    Format.printf
	      "Lmap.is_included_actual_generic: There was inclusion@\n" *)
	  with Not_found (* from Base.Poly_map.find *) -> ()
	    (* we'll do [base] when it is instanciated *)
	done;
	!instanciation

  let is_included_by_location_enum m1 m2 locs =
    if Zone.equal locs Zone.bottom  then true
    else match locs with
      | Zone.Top _ -> is_included m1 m2
      | Zone.Map locs ->
          match m1, m2 with
	  | Top, _ | _, Top -> assert false
            | Bottom ,_ -> assert false
            | _, Bottom -> assert false
            | Map m1, Map m2 ->
                let treat_offset varid offs2  =
		  try
	            ignore (Zone.find_or_bottom varid locs);
		    (* at this point varid is present in locs *)
                      let offs1 = LBase.find_or_default varid m1
                      in LOffset.is_included_exn offs1 offs2
                  with Not_found -> () (* varid not in locs *)
                in
                try
	          LBase.iter treat_offset m2;
	          true
                with
	          Is_not_included -> false
(*
  let top = empty

  let bottom  = None
*)
  (* Precondition : m1 <= m2 *)
  type widen_hint = bool * Base.Set.t * (Base.t -> widen_hint_offsetmap)
  let widen (widen_other_keys, wh_key_set, wh_hints) r1 r2 =
    let result =
      match r1,r2 with
      | Top, _ | _, Top -> assert false
    | Bottom,Bottom -> false, Bottom
    | _,Bottom -> assert false (* thanks to precondition *)
    | Bottom, m -> false, m
    | Map m1,Map m2 ->
        let m_done, m_remain =
          (* [m_done] = widened state on keys of [wh_key_set].
             if a widening is performed for one of them,
             [m_remain] will be empty.
          *)
          Base.Set.fold
            (fun key (m_done, m_remain) ->
               let offs2 = LBase.find_or_default key m2 in
               let offs1 = LBase.find_or_default key m1 in
               let fixed = LOffset.is_included offs2 offs1 in
                 (* Format.printf "key=%a, fixed=%b@."
                    Base.pretty key fixed; *)
                 if fixed
                 then (m_done, LBase.remove key m_remain)
                 else
		   let new_off = LOffset.widen (wh_hints key) offs1 offs2
		   in
                     LBase.add key new_off m_done, LBase.empty)
	    wh_key_set
	    (m2, m2)
        in
	let fixed_for_all_wh_key = not (LBase.is_empty m_remain) in
          (* Format.printf "widening (widen_other_keys=%b, fixed_for_all_wh_key %b)@."
            widen_other_keys fixed_for_all_wh_key; *)
        if widen_other_keys
          then
            let other_keys_widened =
              Map
                (LBase.fold
	           (fun base offs2 acc ->
                      (* Format.printf "widening also on key %a@."
                        Base.pretty base; *)
                        let offs1 = LBase.find_or_default base m1 in
                        let new_off =
                          LOffset.widen (wh_hints base) offs1 offs2
                        in
                        LBase.add base new_off acc)
	           m_remain
	           m_done)
            in
              true, other_keys_widened
          else
            fixed_for_all_wh_key, Map m_done
        in
    result

(* TODO: in order for copy_paste to be able to handle a semi-valid [dst_loc],
   this function needs to have a ~with_alarms argument *)
  let paste_offsetmap map_to_copy dst_loc start size m =
    match m with
    | Bottom | Top -> assert false
    | Map m ->
    let dst_is_exact =
      Locations.valid_cardinal_zero_or_one
	(Locations.make_loc dst_loc (Int_Base.inject size))
    in
    let stop = Int.pred (Int.add start size) in
    let had_non_bottom = ref false in
    let plevel = Parameters.Dynamic.Int.get "-plevel" in
    let treat_dst k_dst i_dst (acc_lmap : LBase.t) =
      let validity = Base.validity k_dst in
      let offsetmap_dst = LBase.find_or_default k_dst m in
      let new_offsetmap =
        try
	  ignore (Ival.cardinal_less_than i_dst plevel);
	  Ival.fold
            (fun start_to acc ->
	      let stop_to = Int.pred (Int.add start_to size) in
	      match validity with
	      | Base.Periodic _ -> raise Cannot_copy
	      | Base.Known (b,e)
	      | Base.Unknown (b,e) when Int.lt start_to b || Int.gt stop_to e ->
		  CilE.warn_mem_write CilE.warn_all_mode;
		  acc
	      | Base.Known _ | Base.All | Base.Unknown _ ->
		  had_non_bottom := true;
		  (if dst_is_exact then LOffset.copy_paste
		    else LOffset.copy_merge)
		    map_to_copy
		    start
		    stop
		    start_to
		    acc)
            i_dst
            offsetmap_dst
	with Not_less_than ->
	  raise Cannot_copy
      in
      LBase.add k_dst new_offsetmap acc_lmap
    in
    try
      let result = Location_Bits.fold_i treat_dst dst_loc m in
      if not !had_non_bottom
      then begin
          ignore (CilE.warn_once
                     "all target addresses were invalid. This path is assumed to be dead.");
          bottom
	end
      else Map result
    with Location_Bits.Error_Top -> (* from Location_Bits.fold_i *)
      raise Cannot_copy

  let copy_offsetmap ~with_alarms src_loc mm =
    match mm with
      | Bottom -> None
      | Top -> Some LOffset.empty
      | Map m ->
	  begin
            try
              let size = Int_Base.project src_loc.size in
	      try
                begin
                let treat_src k_src i_src (acc : LOffset.t option) =
		  let validity = Base.validity k_src in
                    let offsetmap_src = LBase.find_or_default k_src m in
		    let copy =
		      LOffset.copy_ival
			~validity
			~with_alarms
			i_src
			offsetmap_src
			size
		    in
		      match acc with
		      | None -> Some copy
		      | Some acc ->
			  let r = snd (LOffset.join copy acc) in
			  if LOffset.is_empty r then
			    raise Not_found;
                          Some r

                in
                Location_Bits.fold_i treat_src src_loc.loc None
	        end
              with
	    | Location_Bits.Error_Top (* from Location_Bits.fold *)
            | Cannot_copy (* from LOffset.copy_ival *) ->
	        let v =
		  find ~conflate_bottom:false ~with_alarms
		    mm src_loc
		in
                Some
		  (LOffset.update_ival
                      ~with_alarms:warn_none_mode
                      ~validity:Base.All
                      ~exact:true
                      ~offsets:Ival.zero
                      ~size
                      LOffset.empty
                      v)
	    with
	    | Int_Base.Error_Top  (* from Int_Base.project *) ->
                Some LOffset.empty
          end


  let copy_paste src_loc dst_loc mm =
    assert (Int_Base.equal src_loc.size dst_loc.size );

(* temporary fix *)
      if not (Locations.can_be_accessed src_loc
                  && Locations.can_be_accessed dst_loc)
          then raise Cannot_copy;

    try
      let size = Int_Base.project src_loc.size in
      let result =
	copy_offsetmap ~with_alarms:warn_none_mode src_loc mm in
      match result with
      | Some result ->
          paste_offsetmap result dst_loc.loc Int.zero size mm
      | None -> bottom
    with
    | Int_Base.Error_Top  (* from Int_Base.project *) ->
	raise Cannot_copy

  let fold ~size f m acc =
    match m with
      | Bottom -> raise Error_Bottom
      | Top -> assert false
      | Map m ->
          try
            LBase.fold
              (fun k v acc ->
                 LOffset.fold_whole
                   ~size
                 (fun ival size v acc ->
                    let loc = Location_Bits.inject k ival in
                    f (make_loc loc (Int_Base.inject size)) v acc)
                   v
                   acc)
              m
              acc
          with Invalid_argument "Offsetmap.Make.fold" ->
            raise (Invalid_argument "Lmap.fold")

  let fold_single_bindings ~size f m acc =
    match m with
    | Bottom -> raise Error_Bottom
      | Top -> assert false
    | Map m ->
        try
          LBase.fold
            (fun k v acc ->
              LOffset.fold_single_bindings
                ~size
                (fun ival size v acc ->
                  let loc = Location_Bits.inject k ival in
                  f (make_loc loc (Int_Base.inject size)) v acc)
                v
                acc)
            m
            acc
        with Invalid_argument "Offsetmap.Make.fold" ->
          raise (Invalid_argument "Lmap.fold")

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

  let compute_actual_final_from_generic
      actual_orig generic_final filter instanciation =
    match generic_final with
    | Bottom ->
        Bottom,  (* the called function does not terminate *)
        Location_Bits.Top_Param.bottom
    | Top -> assert false
    | Map generic_finalcontent ->
        let actual_orig =
	  match actual_orig with
	    Map x -> x
	  | Bottom | Top -> assert false
	in
        try
          let result, clobbered =
            Zone.fold_i
            (fun base itvs (acc,clobbered_acc) ->
               let new_offsetmap =
		 LBase.find_or_default base generic_finalcontent
               in
	       let new_acc =
                 if Base.is_hidden_variable base
		 then
		   let instance =
		     try
		       Base.Map.find base instanciation
		     with Not_found ->
		       Format.printf
                         "Internal error: hidden variable %a appears in generic state but not in instanciation@."
			 Base.pretty base;
		       assert false
		   in
		   let instance_bits = loc_bytes_to_loc_bits instance in
		   begin try
		     let instance_base, instance_offset =
		       Location_Bits.find_lonely_binding instance_bits
		     in
		     let instance_offset = Ival.project_int instance_offset in
		     let original_offsetmap =
		       LBase.find_or_default instance_base actual_orig
                     in
		     let shifted_original =
		       LOffset.shift (Int.neg instance_offset)
			 original_offsetmap
		     in
		     let merged_offsetmap =
		       LOffset.merge_by_itv
			 shifted_original
			 new_offsetmap
			 itvs
		     in
		     let shifted_back_result =
		       LOffset.shift
			 instance_offset
			 merged_offsetmap
		     in
	             (*	      Format.printf "caffg: shifted original:%a@\nnew:%a@\nresult:%a@\n"
			      (LOffset.pretty None) shifted_original
			      (LOffset.pretty None) new_offsetmap
			      (LOffset.pretty None) shifted_back_result; *)
		     LBase.add instance_base shifted_back_result acc
		   with Not_found | Ival.Not_Singleton_Int ->
		     assert false (* TODO: is it possible to be more general?*)
		   end
		 else begin
                   let original_offsemap =
		     LBase.find_or_default base actual_orig
                   in
                   let merged_offsetmap =
                     LOffset.merge_by_itv original_offsemap new_offsetmap itvs
                   in
                   LBase.add base merged_offsetmap acc
		 end
               in
               new_acc,
               Location_Bits.Top_Param.join
                 (Location_Bits.Top_Param.inject_singleton base)
                 clobbered_acc)
            filter
            (actual_orig,Location_Bits.Top_Param.bottom)
          in
          Map result, clobbered
        with Zone.Error_Top ->
          generic_final, (* [filter] is [top] *)
          Location_Bits.Top_Param.top


  let reciprocal_image base m = (*: Base.t -> t -> Zone.t*Location_Bits.t*)
    match m with
    | Bottom -> assert false
    | Top -> assert false
    | Map m ->
        if Base.is_null base then Zone.top,Location_Bits.top
        else
          LBase.fold
            (fun b offs (acc1,acc2) ->
               let interv_set,ival = LOffset.reciprocal_image offs base in
               let acc1 = Zone.join acc1 (Zone.inject b interv_set) in
               let acc2 = Location_Bits.join acc2 (Location_Bits.inject b ival) in
               acc1,acc2)
            m
            (Zone.bottom,Location_Bits.bottom)

  let cached_fold ~f ~cache ~temporary ~joiner ~empty =
    let cached_f = LBase.cached_fold ~f ~cache ~temporary ~joiner ~empty
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
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
