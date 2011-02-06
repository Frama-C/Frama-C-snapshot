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

open Locations
open Abstract_interp
open Abstract_value
open Cvalue_type

(* To be raised whenever we need to fall back to values computations *)
exception Use_Main_Memory

module V_Offsetmap_For_Relations = Offsetmap.Make(V)

module Partial_lmap = Lmap_whole.Make_LOffset(V)(V_Offsetmap_For_Relations)

module Relation_between =
  Partial_lmap.Make
    (struct let default_offsetmap _ = V_Offsetmap_For_Relations.empty end)

module Cluster = struct

  type tt =
      { id : int; (* unique identifier *)
	size : Int.t; (* size of the values that are in the relation *)
	contents : V_Offsetmap.t;
            (* optional offsetmap. Only makes sense if
               size = sizeofpointer(), and (with some exceptions),
               when the values in rel are not integers *)
	rel : Relation_between.t;
            (* maps the values in the relation to an offset indicating
               their position wrt each other *)
	virtual_to_real : Location_Bits.t }

  let compare x y = Datatype.Int.compare x.id y.id
  let equal x y = x.id = y.id
  let hash t = t.id

  let rel_at_least_2 ~rel ~size =
    try
      let counter = Relation_between.fold
	~size
	(fun k v counter ->
          assert (if V.cardinal_zero_or_one v then
                    true
                  else
                    (Format.printf "k:%a@.v:%a@."
                      Locations.pretty k
                      V.pretty v;
                     false)
                 );
	   let card = Location_Bits.cardinal_less_than k.loc 2 in
           card + counter)
	rel
	0
      in
      counter >= 2
    with
    | Invalid_argument "Lmap.fold" -> assert false
    | Not_less_than -> true

  let has_information cluster =
    (not (Relation_between.is_empty cluster.rel)) &&
	( rel_at_least_2 ~size:cluster.size ~rel:cluster.rel ||
	    not (V_Offsetmap.is_empty cluster.contents))

  let pretty fmt c =
    Format.fprintf fmt "[[%d: rel=%a virtual_to_real:%a contents=%a]]" c.id
      Relation_between.pretty c.rel
      Location_Bits.pretty c.virtual_to_real
      V_Offsetmap.pretty c.contents

  let cluster_counter = ref 0

  let make ~size ~contents ~rel ~virtual_to_real =
    assert (not (Location_Bits.cardinal_zero_or_one virtual_to_real));
    let new_count = succ !cluster_counter in
    if new_count = 0
    then begin
      Format.printf "Internal limit reached. Please report@.";
      exit 1;
    end;
    cluster_counter := new_count;
    let c =
      { id = new_count;
        size = size;
        contents = contents;
        rel = rel;
        virtual_to_real = virtual_to_real }
    in
    assert (if not (has_information c)
	    then begin
	      Cil.warning "Internal error while creating cluster %a@."
		pretty c;
	      false
	    end
	    else true);
    c

  exception Stop
  exception No_more_cluster

  let filter_base f c =
    let keep_content =
      try
	Location_Bits.fold_i
	  (fun k _v _acc -> if not (f k) then raise Stop)
	  c.virtual_to_real
	  ();
        true
      with Stop | Location_Bits.Error_Top -> false
    in
    let new_rel = Relation_between.filter_base f c.rel in
    let new_contents =
      if keep_content then
        c.contents
      else
        V_Offsetmap.empty
    in
    if not (has_information { c with rel = new_rel ; contents = new_contents })
    then raise No_more_cluster
    else make
      ~size:c.size
      ~contents:new_contents
      ~virtual_to_real:c.virtual_to_real
      ~rel:new_rel

  include Datatype.Make_with_collections
    (struct
      type t = tt
      let name = "Relation_types.Cluster"
      let structural_descr =
	Structural_descr.t_tuple
	  [| Structural_descr.p_int;
	     Int.packed_descr;
	     V_Offsetmap.packed_descr;
	     Relation_between.packed_descr;
	     Location_Bits.packed_descr |]
      let reprs =
	List.fold_left
	  (fun acc o ->
	    List.fold_left
	      (fun acc r ->
		List.fold_left
		  (fun acc l ->
		    { id = -1;
		      size = Int.zero;
		      contents = o;
		      rel = r;
		      virtual_to_real = l }
		    :: acc)
		  acc
		  Location_Bits.reprs)
	      acc
	      Relation_between.reprs)
	  []
	  V_Offsetmap.reprs
      let hash = hash
      let compare = compare
      let equal = equal
      let pretty = pretty
      let rehash = Datatype.identity
(*	let module H =
	      Hashtbl.Make(struct
		type t = tt
		let hash c = c.id
		let equal c d = c.id = d.id
	      end)
	in
	let rehash_table = H.create 17 in
	fun c ->
	  try
	    H.find rehash_table c;
	    c
	  with Not_found ->
	    cluster_counter := Extlib.max_cpt c.id !cluster_counter;
	    H.add rehash_table c ();
	    c*)
      let copy = Datatype.undefined
      let internal_pretty_code = Datatype.undefined
      let varname = Datatype.undefined
      let mem_project = Datatype.never_any_project
     end)

end

type cluster_info = No_cluster | Cluster of Cluster.t | Bottom_cluster

module Cluster_Info = struct

  module Top_Param = VarinfoSetLattice
  type widen_hint = unit

  let hash v =
    match v with
      No_cluster -> 1975
    | Bottom_cluster -> 19751
    | Cluster c -> c.Cluster.id

  let tag = hash

  let compare x y =
    match x, y with
      No_cluster, No_cluster | Bottom_cluster, Bottom_cluster -> 0
    | No_cluster, _ -> 1
    | _, No_cluster -> -1
    | Bottom_cluster, _ -> 1
    | _, Bottom_cluster -> -1
    | Cluster c1, Cluster c2 -> Datatype.Int.compare c1.Cluster.id c2.Cluster.id

  let equal x y = compare x y = 0

  let pretty fmt v = match v with
    | No_cluster -> Format.fprintf fmt "NoCluster"
    | Bottom_cluster -> Format.fprintf fmt "BottomCluster"
    | Cluster c -> Cluster.pretty fmt c

  include Datatype.Make
  (struct
    type t = cluster_info
    let name = "Relation_types.Cluster_Info"
    let reprs =
      No_cluster
      :: Bottom_cluster
      :: List.map (fun c -> Cluster c) Cluster.reprs
    let structural_descr =
      Structural_descr.Structure
	(Structural_descr.Sum [| [| Cluster.packed_descr |] |])
    let hash = hash
    let equal = equal
    let compare = compare
    let pretty = pretty
    let rehash = Datatype.identity
    let copy = Datatype.undefined
    let internal_pretty_code = Datatype.undefined
    let varname = Datatype.undefined
    let mem_project = Datatype.never_any_project
   end)

  let project _ = assert false

  let join x y =
    match x, y with
      Bottom_cluster, Bottom_cluster -> Bottom_cluster
    | Bottom_cluster, x | x, Bottom_cluster -> x
    | Cluster xx, Cluster yy when xx.Cluster.id = yy.Cluster.id -> x
    | _ -> No_cluster
  let meet _ _ = Bottom_cluster

  let default _base _b _e = No_cluster
  let defaultall _base = No_cluster

  let cardinal_less_than _ = assert false
  let cardinal_zero_or_one = function
      Cluster _ -> true
    | Bottom_cluster -> assert false
    | No_cluster -> false

  let link _ = assert false (* Not implemented yet. *)
  let narrow _ = assert false (* Not implemented yet. *)
  let widen _ = assert false (* Not implemented yet. *)

  let little_endian_merge_bits ~conflate_bottom:_ ~total_length:_ ~value:_ ~offset:_ _ = No_cluster
  let big_endian_merge_bits ~conflate_bottom:_ ~total_length:_ ~length:_ ~value:_ ~offset:_ _ = No_cluster

  let intersects _ = assert false
  let is_included _ = assert false
  let is_included_exn _ = assert false
  let is_included_actual_generic _ = assert false
  let top = No_cluster
  let inject_top_origin _ = assert false
  let top_absolute_origin () = top
  let top_int = top
  let of_char _ = top

  let bottom = Bottom_cluster

  exception Error_Bottom
  exception Error_Top

  let all_values ~size:_ _ = assert false

  let anisotropic_cast ~size:_ s =
    (*Format.printf "Size for anisotropic cast:%a@\n" Int.pretty size;*)
    s
  let topify _ = top
  let under_topify _ = assert false
  let topify_misaligned_read_origin = topify
  let topify_arith_origin = topify
  let topify_merge_origin = topify
  let topify_with_origin _ = topify
  let cast ~with_alarms:_ ~size:_ ~signed:_ _ = assert false
  let is_isotropic x = equal x top

  exception Cannot_extract
  let extract_bits ~start:_ ~stop:_  _ =
    raise Cannot_extract
end

let same_clusters c cluster = c.Cluster.id = cluster.Cluster.id

let same_clusterinfo_cluster cinfo cluster =
  match cinfo with
  | Cluster c when same_clusters c cluster -> true
  | _ -> false

(*
module type Participation_Map_S = sig
  include Lmap.Location_map with type y = cluster_info
  val add_binding :  t -> Locations.location -> y -> t
  val find : t -> Locations.location -> y
end
*)
(* Each location in the map is associated to a single cluster.
   A binding must be present for every location that belongs to rel field of
   one of the clusters in the map. *)
module Participation_Map = struct

  module Cluster_Info_plus =
  struct
    include Cluster_Info
    let singleton_zero = top
    let pretty fmt c = match c with
    | Cluster c -> Format.fprintf fmt "C:%d" c.Cluster.id
    | No_cluster -> Format.fprintf fmt "C:no"
    | Bottom_cluster -> Format.fprintf fmt "C:bottom"
    let pretty_c_assert _lv _s _fmt _v = assert false
  end

  module Cluster_Info_Offsetmap = Offsetmap.Make(Cluster_Info_plus)

  module Partial_Participation_Map =
    Lmap_whole.Make_LOffset(Cluster_Info_plus)(Cluster_Info_Offsetmap)

  open Partial_Participation_Map

  include 
      Make(struct let default_offsetmap _ = Cluster_Info_Offsetmap.empty end)

  let add_binding s x =
    assert (Location_Bits.is_relationable x.loc);
    add_binding s ~exact:true x


  let add_whole loc v m =
    assert (Location_Bits.is_relationable loc.loc);
    add_whole loc v m

  let find  m l =
    try
      find  ~with_alarms:CilE.warn_none_mode m l
    with Cluster_Info.Cannot_extract -> No_cluster

end

type tt =
    { participation_map : Participation_Map.t;
      all_clusters : Cluster.Set.t }

let pretty_tt fmt v =
  Format.fprintf fmt
    "PartMap:%a@\nClusters=%a@\n"
    Participation_Map.pretty v.participation_map
    (fun fmt () ->
       Cluster.Set.iter
	 (fun c -> Format.fprintf fmt "Cluster:%a@\n" Cluster.pretty c)
         v.all_clusters)
    ()

let empty_tt =
  { participation_map = Participation_Map.empty;
    all_clusters = Cluster.Set.empty}

let index_cluster_into_participation_map cluster pmap =
  let copy_loc loc _ acc =
(*    Format.printf "copy_loc size:%a@\n" Int_Base.pretty lc.Cluster.size;*)
    Participation_Map.add_whole loc (Cluster cluster) acc
  in
  Relation_between.fold_single_bindings
    ~size:cluster.Cluster.size
    copy_loc
    cluster.Cluster.rel
    pmap

let remove_cluster_from_participation_map cluster pmap =
  let copy_loc loc _ acc =
    Participation_Map.remove_whole loc acc
  in
  Relation_between.fold_single_bindings
    ~size:cluster.Cluster.size
    copy_loc
    cluster.Cluster.rel
    pmap

let check_tt map =
  Cluster.Set.fold
    (fun c () ->
      Relation_between.fold
        ~size:c.Cluster.size
	(fun loc _ () ->
	  match Participation_Map.find map.participation_map loc with
	    Cluster other_c when Cluster.equal c other_c ->
	      ()
	  | _ ->
	      Format.printf "Relation_type.check_tt %a@."
		pretty_tt map;
	      assert false)
	c.Cluster.rel
	())
    map.all_clusters
    ()


let add_new_cluster new_cluster map =
  let new_participation_map =
    index_cluster_into_participation_map
      new_cluster
      map.participation_map
  in
  let new_all_clusters = Cluster.Set.add new_cluster map.all_clusters in
  let result =
    { participation_map = new_participation_map;
      all_clusters = new_all_clusters; }
  in
  assert (check_tt result; true);
  result





(* precondition : the locations involved in new_cluster must include
   all the locations involved in old_cluster *)
let replace_cluster ~old_cluster ~new_cluster map =
  let new_participation_map =
    index_cluster_into_participation_map
      new_cluster
      map.participation_map
  in
  let new_all_clusters =
    Cluster.Set.add
      new_cluster
      (Cluster.Set.remove old_cluster map.all_clusters)
  in
  let result =
    { participation_map = new_participation_map;
      all_clusters = new_all_clusters; }
  in
  assert (check_tt result; true);
  result

let remove_cluster ~old_cluster map =
  let new_participation_map =
    remove_cluster_from_participation_map
      old_cluster
      map.participation_map
  in
  let new_all_clusters = Cluster.Set.remove old_cluster map.all_clusters in
  let result =
    { participation_map = new_participation_map;
      all_clusters = new_all_clusters; }
  in
  assert (check_tt result; true);
  result

(*
let replace_cluster ~old_cluster ~new_cluster map =
  add_new_cluster new_cluster (remove_cluster ~old_cluster map)
*)

let virtual_to_real main_memory loc =
  loc_bytes_to_loc_bits
    (Model.find ~conflate_bottom:true ~with_alarms:CilE.warn_none_mode
	main_memory loc)

exception Tt_not_included

let is_included_rel tt1 c2 =
  let f loc v2 acc =
    match acc with
    | None ->
	begin
	  match Participation_Map.find tt1.participation_map loc with
	    Cluster c1 ->
	      if Int.compare c2.Cluster.size c1.Cluster.size <> 0
	      then raise Tt_not_included;
	      let v1 =
                Relation_between.find ~with_alarms:CilE.warn_none_mode 
		  c1.Cluster.rel loc
              in
	      let diff = V.add_untyped (Int_Base.minus_one) v2 v1 in
	      let diff_ival = V.project_ival diff in
	      if not (Ival.cardinal_zero_or_one diff_ival)
	      then raise Tt_not_included;
	      Some (c1,diff)
	  | No_cluster -> raise Tt_not_included
	  | Bottom_cluster -> assert false
	end
    | Some(c1,d) ->
	let v1 = Relation_between.find ~with_alarms:CilE.warn_none_mode c1.Cluster.rel loc in
	let diff = V.add_untyped (Int_Base.minus_one) v2 v1 in
	if not (V.equal d diff )
	then raise Tt_not_included;
	acc
  in
  try
    match (Relation_between.fold
      ~size:c2.Cluster.size
      f
      c2.Cluster.rel
      None) with
      | None -> assert false
      | Some v -> v
  with V.Not_based_on_null -> raise Tt_not_included

let is_included_offsetmap diff c1 c2 =
(*  if not (V.is_zero diff)
  then begin
    Format.printf "Calling is_included_offsetmap diff:%a c1:%a c2:%a@\n"
      V.pretty diff
      Cluster.pretty c1
      Cluster.pretty c2;
  end; *)
  try
    let varid, offs = V.find_lonely_key diff in
    assert (Base.is_null varid);
    let offs = Int.mul (Bit_utils.sizeofchar()) (Ival.project_int offs) in
    let shifted_c2 =
      V_Offsetmap.shift offs c2.Cluster.contents
    in
(*    if not (V.is_zero diff)
    then
      Format.printf "shifted_c2:%a@\n"
	(V_Offsetmap_For_Relations.pretty None) shifted_c2;	*)
    V_Offsetmap.is_included
      c1.Cluster.contents
      shifted_c2;
  with
    Not_found | Ival.Not_Singleton_Int -> assert false

let is_included_tt tt1 tt2 =
  (*Format.printf "is_included_tt@ @[%a@]@ @[%a@]===@\n"
    pretty_tt tt1
    pretty_tt tt2;*)
  let check_cluster c2 =
    (*Format.printf "is_included_tt->check_cluster@ @[%a@]@\n"
      Cluster.pretty c2;*)
    let c1,diff = is_included_rel tt1 c2 in
    if not (is_included_offsetmap diff c1 c2)
    then raise Tt_not_included
  in
  try
    Cluster.Set.iter check_cluster tt2.all_clusters;
    true
  with Tt_not_included ->
    false

let find_hint h k =
  try Int.Hashtbl.find h k
  with Not_found -> []

let join (m1 : tt) (m2 : tt) =
  let f cluster1 cluster2 acc =
    if not (Int.equal cluster1.Cluster.size cluster2.Cluster.size)
    then acc
    else (* Clusters have the same size for the values *)
	(*      if cluster1.Cluster.id = cluster2.Cluster.id then ((*variables de cluster2*),cluster2)::acc
		else -- optimisation qui nécessite de réfléchir pour être activée *)
      let h = Int.Hashtbl.create 7 in
      Relation_between.fold_single_bindings
        ~size:cluster1.Cluster.size
        (fun loc1 v1 _acc ->
          let v2 =
	    Relation_between.find ~with_alarms:CilE.warn_none_mode
	      cluster2.Cluster.rel
	      loc1
	  in
          if Location_Bytes.cardinal_zero_or_one v2 then
            let delta = V.add_untyped
              Int_Base.minus_one
              v1 v2
            in
            try
              let delta_i =
                Ival.project_int
		  (V.project_ival delta)
              in
              Int.Hashtbl.replace h delta_i (loc1::(find_hint h delta_i))
            with
            | V.Not_based_on_null
            | Ival.Not_Singleton_Int ->  ())
        cluster1.Cluster.rel
	();
      let create_subcluster
	  offs locs acc =
	let offs = Int.mul offs (Bit_utils.sizeofchar()) in
	  (*	  if not (Int.is_zero offs) then
		  Format.printf "shift:%a@\n" Int.pretty offs; *)
	let _,new_contents =
	  V_Offsetmap.join
	    (V_Offsetmap.shift offs cluster1.Cluster.contents)
	    cluster2.Cluster.contents
	in
	match locs with
	| [] -> assert false (* Int.Hashtbl.fold should not call this function
				on the empty list *)
	| [_] when V_Offsetmap.is_empty new_contents ->
	      (* there is no information in this cluster. *)
	  acc
	| _ ->
	  let new_rel =
            List.fold_left
              (fun acc loc ->
                Location_Bits.fold_enum
		  ~split_non_enumerable:(-1)
                  (fun loc_no_size acc ->
                    let loc = make_loc loc_no_size loc.size in
                    let v = Relation_between.find
                      ~with_alarms:CilE.warn_none_mode
                      cluster2.Cluster.rel
                      loc
                    in
                    assert (Location_Bits.cardinal_zero_or_one loc.loc);
                    Relation_between.add_whole loc v acc)
                  loc.loc
                  acc)
              Relation_between.empty
              locs
          in
	  let new_virtual_to_real =
	    Location_Bits.join
	      (Location_Bits.location_shift
		 (Ival.inject_singleton (Int.neg offs))
		 cluster1.Cluster.virtual_to_real)
	      cluster2.Cluster.virtual_to_real
	  in
	  let new_cluster =
	    Cluster.make
	      ~size:cluster1.Cluster.size
	      ~contents:new_contents
	      ~rel:new_rel
	      ~virtual_to_real:new_virtual_to_real
	  in
	  add_new_cluster new_cluster acc
      in
      Int.Hashtbl.fold create_subcluster h acc
  in
  let final_state =
    Cluster.Set.fold
      (fun cluster1 acc ->
	Cluster.Set.fold
	  (fun cluster2 acc ->
	    f cluster1 cluster2 acc)
	  m2.all_clusters
	  acc
      )
      m1.all_clusters
      empty_tt
  in
  (*  Format.printf "relation join:%a %a -> %a@\n"
      pretty_tt m1
      pretty_tt m2
      pretty_tt final_state; *)
  final_state

module type Model_S = sig
  include Datatype.S
  type widen_hint = Model.widen_hint
  type cluster
  val is_reachable : t -> bool
  val pretty_c_assert : Format.formatter -> t -> unit
  val pretty_without_null : Format.formatter -> t -> unit
  val pretty_filter :
    Format.formatter -> t -> Zone.t -> (Base.t -> bool) -> unit
  val join : t -> t -> t
  val find :
    conflate_bottom:bool ->
    with_alarms:CilE.warn_mode ->
    t ->
    location ->
    Location_Bytes.t
  val find_unspecified : with_alarms:CilE.warn_mode -> t -> location ->
    Cvalue_type.V_Or_Uninitialized.t

  val add_binding :
    with_alarms:CilE.warn_mode ->
    exact:bool ->
    t ->
    location ->
    Location_Bytes.t ->
    t
  val add_binding_unspecified : t -> location -> t

  val reduce_binding : t -> location -> Location_Bytes.t -> t
  val is_included : t -> t -> bool
  val is_included_actual_generic :
    Zone.t -> t -> t -> Location_Bytes.t Base.Map.t
  val widen :  widen_hint -> t -> t -> (bool * t)
  val bottom : t
  val inject : Model.t -> t
  val empty_map : t
  val top : t
  val is_top: t -> bool
  val value_state : t -> Model.t
  val drop_relations : t -> t
  val filter_base : (Base.t -> bool) -> t -> t
  val remove_base : Base.t -> t -> t
  val clear_state_from_locals : Cil_types.fundec -> t -> t
  val uninitialize_locals: Cil_types.block list -> t -> t
  val compute_actual_final_from_generic :
    t -> t -> Zone.t -> Model.instanciation -> t * Location_Bits.Top_Param.t
  val is_included_by_location_enum :  t -> t -> Zone.t -> bool

  val find_mem : location -> Int_Base.t ->
    Ival.t -> t -> V.t
  val add_mem : location -> Int_Base.t ->
    Ival.t -> t -> V.t -> cluster list * t
  val propagate_change_from_real_to_virt :
    protected_clusters:cluster list -> location -> t -> V.t -> t

  val add_equality : ?offset:Ival.t -> t -> location -> location -> t
  val reduce_equality : t -> location -> location -> t
  val compute_diff : t -> location -> location -> V.t
  val shift_location : t -> location -> Ival.t -> V.t -> t
  val find_base : Base.t -> t -> V_Offsetmap.t
  val create_initial : base:Base.t ->
    v:V.t ->
    modu:Int.t ->
    state:t -> t

  val paste_offsetmap :
    Cvalue_type.V_Offsetmap.t -> Location_Bits.t -> Int.t -> Int.t -> t -> t
  val copy_paste : location  -> location -> t -> t
  val copy_from_virtual :
    location ->
    Ival.t ->
    Int.t -> t -> Cvalue_type.V_Offsetmap.t
  val copy_offsetmap : with_alarms:CilE.warn_mode ->
    Locations.location -> t -> Cvalue_type.V_Offsetmap.t option

  val comp_prefixes: t -> t -> unit
  val find_prefix : t -> Hptmap.prefix -> Cvalue_type.Model.subtree option

end

module Model : Model_S with type t = Cvalue_type.Model.t * tt = struct

  type model = Cvalue_type.Model.t * tt
  type widen_hint = Model.widen_hint
  type cluster = Cluster.t

  let is_reachable (x,_) = Model.is_reachable x

  let create_initial ~base ~v ~modu ~state:(s,r) =
    (Model.create_initial ~base ~v ~modu ~state:s),r

  let copy_offsetmap ~with_alarms l (x,_) =
    Model.copy_offsetmap ~with_alarms l x

  let pretty fmt (x,y) =
    Model.pretty fmt x;
    if Kernel.debug_atleast 1 then pretty_tt fmt y

  let pretty_c_assert fmt (x,_) =
    Model.pretty_c_assert fmt x

  let pretty_without_null fmt (x,y) =
    Model.pretty_without_null fmt x;
    if Kernel.debug_atleast 1 then pretty_tt fmt y

  let pretty_filter fmt (x,_y) outs refilter =
    Model.pretty_filter fmt x outs refilter

  let join (a,b as f) (c,d as s) =
    let result =
      if not (is_reachable f) then s
      else if not (is_reachable s) then f
      else
	let _l,value_state = Model.join a c in
	value_state, join b d
    in
    (*Format.printf "f:%a@\ns:%a@\nresult:%a@\n"
      pretty f pretty s pretty result;*)
    (*Format.printf "Rtype.Model.join finished@.";*)
    result

  let find_base vi (t,_) = Model.find_base vi t

  let value_state (x,_y) = x

  let find_unspecified ~with_alarms (x,_) loc = Model.find_unspecified ~with_alarms x loc

  let find ~conflate_bottom ~with_alarms (x,_) loc =
    Model.find ~conflate_bottom ~with_alarms x loc

  let bottom = Model.bottom, empty_tt
  let top = Model.top, empty_tt
  let empty_map = Model.empty_map, empty_tt

  let is_top (a,_) = Model.equal a Model.top
  let inject s = s, empty_tt

  let add_binding ~with_alarms ~exact (s,rel) left v =
    let r = Model.add_binding ~with_alarms ~exact s left v in
    r, rel

  let add_binding_unspecified (s,rel) left =
    Model.add_binding_unspecified s left,
    rel

  let is_included (a,a') (b,b') =
    Model.is_included a b && (is_included_tt a' b')

  let equal (a,a') (b,b') =
    Model.equal a b &&
      (is_included_tt a' b') && (is_included_tt b' a')
      (* TODO: make more efficient *)

  let is_included_actual_generic inouts (a,a') (b,b') =
    assert (a' = empty_tt);
    assert (b' = empty_tt);
    Model.is_included_actual_generic inouts a b

  let widen wh (a,a') (b,b') =
    let tt = if is_included_tt b' a' then a' else empty_tt in
    let flag, values =
      Model.widen wh a b
    in
    flag,(values, tt)

  let paste_offsetmap map_to_copy dst_loc start size (a, a') =
    let result =
      Model.paste_offsetmap map_to_copy dst_loc start size a
    in
    if Model.is_reachable result then result, a' else bottom

  let copy_paste loc1 loc2 (a, a') =
    Model.copy_paste loc1 loc2 a, a'

  let drop_relations (a,_a') = a, empty_tt

  let comp_prefixes (a, _) (b, _) = Model.comp_prefixes a b
  let find_prefix (a, _) prefix = Model.find_prefix a prefix

  let copy_from_virtual
      sub_left_loc
      (target_offset : Ival.t) target_size (_main, map) =
    let losize = Int_Base.inject (Int.of_int(Bit_utils.sizeofpointer())) in
    let treat_one_exact_location l acc =
      assert (Location_Bits.cardinal_zero_or_one l);
      let loc = make_loc l losize in
      let cluster =
	Participation_Map.find map.participation_map loc
      in
      match cluster with
      | Bottom_cluster
      | No_cluster -> (* no cluster for loc -> use main memory *)
	  raise Use_Main_Memory;
      | Cluster c ->
	  let cluster_offset =
	    Relation_between.find ~with_alarms:CilE.warn_none_mode c.Cluster.rel loc
	  in
	  try
	    let cluster_offset = V.project_ival cluster_offset in
(*	    Format.printf "cluster_o:%a target_o:%a@\n"
	      Ival.pretty cluster_offset Ival.pretty target_offset; *)
	    let cluster_offset =
	      Ival.scale (Bit_utils.sizeofchar()) cluster_offset in
	    let actual_offset =
		Ival.sub target_offset cluster_offset
	    in
	    let real =
	      Location_Bits.location_shift actual_offset
		c.Cluster.virtual_to_real
	    in
	    if not (Locations.can_be_accessed
		       (Locations.make_loc real (Int_Base.inject target_size)))
	    then raise Lmap.Cannot_copy;
	    try
		let f offs acc =
		  let copy =
		    V_Offsetmap.copy_offsmap
		      c.Cluster.contents
		      offs (Int.pred (Int.add offs target_size))
		  in
		  match acc with
                  | None -> Some copy
                  | Some acc ->
		      Some(snd (V_Offsetmap.join copy acc))
		in
		Ival.fold f actual_offset acc
	    with Int_Base.Error_Top -> raise Use_Main_Memory;
	  with  V.Not_based_on_null ->
	    raise Use_Main_Memory
    in
    try
      Cilutil.out_some
	(Location_Bits.fold_enum
	    ~split_non_enumerable:(-1)
	   treat_one_exact_location
	   sub_left_loc.loc
	   None)
    with Location_Bits.Error_Top -> raise Use_Main_Memory

  let hash (a, _b) = Model.hash a (*+ 97*hash b*)

  let filter_base_tt f a =
    Cluster.Set.fold
      (fun cl acc -> try add_new_cluster (Cluster.filter_base f cl) acc
       with Cluster.No_more_cluster -> acc)
      a.all_clusters
      empty_tt

  let filter_base f (a,a') =
    Model.filter_base f a,
    filter_base_tt f a'

  let remove_base b (a,a') =
    assert (a' == empty_tt);
    Model.remove_base b a,
    empty_tt

  let clear_state_from_locals fundec (state,r) =
    let locals = List.map Base.create_varinfo fundec.Cil_types.slocals in
    let formals = List.map Base.create_varinfo fundec.Cil_types.sformals in
    let cleanup acc v = Cvalue_type.Model.remove_base v acc in
    let result = List.fold_left cleanup state locals in
    List.fold_left cleanup result formals,
    filter_base_tt (fun v -> not (Base.is_formal_or_local v fundec)) r
(*
    List.iter cleanup
      filter_base
      (fun v -> not (Base.is_formal_or_local v fundec))
      state
*)

  let uninitialize_locals blocks (state,r) =
    let locals =
      List.fold_left
	(fun acc block ->
	  List.fold_left
	    (fun acc vi -> (Locations.loc_of_varinfo vi) :: acc)
	    acc
	    block.Cil_types.blocals)
        []
	blocks
    in
   let state' =
      List.fold_left Cvalue_type.Model.add_binding_unspecified state locals
    in
    let r' = filter_base_tt (fun v -> not (List.exists
                                             (Base.is_block_local v) blocks)) r
    in
     (state', r')

  let compute_actual_final_from_generic (a,_a') (b,_b') loc instanciation =
    let a,b = Model.compute_actual_final_from_generic a b loc instanciation in
    (a,empty_tt),b

  let is_included_by_location_enum (a,_a') (b,_b') loc =
    Model.is_included_by_location_enum a b loc

  let find_mem
      (loc : Locations.location)
      (target_size : Int_Base.t)
      (target_offset : Ival.t)
      (_main_memory, map : model) =
    let losize = loc.size in
    let treat_one_exact_location l acc =
      assert (Location_Bits.cardinal_zero_or_one l);
      let loc = make_loc l losize in
      let cluster = Participation_Map.find map.participation_map loc
      in
      match cluster with
      | Bottom_cluster
      | No_cluster -> (* no cluster for loc -> use main memory *)
          raise Use_Main_Memory;
      | Cluster c ->
	  let cluster_offset =
            Relation_between.find ~with_alarms:CilE.warn_none_mode 
	      c.Cluster.rel
	      loc
          in
	  try
	    let cluster_offset =
              Ival.scale (Bit_utils.sizeofchar())
		(V.project_ival cluster_offset)
            in
	    let actual_offset =
	      Ival.sub target_offset cluster_offset
	    in
	    let real =
	      Location_Bits.location_shift actual_offset
		c.Cluster.virtual_to_real
	    in
  (*          Format.printf "cluster_offs:%a@\ntarget_offset:%a@\nreal:%a@\n"
              Ival.pretty cluster_offset
              Ival.pretty target_offset
	      Location_Bits.pretty real;*)
	    if not (Locations.can_be_accessed (Locations.make_loc real target_size))
	    then raise Use_Main_Memory;
	    try
	      let target_size = Int_Base.project target_size in
              let acc = V_Or_Uninitialized.initialized acc in
	      let r =
		V_Or_Uninitialized.get_v
		  (V_Or_Uninitialized.join
                      (V_Offsetmap.find_ival
			  ~conflate_bottom:true
                          ~validity:Base.All
                          ~with_alarms:CilE.warn_none_mode
                          (* anyway, there is no validity *)
		          actual_offset c.Cluster.contents target_size)
                      acc
                  )
              in
              (*Format.printf "find_mem: %a@\n" V.pretty r;*)
              r
	    with Int_Base.Error_Top
	    (* suppressed this exception from find_ival.
	       Wondering if it could come from somewhere else
	       | Not_found (* from LOffset.find_ival *) *)
	          -> raise Use_Main_Memory
	  with V.Not_based_on_null -> raise Use_Main_Memory
    in
    try
      let r = Location_Bits.fold_enum
	    ~split_non_enumerable:(-1)
        treat_one_exact_location
        loc.loc
        V.bottom
      in
      (*Format.printf "find_mem(result): %a@\n" V.pretty r;*)
      r
    with
      | Ival.Error_Top -> assert false
      | Location_Bits.Error_Top -> V.top


  let add_mem
      (loc : Locations.location)
      (target_size : Int_Base.t)
      (target_offset : Ival.t)
      (main, map as _orig: Model.t * tt)
      (value : V.t) =
    let virtual_to_real = virtual_to_real main loc in
    assert (Location_Bits.cardinal_zero_or_one loc.loc);
    let real_loc =
      (Location_Bits.location_shift target_offset virtual_to_real)
    in
    let exact_real_loc =
      Location_Bits.cardinal_zero_or_one real_loc
	(* (Locations.valid_cardinal_zero_or_one real_loc) *)
    in
    (* main,map is the state in which the assignment has been
       naively treated. Now we try to improve on that: *)
    let target_size =
      try Int_Base.project target_size
      with Int_Base.Error_Top -> assert false
    in
    let loc_size =
      try Int_Base.project loc.size
      with Int_Base.Error_Top -> assert false
    in
    let result =
      let clusterlist =
        Participation_Map.concerned_bindings
	  map.participation_map
	  loc
      in
      (*Format.printf "ONC+E_CLuster(before): loc=%a@\n" Location_Bits.pretty loc.loc;*)
      let protected_clusters,improved_relations =
        match clusterlist with
        | [] -> (* no cluster: create a new one *)
	    let exact = Ival.cardinal_zero_or_one target_offset in
            if exact_real_loc || not exact then
	      (* exact_virtual_to_real: virtual_to_real is so precise that it is not
		 needed to create a cluster.
		 not exact: we don't have enough information to create a cluster *)
              [],map
            else
	      let contents =
                V_Offsetmap.update_ival
		  ~with_alarms:CilE.warn_none_mode
                  ~validity:Base.All
	          ~exact
	          ~offsets:target_offset
	          ~size:target_size
		  V_Offsetmap.empty
	          (V_Or_Uninitialized.initialized value)
	      in
	      if V_Offsetmap.is_empty contents
	      then [], map
	      else
	        let rel = Relation_between.add_whole
	          loc
                  V.singleton_zero
		  Relation_between.empty
	        in
	        let new_cluster =
	          Cluster.make
	            ~contents
	            ~size:loc_size
	            ~rel
	            ~virtual_to_real
	        in
	        [new_cluster],add_new_cluster new_cluster map

        | [Cluster c]
	    when same_clusterinfo_cluster
	      (Participation_Map.find map.participation_map loc) c ->
	    (* one "just right" cluster: modify it *)
	    if exact_real_loc then [], map else
	      let exact = Ival.cardinal_zero_or_one target_offset in
	      let intrinsic_offset =
		Relation_between.find ~with_alarms:CilE.warn_none_mode c.Cluster.rel loc
              in
	      begin try
	          let intrinsic_offset =
                    Ival.scale (Bit_utils.sizeofchar())
		      (V.project_ival intrinsic_offset)
	          in
	          assert (Ival.cardinal_zero_or_one intrinsic_offset);
	          let offset = Ival.sub target_offset intrinsic_offset in
	          (*Format.printf "offset AAAA:%a@\n" Ival.pretty offset;*)
	          let contents =
	            V_Offsetmap.update_ival
		      ~with_alarms:CilE.warn_none_mode
                      ~validity:Base.All
		      ~exact
		      ~offsets:offset
		      ~size:target_size
		      c.Cluster.contents
		      (V_Or_Uninitialized.initialized value)
	          in
	          let new_cluster =
	            Cluster.make
		      ~contents
		      ~size:loc_size
		      ~rel:c.Cluster.rel
		      ~virtual_to_real:c.Cluster.virtual_to_real
	          in
		  [new_cluster],replace_cluster ~new_cluster ~old_cluster:c map
		with V.Not_based_on_null -> [],map
	      end
        | _ ->
	    (* was this state cleaned up properly before calling add_mem?
	       At least it doesn't look so... *)
	    Format.printf "state not cleaned up?@\nloc=%a@\nstate=%a@\n"
	      Locations.pretty loc
	      pretty_tt map;
            assert false
      in
      let result_state = main,improved_relations
      in
      protected_clusters,result_state
    in
    (*Format.printf "add_mem loc:%a@\nadd_mem target_size:%a
      add_mem target_offset:%a@\nadd_mem initial:%a@\nadd_mem result:%a@\n"
      Location_Bits.pretty loc.loc
      Int.pretty target_size
      Ival.pretty target_offset
      pretty orig pretty (snd result);*)
    result

  exception No_information
  let propagate_change_from_real_to_virt ~protected_clusters loc m value =
    let main_mem,m = m in
    let result =
      match loc.loc with
      | Location_Bits.Top _ -> main_mem,empty_tt
      | Location_Bits.Map locm ->
	  (* [pc 06/2006] the next two bindings should be deforested *)
          let invalidated_clusters =
            Participation_Map.concerned_bindings m.participation_map loc
          in
	  let invalidated_clusters =
	    List.fold_right
	      (function Cluster c -> Cluster.Set.add c | _ -> assert false)
	      invalidated_clusters
	      Cluster.Set.empty
	  in
          let m =
	    Cluster.Set.fold
              (fun cluster acc ->
                 if List.exists (Cluster.equal cluster) protected_clusters
		 then acc
                 else
                   let new_rel =
		     Relation_between.remove_whole
		       loc
		       cluster.Cluster.rel
		   in
		   let can_be_removed = not (Cluster.has_information
		                               { cluster with Cluster.rel = new_rel })
		   in
		   if can_be_removed
		   then remove_cluster ~old_cluster:cluster acc
		   else
                     let new_cluster =
                       Cluster.make
		         ~size:cluster.Cluster.size
		         ~contents:cluster.Cluster.contents
		         ~virtual_to_real:cluster.Cluster.virtual_to_real
		         ~rel:new_rel
                     in replace_cluster ~new_cluster ~old_cluster:cluster acc)
              invalidated_clusters
	      m
          in
          let size =
            try
              Int_Base.project loc.size
            with Int_Base.Error_Top -> assert false (* TODO *)
          in
          let treat_cluster cluster acc =
	    (*Format.printf "treat_cluster(start): %a@\n"
	      V.pretty value;*)
	    try
              let new_cluster =
                if List.exists (Cluster.equal cluster)
		  protected_clusters
	        then
                  cluster
                else
                  let treat_base base offsets acc =
	            try
	              let offsets_in_loc =
		        Location_Bits.M.find base locm
		      in
	              let new_offsets =
		        Ival.sub offsets_in_loc offsets
		      in
(*		      Format.printf "treat_cluster: %a %a@."
			Base.pretty base
			V.pretty value; *)
                      V_Offsetmap.update_ival
		        ~with_alarms:CilE.warn_none_mode
                        ~validity:Base.All
                        ~exact:false
                        ~size
                        ~offsets:new_offsets
                        acc
                        (V_Or_Uninitialized.initialized value)
	            with Not_found (*from Location_Bits.M.find *) ->
		      (* Format.printf "treat_cluster(not_found): %a@\n"
		         V.pretty value;*)
                      acc
	          in
	          let contents =
	            try Location_Bits.fold_i
	              treat_base
	              cluster.Cluster.virtual_to_real
	              cluster.Cluster.contents
                    with Location_Bits.Error_Top ->
		      V_Offsetmap.empty
	          in
                  if Cluster.has_information {cluster with Cluster.contents = contents } then
	            Cluster.make
	              ~size:cluster.Cluster.size
	              ~contents
	              ~rel:cluster.Cluster.rel
	              ~virtual_to_real:cluster.Cluster.virtual_to_real
                  else raise No_information
	      in
	      { all_clusters = Cluster.Set.add new_cluster acc.all_clusters;
	        participation_map =
                  index_cluster_into_participation_map new_cluster acc.participation_map }
            with  No_information -> acc
          in
	  main_mem,
	  Cluster.Set.fold
	    treat_cluster
	    m.all_clusters
	    empty_tt
    in
(*    Format.printf
      "propagate_change_from_real_to_virt:loc:%a@\nval:%a@\norig state:%a@\nresult: %a@\n"
      Location_Bits.pretty loc.loc
      V.pretty value
      pretty (main_mem,m)
      pretty result;
*)
    result

  let reduce_binding (main, map as _state) left value =
    assert (Locations.valid_cardinal_zero_or_one left);
    let left = Locations.valid_part left in
    match left.size with
    | Int_Base.Bottom -> assert false
    | Int_Base.Value _ when
	  (Location_Bits.cardinal_zero_or_one left.loc) ->
	(* could do better : if left.loc is made of several locations,
	   all of which are in the same cluster *)
	begin match Participation_Map.find map.participation_map left with
	| Cluster c ->
(*	    Format.printf "cluster: %a@."
	      Cluster.pretty c ; *)
	    let left_offset =
	      Relation_between.find ~with_alarms:CilE.warn_none_mode c.Cluster.rel left
	    in
	    let value = V.add left_offset value in
	    let update_loc loc offs acc =
	      let reduced_value =
		V.add_untyped (Int_Base.minus_one) value offs
	      in
	      Model.reduce_binding ~with_alarms:CilE.warn_none_mode
		acc loc reduced_value
	    in
	    let improved_main =
	      Relation_between.fold_single_bindings
		~size:c.Cluster.size
		update_loc
		c.Cluster.rel
		main
	    in
	    improved_main, map
	| _ ->
	    Model.reduce_binding ~with_alarms:CilE.warn_none_mode
	      main left value,
	    map
	end
    | _ ->
	Model.reduce_binding ~with_alarms:CilE.warn_none_mode
	  main left value,
	map

  (* [offset] must be in bytes *)
  let add_equality ?offset (main,map as state) left right =
    (*Format.printf "add_equality left:%a right:%a@\nstate:%a@\n"
     Locations.pretty left Locations.pretty right
     pretty state;*)
    match left.size with
    | Int_Base.Bottom -> assert false
    | Int_Base.Value size when
	(Location_Bits.is_relationable left.loc)
	&& Int_Base.equal left.size right.size
	&& not (Zone.intersects
		  (valid_enumerate_bits left)
		  (valid_enumerate_bits right)) ->
	begin match Participation_Map.find map.participation_map right with
	| Cluster c ->
	    let offset_right =
	      Relation_between.find ~with_alarms:CilE.warn_none_mode c.Cluster.rel right
	    in
	    let offset =
	      match offset with
		None -> offset_right
	      | Some o ->
		    V.location_shift o offset_right
	    in
	    assert (if V.cardinal_zero_or_one offset
		    then true
		    else (Format.printf "State at error point:%a@\n"
		      pretty state; false));
            let new_cluster = Cluster.make
              ~rel:(Relation_between.add_whole
                      left
                      offset
		      c.Cluster.rel)
              ~contents:c.Cluster.contents
              ~size
              ~virtual_to_real:c.Cluster.virtual_to_real
            in
	    main, (replace_cluster ~new_cluster ~old_cluster:c map)
	| No_cluster ->
	    let clusterlist =
              Participation_Map.concerned_bindings
		map.participation_map
		right
	    in

	    let virtual_to_real = virtual_to_real main right in
	    if (Location_Bits.cardinal_zero_or_one virtual_to_real) ||
	      clusterlist <> []
	    then state
	    else begin
		let offset =
		  match offset with
		    None -> V.singleton_zero
		  | Some o -> V.inject_ival o
		in
		assert (V.cardinal_zero_or_one offset);
		let rel =
		  (Relation_between.add_whole
		      left
		      offset
		      (Location_Bits.fold_enum
			  ~split_non_enumerable:(-1)
			  (fun loc acc ->
			    let locsize = Locations.make_loc loc right.size
			    in
			    Relation_between.add_whole
			      locsize
			      V.singleton_zero
			      acc)
			  right.loc
			  Relation_between.empty))
		in
		let new_cluster =
		  (*	      ( try *)
		  Cluster.make ~size ~virtual_to_real ~rel
		    ~contents: V_Offsetmap.empty
		    (*
				with _e ->
				Format.printf "debugging add_equality rel=%a left=%a right=%a@."
				Relation_between.pretty rel
				Locations.pretty left Locations.pretty right;
				raise _e ) *)
		in
		(*    Format.printf "add_equality new_cluster:%a@\n"
		      Cluster.pretty new_cluster; *)
		main,(add_new_cluster new_cluster map)
	      end
	| Bottom_cluster -> assert false
	end
    | Int_Base.Top | Int_Base.Value _ -> state


  (*  TODO : detect unsatisfiability and reduce to bottom *)
  let reduce_equality (main,map as state) left right =
    (*Format.printf "reduce_equality left:%a right:%a@\n"
      Locations.pretty left Locations.pretty right;*)
    match left.size with
    | Int_Base.Bottom -> assert false
    | Int_Base.Value size when
	(Location_Bits.cardinal_zero_or_one left.loc)
	&& Int_Base.equal left.size right.size
	&& not (Zone.intersects
		  (valid_enumerate_bits left)
		  (valid_enumerate_bits right)) ->
	(match Participation_Map.find map.participation_map right
	with
	| Cluster c ->
	    if
	      (Participation_Map.concerned_bindings map.participation_map left)
	      = []
	    then
	      let rel =
		Relation_between.add_whole
                  left
                  (Relation_between.find ~with_alarms:CilE.warn_none_mode c.Cluster.rel right)
		  c.Cluster.rel
	      in
              let new_cluster =
		Cluster.make
		  ~rel
		  ~contents:c.Cluster.contents
		  ~size
		  ~virtual_to_real:c.Cluster.virtual_to_real
              in
	      main, (replace_cluster ~new_cluster ~old_cluster:c map)
	    else
	      ( match Participation_Map.find map.participation_map left with
	      | Cluster cleft ->
		  if cleft.Cluster.id <> c.Cluster.id
		  then try
		    let delta =
		      V.add_untyped
			Int_Base.minus_one
			(Relation_between.find ~with_alarms:CilE.warn_none_mode
			    c.Cluster.rel right)
			(Relation_between.find ~with_alarms:CilE.warn_none_mode
			    cleft.Cluster.rel left)
		    in
		    assert (V.cardinal_zero_or_one delta);
		    let rel =
		      Relation_between.fold_single_bindings
			~size
			(fun loc v acc ->
			  let new_v =
			    V.add_untyped Int_Base.one
			      v
			      delta
			  in
			  Relation_between.add_whole loc new_v acc)
			cleft.Cluster.rel
			c.Cluster.rel
(* NB: this wouldn't have to be the _single_binding kind of fold but
   add_whole currently doesn't support non-exact locations *)
		    in
(*		    Format.printf "rel:%a@."
		      Relation_between.pretty rel;*)
		    let delta = Ival.project_int (V.project_ival delta) in
		    let offset = Int.neg (Int.mul (Bit_utils.sizeofchar()) delta)
		    in
		    let shifted_left_content =
		      V_Offsetmap.shift offset
			cleft.Cluster.contents (* FIXME *)
		    in
(*		    Format.printf "left_content:%a shifted_left_content:%a right=%a@."
		      V_Offsetmap.pretty cleft.Cluster.contents
		      V_Offsetmap.pretty  shifted_left_content
		      V_Offsetmap.pretty  c.Cluster.contents;*)
		    let contents =
		      V_Offsetmap.over_intersection
			c.Cluster.contents
			shifted_left_content
		    in
		    let new_cluster =
		      Cluster.make
			~rel
			~contents
			~size
			~virtual_to_real:c.Cluster.virtual_to_real
			(* FIXME : use over-intersection of virtual_to_reals *)
		    in
		    let map = remove_cluster ~old_cluster:c map in
		    main, (replace_cluster ~new_cluster ~old_cluster:cleft map)
		    with V.Not_based_on_null ->
		      (* from project_ival *) state
		  else
		    state
	      | _ -> state)
        | Bottom_cluster -> assert false
        | No_cluster ->
	    match Participation_Map.find map.participation_map left
	    with
	    | Cluster c when
		[] = (Participation_Map.concerned_bindings
			map.participation_map
			right)  ->
		let l =
		  Relation_between.find ~with_alarms:CilE.warn_none_mode c.Cluster.rel left
		in
		let rel =
		  Relation_between.add_whole
		    right
		    l
		    c.Cluster.rel
		in
(*		Format.printf "left:%a right:%a l:%a rel:%a@."
		  Locations.pretty left
		  Locations.pretty right
		  Location_Bytes.pretty l
		  Relation_between.pretty rel; *)
		let new_cluster =
		  Cluster.make
		    ~rel
		    ~contents:c.Cluster.contents
		    ~size
		    ~virtual_to_real:c.Cluster.virtual_to_real
		in
		main, (replace_cluster ~new_cluster ~old_cluster:c map)
	    | Bottom_cluster -> assert false
	    | Cluster _ | No_cluster -> state)
    | _ -> state

  (* result is in bytes *)
  let compute_diff (_,map) left right =
(*    Format.printf "compute_diff called@\n";*)
    let result =
      match Participation_Map.find map.participation_map right, right.size with
      | Cluster cr, Int_Base.Value s when Int.equal s cr.Cluster.size ->
          begin match Participation_Map.find map.participation_map left with
          | Cluster cl when same_clusters cl cr ->
              V.add_untyped (Int_Base.minus_one)
                (Relation_between.find
                   ~with_alarms:CilE.warn_none_mode
                   cl.Cluster.rel
                   right)
                (Relation_between.find
                   ~with_alarms:CilE.warn_none_mode
                   cl.Cluster.rel
                   left)
          | _ -> raise Use_Main_Memory
          end
      | _ -> raise Use_Main_Memory
    in
  (*  Format.printf "compute_diff returns %a@\n" V.pretty result;*)
    result

  (* [offset] is in bytes *)
  let shift_location ((main,map) as initial) loc offset right =
    let offset = Ival.neg offset in
    match Participation_Map.find map.participation_map loc, loc.size with
    | Cluster cr, Int_Base.Value s when
        Int.equal s cr.Cluster.size
        && Location_Bits.cardinal_zero_or_one loc.loc
	&& Ival.cardinal_zero_or_one offset
        && (try
              let base,_ = Location_Bits.find_lonely_key loc.loc in
              Location_Bits.fold_bases
                (fun b () -> if Base.compare base b = 0 then raise Exit)
                cr.Cluster.virtual_to_real ();
              true
            with | Exit | Location_Bits.Error_Top -> false
            | Not_found -> assert false)->
        (*Format.printf "cluster to shift:%a@.offset: %a@." Cluster.pretty cr
          Ival.pretty offset ;*)
          let protected_cluster =
            Cluster.make
              ~size:cr.Cluster.size
              ~contents:cr.Cluster.contents
              ~rel:(Relation_between.add_binding
                      ~with_alarms:CilE.warn_none_mode
                      ~exact:true
                      cr.Cluster.rel loc
                      (Location_Bytes.location_shift
                         offset
                         (Relation_between.find ~with_alarms:CilE.warn_none_mode cr.Cluster.rel loc)))
              ~virtual_to_real:cr.Cluster.virtual_to_real
          in
          (* Format.printf "protected_cluster:%a@\n" Cluster.pretty protected_cluster;*)
          let initial = main,replace_cluster
            ~new_cluster:protected_cluster
            ~old_cluster:cr
            map
          in
          (*Format.printf "intial_state:%a@\n" pretty initial;*)
          let result = propagate_change_from_real_to_virt
            ~protected_clusters:[protected_cluster]
            loc
            initial
            right
          in
          (*Format.printf "cleaned_state:%a@\n" pretty result;*)
          result
    | _ -> propagate_change_from_real_to_virt ~protected_clusters:[] loc initial right

  include Datatype.Make
      (struct
	 type t = model
	 let name = "Relations_type.Model"
	 let structural_descr =
	   Structural_descr.t_tuple
	     [| Cvalue_type.Model.packed_descr;
		Structural_descr.pack
		  (Structural_descr.t_record
		     [| Participation_Map.packed_descr;
			Cluster.Set.packed_descr |]) |]
	 let reprs =
	   List.map
	     (fun m ->
	       m,
	       { participation_map = Participation_Map.empty;
		 all_clusters = Cluster.Set.empty })
	     Cvalue_type.Model.reprs
	 let hash = hash
	 let equal = equal
	 let compare = Datatype.undefined
	 let pretty = pretty
	 let rehash = Datatype.identity
	 let copy = Datatype.undefined
	 let internal_pretty_code = Datatype.pp_fail
	 let varname = Datatype.undefined
	 let mem_project = Datatype.never_any_project
       end)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
