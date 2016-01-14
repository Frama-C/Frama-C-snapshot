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

open Locations

exception Bitwise_cannot_copy

module type Location_map_bitwise = sig

  type v

  type map

  type lmap = Top | Map of map | Bottom

  include Datatype.S with type t = lmap
  include Lattice_type.Bounded_Join_Semi_Lattice with type t := t
  include Lattice_type.With_Top with type t := t

  module LOffset :
    module type of Offsetmap_bitwise_sig
      with type v = v
      and type intervals = Int_Intervals.t

  val is_empty : t -> bool
  val is_bottom : t -> bool
  val empty : t
  val empty_map: map

  val pretty_generic_printer:
    ?pretty_v: v Pretty_utils.formatter ->
    ?skip_v: (v -> bool) ->
    sep:string ->
    unit ->
    t Pretty_utils.formatter

  val add_binding : reducing:bool -> exact:bool -> t -> Zone.t -> v -> t
  val add_binding_loc: reducing:bool -> exact:bool -> t -> location -> v -> t
  val add_base: Base.t -> LOffset.t -> t -> t
  val remove_base: Base.t -> t -> t

  val find : t -> Zone.t -> v
  val filter_base : (Base.t -> bool) -> t -> t

  val map: (v -> v) -> t -> t

  val fold : (Zone.t -> v -> 'a -> 'a) -> map -> 'a -> 'a
  val fold_base : (Base.t -> LOffset.t -> 'a -> 'a) -> map -> 'a -> 'a
  val fold_fuse_same : (Zone.t -> v -> 'a -> 'a) -> map -> 'a -> 'a

  val fold_join_zone:
    both:(Int_Intervals.t -> LOffset.t -> 'a) ->
    conv:(Base.t -> 'a -> 'b) ->
    empty_map:(Locations.Zone.t -> 'b) ->
    join:('b -> 'b -> 'b) ->
    empty:'b ->
    Locations.Zone.t -> map -> 'b

  val map2:
    cache:Hptmap_sig.cache_type -> symmetric:bool -> idempotent:bool
    -> empty_neutral:bool -> (LOffset.t -> LOffset.t -> LOffset.map2_decide) ->
    (v -> v -> v) -> map -> map -> map

  val shape: map -> LOffset.t Hptmap.Shape(Base.Base).t

  val imprecise_write_msg: string ref

  val clear_caches: unit -> unit
end

module type With_default = sig
  include Lattice_type.Bounded_Join_Semi_Lattice
  include Lattice_type.With_Top with type t := t
  include Lattice_type.With_Narrow with type t := t
  val default: t
end

module Make_bitwise (V: With_default): Location_map_bitwise with type v = V.t =
struct

  module LOffset = struct
    include Offsetmap.Make_bitwise(V)
    let copy = Datatype.undefined
  end

  exception Invalid_base

  (* validity must not be invalid; otherwise, Invalid_base is raised. *)
  let default_offsetmap_aux b validity =
    let default () =
      match Base.valid_range validity with
      | None -> raise Invalid_base
      | Some (ib, ie) ->
        assert (Integer.(equal ib zero));
        LOffset.create ~size:(Integer.succ ie) V.default
    in
    if Base.equal Base.null b then
      match validity with
      | Base.Invalid -> raise Invalid_base
      | Base.Known (ib, ie) | Base.Unknown (ib, _, ie) ->
        if Integer.is_zero ib then
          default ()
        else begin
          (* NULL is special, because the validity may not start at 0. We must
             bind the beginning of the interval to bottom. *)
          assert (Integer.gt ib Integer.zero);
          let to_bottom = LOffset.create ~size:(Integer.succ ie) V.bottom in
          let range = Int_Intervals.inject_bounds ib ie in
          match LOffset.add_binding_intervals
           ~validity ~exact:true range V.default to_bottom
          with
          | `Bottom -> assert false
          | `Map m -> m
        end
    else
      default ()

  let default_offsetmap b = default_offsetmap_aux b (Base.validity b)

  module LBase = struct
    include Hptmap.Make(Base.Base)(LOffset)(Hptmap.Comp_unused)(struct let v = [[]] end)(struct let l = [ Ast.self ] end)
    let () = Ast.add_monotonic_state self

    (* We override [add] so that the map is canonical: no key should be
       bound to its default value. *)
    let add b offsm m =
      let is_default =
        if Base.is_null b then
          (* If we are binding something to NULL, NULL should not be invalid *)
          let default = default_offsetmap Base.null in
          LOffset.equal default offsm
        else
          let is_default v = V.equal v V.default in
          LOffset.is_single_interval ~f:is_default offsm
      in
      if is_default then
        remove b m
      else
        add b offsm m

  end

  let clear_caches () =
    LBase.clear_caches ();
    LOffset.clear_caches ();
  ;;

  type map = LBase.t

  let imprecise_write_msg = LOffset.imprecise_write_msg

  let find_or_default b m =
    try LBase.find b m
    with Not_found -> default_offsetmap b

  type lmap = Top | Map of LBase.t | Bottom
  type v = V.t
  let empty_map = LBase.empty
  let empty = Map LBase.empty
  let bottom = Bottom

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

  let pretty_generic_printer ?pretty_v ?skip_v ~sep () fmt m =
    match m with
    | Top -> Format.fprintf fmt "@[%sTOP@]" sep
    | Bottom -> Format.fprintf fmt "@[%sUNREACHABLE@]" sep
    | Map m ->
      let pp_one fmt (base, offs) =
        Format.fprintf fmt "@[%a@[<v>%a@]@]"
          Base.pretty base
          (LOffset.pretty_generic
             ?typ:(Base.typeof base) ?pretty_v ?skip_v ~sep ()) offs
      in
      Pretty_utils.pp_iter ~pre:"@[<v>" ~sep:"@ " ~suf:"@]"
        (Extlib.iter_uncurry2 LBase.iter) pp_one fmt m

  let pretty = pretty_generic_printer ~sep:"FROM" ()

  include Datatype.Make
      (struct
        type t = lmap
        let reprs = Top :: List.map (fun b -> Map b) LBase.reprs
        let structural_descr =
	  Structural_descr.t_sum [| [| LBase.packed_descr |] |]
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

 let fold_base f m acc = LBase.fold f m acc

  let fold_fuse_same f m acc =
    let f' b offs acc =
      LOffset.fold_fuse_same
        (fun itvs v acc -> f (Zone.inject b itvs) v acc)
        offs acc
    in
    fold_base f' m acc

  let for_writing_validity ~reducing b =
    if not reducing && Base.is_read_only b
    then Base.Invalid
    else Base.validity b

 let add_binding ~reducing ~exact m (loc:Zone.t) v  =
   let aux_base_offset base offs m =
     let validity = for_writing_validity ~reducing base in
     try
       let offsm = find_or_default base m in
       match LOffset.add_binding_intervals ~validity ~exact offs v offsm with
       | `Bottom -> m
       | `Map new_offsetmap -> LBase.add base new_offsetmap m
     with Invalid_base -> m
   in
   match loc, m with
   | Zone.Top (Base.SetLattice.Top, _),_|_,Top -> Top
   | _, Bottom -> Bottom
   | _, Map m -> Map (Zone.fold_topset_ok aux_base_offset loc m)

 let add_binding_loc ~reducing ~exact m loc v  =
   let aux_base_offset base offs m =
     let validity = for_writing_validity ~reducing base in
     try
       let offsm = find_or_default base m in
       let new_offsetmap =
         LOffset.add_binding_ival ~validity ~exact offs ~size:loc.size v offsm
       in
       match new_offsetmap with
       | `Bottom -> m
       | `Map new_offsetmap -> LBase.add base new_offsetmap m
     with Invalid_base -> m
   in
   match loc.loc, m with
   | Location_Bits.Top (Base.SetLattice.Top, _),_|_,Top -> Top
   | _, Bottom -> Bottom
   | _, Map m ->
     Map (Location_Bits.fold_topset_ok aux_base_offset loc.loc m)

 let add_base b offsm = function
   | Bottom | Top as m -> m
   | Map m -> Map (LBase.add b offsm m)

 let remove_base b = function
   | Bottom | Top as m -> m
   | Map m -> Map (LBase.remove b m)

 let join_on_map =
   (* [join t Empty] is [t] if unbound bases are bound to [bottom] by default*)
   if V.(equal default bottom)
   then
     LBase.join
       ~cache:(Hptmap_sig.PersistentCache "lmap_bitwise.join")
       ~decide:(fun _ v1 v2 -> LOffset.join v1 v2)
       ~symmetric:true ~idempotent:true
   else
     let decide =
       let get b = function Some v -> v | None -> default_offsetmap b in
       fun b v1 v2 -> LOffset.join (get b v1) (get b v2)
     in
     LBase.generic_join
       ~cache:(Hptmap_sig.PersistentCache "lmap_bitwise.join")
       ~symmetric:true ~idempotent:true ~decide


 let join m1 m2 =
   let result = match m1, m2 with
   | Top, _ | _, Top -> Top
   | Bottom, m | m, Bottom -> m
   | Map m1, Map m2 -> Map (join_on_map m1 m2)
   in
   (*Format.printf "JoinBitWise: m1=%a@\nm2=%a@\nRESULT=%a@\n"
     pretty m1
     pretty m2
     pretty result;*)
   result

 let map f = function
   | Top -> Top
   | Bottom -> Bottom
   | Map m -> Map (LBase.map (fun m -> LOffset.map f m) m)

 let map2 ~cache ~symmetric ~idempotent ~empty_neutral fv f =
   let aux = LOffset.map2 cache fv f in
   let decide b om1 om2 = match om1, om2 with
     | None, None -> assert false (* decide is never called in this case *)
     | Some m1, None -> aux m1 (default_offsetmap b)
     | None, Some m2 -> aux (default_offsetmap b) m2
     | Some m1, Some m2 -> aux m1 m2
   in
   if empty_neutral
   then LBase.join ~symmetric ~idempotent ~cache ~decide:(fun _ m1 m2 -> aux m1 m2)
   else LBase.generic_join ~symmetric ~idempotent ~cache ~decide

 let is_included_map =
   let name = Pretty_utils.sfprintf "Lmap_bitwise(%s).is_included" V.name in
   let decide_fst b offs1 = LOffset.is_included offs1 (default_offsetmap b) in
   let decide_snd b offs2 = LOffset.is_included (default_offsetmap b) offs2 in
   let decide_both _ offs1 offs2 = LOffset.is_included offs1 offs2 in
   LBase.binary_predicate (Hptmap_sig.PersistentCache name) LBase.UniversalPredicate
     ~decide_fast:LBase.decide_fast_inclusion
     ~decide_fst ~decide_snd ~decide_both

 let is_included m1 m2 =
   match m1, m2 with
    | _, Top -> true
    | Top ,_ -> false
    | Bottom, _ -> true
    | _, Bottom -> false
    | Map m1, Map m2 -> is_included_map m1 m2

 let join_and_is_included m1 m2 = match (m1,m2) with
   | _, Top -> (Top, true)
   | Top, _ -> (Top, false)
   | Bottom, m2 -> (m2, true)
   | m1, Bottom -> (m1, false)
   | Map mm1, Map mm2 ->
     let m = join_on_map mm1 mm2 in
     if LBase.equal m mm2 then m2, true else Map m, false

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

 let find m loc =
   match loc, m with
     | Zone.Top _, _ | _, Top -> V.top
     | _, Bottom -> V.bottom
     | Zone.Map _, Map m ->
         let treat_offset base itvs acc =
           let validity = Base.validity base in
           if validity = Base.Invalid then acc
           else
             let offsetmap = find_or_default base m in
             let v = LOffset.find_iset ~validity itvs offsetmap in
             V.join acc v
         in
         Zone.fold_i treat_offset loc V.bottom

 let fold_join_zone ~both ~conv ~empty_map ~join ~empty =
   let cache = Hptmap_sig.PersistentCache "Lmap_bitwise.fold_on_zone" in
   let empty_left _ = empty (* zone over which to fold is empty *) in
   let empty_right z = empty_map z in
   let both b itvs map_b = conv b (both itvs map_b) in
   let fmap =
     Zone.fold2_join_heterogeneous
       ~cache ~empty_left ~empty_right ~both ~join ~empty
   in
   fun z m -> fmap z (LBase.shape m)


 let shape = LBase.shape

end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
