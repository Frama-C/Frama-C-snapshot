(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

(** Maps from bases to memory maps. The memory maps are those of the
    [Offsetmap] module.
    @plugin development guide *)

open Abstract_interp
open Locations

(** Declaration for a memory map. *)
module type Location_map =
sig
  type y (** type of the values associated to the locations *)
  type loffset (** type of the map associated to a base *)
  type widen_hint_y (** widening hints associated to values *)

    module LBase :
    sig
      type t
      val iter : (Base.base -> loffset -> unit) -> t -> unit
    end

    type tt = private Bottom | Top | Map of LBase.t

    include Datatype.S_with_collections with type t = tt

    type widen_hint = bool * Base.Set.t * (Base.t -> widen_hint_y)

    val inject : Base.t -> loffset -> t

    val add_offsetmap :  Base.t -> loffset -> t -> t

    val pretty_without_null : Format.formatter -> t -> unit
    val pretty_filter:
      Format.formatter -> t -> Locations.Zone.t -> (Base.t -> bool) -> unit
    val add_binding:
      with_alarms:CilE.warn_mode -> exact:bool -> t -> location -> y -> t

    val find :
      conflate_bottom:bool -> with_alarms:CilE.warn_mode -> t -> location -> y

    val join : t -> t -> location list * t
    val is_included : t -> t -> bool

    val top: t
    val is_top: t -> bool

    (** Empty map. Casual users do not need this.*)
    val empty_map : t
    val is_empty_map : t -> bool

    (** Every location is associated to [VALUE.bottom] in [bottom].  This state
        can be reached only in dead code. *)
    val bottom : t
    val is_reachable : t -> bool

    val widen : widen_hint-> t -> t -> (bool * t)

    val filter_base : (Base.t -> bool) -> t -> t

    (** @raise Not_found if the varid is not present in the map. *)
    val find_base : Base.t -> t -> loffset

    val find_base_or_default : Base.t -> t -> loffset

    (** Removes the base if it is present. Does nothing otherwise. *)
    val remove_base : Base.t -> t -> t

    val reduce_previous_binding :
      with_alarms:CilE.warn_mode -> t -> Locations.location -> y -> t
    val reduce_binding :
      with_alarms:CilE.warn_mode -> t -> Locations.location -> y -> t

(*
    (** [copy_paste src dst state] returns a modified version of [state] in
        which everything present in [src] has been copied onto [dst]. [src] and
        [dst] must have the same (non-top) size. The write operation is exact
        iff [dst] is exact. This function never emits alarms, as it is
        usually not the same syntactic context for *)
    val copy_paste : location -> location -> t -> t
*)

    (** [paste_offsetmap ~from:offmap ~dst_loc ~start ~size ~exact m]
        copies [size] bits starting at [start] in [offmap], and pastes
        them at [dst_loc] in [m]. The copy is exact if and only if
        [dst_loc] is exact, and [exact is true]
    *)
    val paste_offsetmap :
      with_alarms:CilE.warn_mode ->
      from:loffset ->
      dst_loc:Location_Bits.t ->
      start:Int.t ->
      size:Int.t ->
      exact:bool ->
      t -> t

    (** [copy_offsetmap alarms loc m] returns the superposition of the
        bits pointed to by [loc] within [m]. [loc.size] must not be top.
        Return [None] if all pointed adresses are invalid in [m]. *)
    val copy_offsetmap :
      with_alarms:CilE.warn_mode -> Locations.location -> t -> loffset option

    val is_included_by_location_enum :  t -> t -> Locations.Zone.t -> bool

    (** @raise Invalid_argument if one location is not aligned or of size
        different of [size].
        @raise Error_Bottom if [m] is bottom. *)
    val fold : size:Int.t -> (location -> y -> 'a -> 'a) -> t -> 'a -> 'a

    (** @raise Invalid_argument "Lmap.fold" if one location is not aligned
        or of size different of [size].
        @raise Error_Bottom if [m] is bottom.  *)
    val fold_single_bindings :
      size:Int.t -> (location -> y -> 'a -> 'a) -> t -> 'a -> 'a

    (** [fold_base f m] calls [f] on all bases bound to non top
        offsetmaps in the non bottom map [m].
        @raise Error_Bottom if [m] is bottom. *)
    val fold_base : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a

    (** [fold_base_offsetmap f m] calls [f] on all bases bound to non
        top offsetmaps in the non bottom map [m].
        @raise Error_Bottom if [m] is bottom.*)
    val fold_base_offsetmap : (Base.t -> loffset -> 'a -> 'a) -> t -> 'a -> 'a

    val find_offsetmap_for_location : Location_Bits.t -> t -> loffset

    val comp_prefixes: t -> t -> unit
    type subtree
    val find_prefix : t -> Hptmap.prefix -> subtree option
    val hash_subtree : subtree -> int
    val equal_subtree : subtree -> subtree -> bool

    (** [reciprocal_image m b] is the set of bits in the map [m] that may lead
        to Top([b]) and  the location in [m] where one may read an address
        [b]+_ *)
    val reciprocal_image : Base.t -> t -> Zone.t*Location_Bits.t

    val create_initial :
      base:Base.t ->
      v:y ->
      modu:Int.t ->
      state:t -> t

    exception Error_Bottom

    val cached_fold :
      f:(Base.t -> loffset -> 'a) ->
      cache:string * int -> temporary:bool ->
      joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a

    val cached_map :
      f:(Base.t -> loffset -> loffset) ->
      cache:string * int -> temporary:bool ->
      t -> t

    exception Found_prefix of Hptmap.prefix * subtree * subtree
end

module Make_LOffset
  (VALUE:Lattice_With_Isotropy.S)
  (LOffset:Offsetmap.S with type y = VALUE.t
                       and type widen_hint = VALUE.widen_hint)
  (Default_offsetmap: sig val default_offsetmap : Base.t -> LOffset.t end):
  Location_map with type y = VALUE.t
               and type widen_hint_y = VALUE.widen_hint
               and type loffset = LOffset.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
