(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(*i $Id: lmap.mli,v 1.71 2008/11/18 12:13:41 uid568 Exp $ i*)

(** Functors making map indexed by locations.
    @plugin development guide *)

open Abstract_interp
open Abstract_value
open Locations
open BaseUtils

exception Cannot_copy

module type Location_map =
sig
  type y (** type of the values associated to the locations *)
  type loffset
  type widen_hint_offsetmap
(*  module LOffset : Offsetmap.S with type y = y
			       and type widen_hint = widen_hint_offsetmap
*)
  module Make
    (Default_offsetmap: sig val default_offsetmap : Base.t -> loffset end):
  sig
    type t (** the type of a map *)

    type widen_hint = bool * BaseSet.t * (Base.t -> widen_hint_offsetmap)

    type instanciation = Location_Bytes.t BaseMap.t

    module Datatype : Project.Datatype.S with type t = t

    val inject : Base.t -> loffset -> t

    val add_offsetmap :  Base.t -> loffset -> t -> t

    val pretty : Format.formatter -> t -> unit
    val pretty_without_null : Format.formatter -> t -> unit
    val pretty_filter: Format.formatter -> t -> Locations.Zone.t -> unit
    val add_binding: 
      with_alarms:CilE.warn_mode -> exact:bool -> t -> location -> y -> t

    val find : with_alarms:CilE.warn_mode -> t -> location -> y

    val join : t -> t -> t
    val is_included : t -> t -> bool
    val equal : t -> t -> bool
    val hash : t -> int
    val is_included_actual_generic :
      Zone.t -> t -> t -> instanciation

    (** Every location is associated to [VALUE.top] in [empty].*)
    val empty : t
    val is_empty : t -> bool

    (** Every location is associated to [VALUE.bottom] in [bottom].
        This state can be reached only in dead code. *)
    val bottom : t
    val is_reachable : t -> bool

    val widen : widen_hint-> t -> t -> (bool * t)


    val filter_base : (Base.t -> bool) -> t -> t

    (** @raise Not_found if the varid is not present in the map. *)
    val find_base : Base.t -> t -> loffset

    (** Removes the base if it is present. Does nothing otherwise. *)
    val remove_base : Base.t -> t -> t
      

    (** [copy_paste src dst state] returns a modified version of [state] in
        which everything present in [src] has been copied onto [dst]. [src] and
        [dst] must have the same size. The write operation is exact iff [dst]
        is exact. 
	@raise Cannot_copy if copy is not possible. *)
    val copy_paste : location -> location -> t -> t

    (** @raise Cannot_copy if copy is not possible. *)
    val paste_offsetmap :
      loffset -> Location_Bits.t -> Int.t -> Int.t -> t -> t

    (** May return [None] as a bottom loffset.
        @raise Cannot_copy if copy is not possible. *)
    val copy_offsetmap : Locations.location -> t -> loffset option

    val compute_actual_final_from_generic :
      t -> t -> Locations.Zone.t -> instanciation -> t*Location_Bits.Top_Param.t

    val is_included_by_location_enum :  t -> t -> Locations.Zone.t -> bool

    (** @raise Invalid_argument if one location is not aligned or of size
        different of [size].
        @raise Error_Bottom if [m] is bottom. *)
    val fold : size:Int.t -> (location -> y -> 'a -> 'a) -> t -> 'a -> 'a


    (** [fold_base f m] calls [f] on all bases bound to non top
	offsetmaps in the non bottom map [m].
        @raise Error_Bottom if [m] is bottom. *)
    val fold_base : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a

    (** [fold_base_offsetmap f m] calls [f] on all bases bound to non
        top offsetmaps in the non bottom map [m].
        @raise Error_Bottom if [m] is bottom.*)
    val fold_base_offsetmap : (Base.t -> loffset -> 'a -> 'a) -> t -> 'a -> 'a

    val find_offsetmap_for_location : Location_Bits.t -> t -> loffset
    val add_whole: location -> y -> t -> t
    val remove_whole: location -> t -> t

    (** [reciprocal_image m b] is the set of bits in the map [m] that may lead
        to Top([b]) and  the location in [m] where one may read an address
        [b]+_ *)
    val reciprocal_image : Base.t -> t -> Zone.t*Location_Bits.t
      (*
        val create_initialized_var :
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
      cache:string * int -> joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a

    val cached_map :
      f:(Base.t -> loffset -> loffset) ->
      cache:string * int ->
      t -> t
  end
end

module Make_LOffset
  (VALUE:Lattice_With_Isotropy.S) 
  (LOffset:Offsetmap.S with type y = VALUE.t 
		       and type widen_hint = VALUE.widen_hint) :
  Location_map with type y = VALUE.t
	       and type widen_hint_offsetmap = VALUE.widen_hint
	       and type loffset = LOffset.t
