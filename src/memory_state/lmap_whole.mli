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

(** Functor making map for whole values with locations as keys *)

open Abstract_interp
open Abstract_value
open Locations

exception Cannot_copy

module Make_LOffset
  (VALUE:Lattice_With_Isotropy.S)
  (LOffset : Offsetmap.S with type y = VALUE.t
			 and type widen_hint = VALUE.widen_hint) :
sig

  type y = VALUE.t (** type of the values associated to the locations *)
  type widen_hint_offsetmap = VALUE.widen_hint
(*  module LOffset : Offsetmap.S with type y = y
			       and type widen_hint = widen_hint_offsetmap
*)

  module Make
    (Default_offsetmap : sig val default_offsetmap : Base.t -> LOffset.t end) :
  sig

    include Datatype.S (** the datype of a map *)

    type widen_hint = bool * Base.Set.t * (Base.t -> widen_hint_offsetmap)
    type instanciation = Location_Bytes.t Base.Map.t

    val inject : Base.t -> LOffset.t -> t

    val pretty_without_null : Format.formatter -> t -> unit
    val pretty_filter :
      Format.formatter ->
      t -> Locations.Zone.t -> unit
    val add_binding :
      with_alarms:CilE.warn_mode -> exact:bool -> t -> location -> y -> t

    val find :
      with_alarms:CilE.warn_mode -> t -> location -> y

    val concerned_bindings : t -> location -> y list

    val join : t -> t -> t
    val is_included : t -> t -> bool
    val is_included_actual_generic : Zone.t -> t -> t -> instanciation

    (** Every location is associated to [VALUE.top] in [empty].*)
    val empty : t
    val is_empty : t -> bool

    (** Every location is associated to [VALUE.bottom] in [bottom].
        This state can be reached only in dead code. *)
    val bottom : t
    val is_reachable : t -> bool

    val widen : widen_hint-> t -> t -> (bool * t)
    val filter_base : (Base.t -> bool) -> t -> t

    (** @raise Not_found if the varid is not present in the map *)
    val find_base : Base.t -> t -> LOffset.t

    (** [copy_paste src dst state] returns a modified version of [state] in
        which everything present in [src] has been copied onto [dst]. [src] and
        [dst] must have the same size. The write operation is exact iff [dst]
        is exact. May raise [Cannot_copy]. *)
    val copy_paste : location -> location -> t -> t

    (** @raise Cannot_copy when ... *)
    val paste_offsetmap :
      LOffset.t -> Location_Bits.t -> Int.t -> Int.t -> t -> t

    (** May return [None] as a bottom LOffset.t
        @raise Cannot_copy when ...*)
    val copy_offsetmap : Locations.location -> t -> LOffset.t option

    val compute_actual_final_from_generic :
      t -> t -> Locations.Zone.t -> instanciation -> t

    val is_included_by_location_enum :  t -> t -> Locations.Zone.t -> bool

    (** @raise Invalid_argument "Lmap.fold" if one location is not aligned
        or of size different of [size]. *)
    val fold : size:Int.t -> (location -> y -> 'a -> 'a) -> t -> 'a -> 'a

    (** @raise Invalid_argument "Lmap.fold" if one location is not aligned
	or of size different of [size].*)
    val fold_single_bindings :
      size:Int.t -> (location -> y -> 'a -> 'a) -> t -> 'a -> 'a

    (** [fold_base f m] calls [f] on all bases bound to non top values in [m] *)
    val fold_base : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a

    val find_offsetmap_for_location : Location_Bits.t -> t -> LOffset.t
    val add_whole: location -> y -> t -> t
    val remove_whole: location -> t -> t

    (** [reciprocal_image m b] is the set of bits in the map [m] that may lead
        to Top([b]) and the location in [m] where one may read an address
        [b]+_ *)
    val reciprocal_image : Base.t -> t -> Zone.t*Location_Bits.t
    (*
      val create_initialized_var :
      Cil_types.varinfo -> Base.validity -> LOffset.t -> Base.t
     *)
    val create_initial :
      base:Base.t ->
      v:y ->
      modu:Int.t ->
      state:t -> t

    val cached_fold :
      f:(Base.t -> LOffset.t -> 'a) ->
      cache:string * int -> temporary:bool ->
      joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a

    val cached_map :
	f:(Base.t -> LOffset.t -> LOffset.t) ->
	  cache:string * int ->     temporary:bool ->
	    t -> t

 end

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
