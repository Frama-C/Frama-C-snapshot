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

open Abstract_interp
open Abstract_value

val debug_now  : bool ref

type itv = Int.t * Int.t

(** May be raised by [update_ival] when the add is completely out of bounds. *)
exception Result_is_bottom

exception Found_Top

module type S = 
sig
  type t 
  type y
  type widen_hint

  module Datatype : Project.Datatype.OUTPUT with type t = t

  val tag : t -> int

  val empty : t
  val is_empty : t -> bool
  val equal : t->t->bool
  val pretty_typ : Cil_types.typ option -> Format.formatter -> t -> unit
  val pretty : Format.formatter -> t -> unit
  val pretty_debug : Format.formatter -> t -> unit
    
    
  val is_included : t -> t -> bool
  val is_included_exn : t -> t -> unit
  val is_included_exn_generic : (y -> y -> unit) -> t -> t -> unit

  val is_included_actual_generic : 
  BaseUtils.BaseSet.t ->
  BaseUtils.BaseSet.t ref ->
    Locations.Location_Bytes.t BaseUtils.BaseMap.t ref -> t -> t -> unit

  val join : t -> t -> (Int.t * Int.t) list * t

  val widen : widen_hint -> t -> t -> t
    
  val find_ival : 
    validity:Base.validity -> with_alarms:CilE.warn_mode
    -> Ival.t -> t -> Int.t -> y -> y
    (** May raise [Not_found] if V.top is found *)

  val concerned_bindings_ival : 
    offsets:Ival.t -> offsetmap:t -> size:Int.t -> y list -> y list
    (** Returns the list of the values associated to at least one bit of the 
	ival. For this function Top is not a binding ! *)

  val update_ival :
    with_alarms:CilE.warn_mode ->
    validity:Base.validity ->
    exact:bool ->
    offsets:Ival.t ->
    size:Int.t ->
    t -> y -> t
    (** May raise [Result_is_bottom] if this is completely out of bound *)

  val overwrite : t -> y -> Origin.t -> t

  val over_intersection : t -> t -> t
    (** An over-approximation of the intersection.  The arguments can not be
	arbitrary offsetmaps: the algorithm would be too complicated. The
	provided algorithm should work fine with offsetmaps that correspond to
	the relation view and the memory view of the same analysed code. *)

  val from_string : string -> t

  val add_whole :  itv -> y -> t -> t
  val remove_whole :  itv -> t -> t

  val fold_whole : 
    size:Int.t -> (Ival.t -> Int.t -> y -> 'a -> 'a) -> t -> 'a -> 'a
    (** May raise [Invalid_argument "Offsetmap.Make.fold"] *)

  val shift_ival : Ival.t -> t -> t option -> t option
    (** [shift_ival shift o acc] returns the join of [acc] and 
	of [o] shifted by all values in [shift].
	Raises [Found_Top] when the result is [Top]. *)

  val copy_paste : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy_merge : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy : t -> Int.t -> Int.t -> t

  val merge_by_itv :  t -> t -> Int_Intervals.t -> t
  val shift : Int.t -> t -> t
  val sized_zero : size_in_bits:Int.t -> t

  val reciprocal_image : t -> Base.t -> Int_Intervals.t * Ival.t
    (** [reciprocal_image m b] is the set of bits in the offsetmap [m] 
	that may lead to Top([b]) and  the set of offsets in [m]
	where one can read an address [b]+_ *)

  val create_initial: v:y -> modu:Int.t -> t

  val reduce_by_int_intervals: t -> Abstract_value.Int_Intervals.t -> t

  val top_stuff : (y -> bool) -> (y -> y) -> t -> t

  val iter_contents : (y -> unit) -> t -> Int.t -> unit
    (** Iter on the contents of offsetmap of given size *)
end

module Make(V : Lattice_With_Isotropy.S) :
  S with type y = V.t and type widen_hint = V.widen_hint



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
