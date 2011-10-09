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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

open Abstract_interp
open Abstract_value

type itv = Int.t * Int.t

(** May be raised by [update_ival] when the add is completely out of bounds. *)
exception Result_is_bottom
exception Result_is_same
exception Found_Top

module type S = sig

  type y
  type widen_hint

  include Datatype.S

  val tag : t -> int

  val empty : t
  val is_empty : t -> bool
  val pretty_c_assert_typ :
    string -> Cil_types.typ -> (unit->unit) -> Format.formatter -> t -> unit
  val pretty_typ : Cil_types.typ option -> Format.formatter -> t -> unit
  val pretty_debug : Format.formatter -> t -> unit

  val reduce : Ival.t -> size:Int.t -> y -> t -> t
(** [reduce i s v o] tries to reduce the offsetmap [o] with the additional
    knowledge that the value of size [s] at [i] is included in [v].
    May raise [Result_is_same]. *)

  val is_included : t -> t -> bool
  val is_included_exn : t -> t -> unit
  val is_included_exn_generic : (y -> y -> unit) -> t -> t -> unit

  val join : t -> t -> (Int.t * Int.t) list * t
  val widen : widen_hint -> t -> t -> t

  val find_ival :
    conflate_bottom:bool -> validity:Base.validity ->
    with_alarms:CilE.warn_mode -> Ival.t -> t -> Int.t -> y
    (** May raise [Not_found] if V.top is found *)

  val cardinal_zero_or_one: Base.validity -> t -> bool

  val find_imprecise_entire_offsetmap : validity:Base.validity -> t -> y

  val concerned_bindings_ival :
    offsets:Ival.t -> offsetmap:t -> size:Int.t -> y list -> y list
    (** accumulates the list of the values associated to at
        least one bit of the ival. For this function Top is not a binding! *)

  val update_ival :
    with_alarms:CilE.warn_mode ->
    validity:Base.validity ->
    exact:bool ->
    offsets:Ival.t ->
    size:Int.t ->
    t -> y -> t
    (** May raise [Result_is_bottom] if this is completely out of bound *)

  val overwrite : t -> y -> Origin.t -> t
    (** [overwrite m v o] computes the offsetmap resulting from writing
        [v] potentially anywhere in [m] *)

  val over_intersection : t -> t -> t
    (** An over-approximation of the intersection.  The arguments can not be
        arbitrary offsetmaps: the algorithm would be too complicated. The
        provided algorithm should work fine with offsetmaps that correspond to
        the relation view and the memory view of the same analysed code. *)

  val from_cstring : Base.cstring -> t
  val add_internal : itv -> Int.t * Int.t * y -> t -> t
  val add_whole :  itv -> y -> t -> t
  val remove_whole :  itv -> t -> t

  val fold_whole :
    size:Int.t -> (Ival.t -> Int.t -> y -> 'a -> 'a) -> t -> 'a -> 'a
    (** May raise [Invalid_argument "Offsetmap.Make.fold"] *)

  val fold_single_bindings :
    size:Int.t -> (Ival.t -> Int.t -> y -> 'a -> 'a) -> t -> 'a -> 'a
    (** May raise [Invalid_argument "Offsetmap.Make.fold"] *)

  val fold_internal :
    (itv -> (Int.t * Int.t * y) -> 'a -> 'a) -> t -> 'a -> 'a

  val shift_ival : Ival.t -> t -> t option -> t option
    (** [shift_ival shift o acc] returns the join of [acc] and
        of [o] shifted by all values in [shift].
        Raises [Found_Top] when the result is [Top]. *)

  val copy_paste : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy_merge : t -> Int.t -> Int.t -> Int.t -> t -> t
  val copy_offsmap : t -> Int.t -> Int.t -> t
  val copy_ival :
    validity:Base.validity ->
    with_alarms:CilE.warn_mode ->
    Ival.t -> t -> Int.t -> t

  val merge_by_itv :  t -> t -> Int_Intervals.t -> t
  val shift : Int.t -> t -> t
  val sized_zero : size_in_bits:Int.t -> t

  val reciprocal_image : t -> Base.t -> Int_Intervals.t * Ival.t
    (** [reciprocal_image m b] is the set of bits in the offsetmap [m]
        that may lead to Top([b]) and  the set of offsets in [m]
        where one can read an address [b]+_ *)

  val create_initial: v:y -> modu:Int.t -> t

  val reduce_by_int_intervals: t -> Abstract_value.Int_Intervals.t -> t

  val top_stuff :
    (y -> bool) -> (y -> 'a * y) -> ('a -> 'a -> 'a) -> 'a -> t -> 'a * t

  val iter_contents : (y -> unit) -> t -> Int.t -> unit
    (** Iter on the contents of offsetmap of given size *)

  val fold : (Int.t * Int.t -> Int.t * Int.t * y -> 'a -> 'a) -> t -> 'a -> 'a

  val is : t -> y -> bool
end

module Make(V : Lattice_With_Isotropy.S):
  S with type y = V.t and type widen_hint = V.widen_hint

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
