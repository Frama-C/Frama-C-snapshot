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

(** Maps from intervals to values (aka rangemaps).
*)
module Int :
    sig
      type t = int64
      val equal: t -> t -> bool
    end

module Make :
  functor (V : Lattice_With_Isotropy.S) ->
sig
  type t
  type y

  val empty : t
  val is_empty: t -> bool
  val equal_vv : Int.t * Int.t * V.t -> Int.t * Int.t * V.t -> bool
  val equal : t -> t -> bool
  val get_max : t -> Int.t

  exception Empty_tree

  val fold_offset :
    (Int.t -> int64 -> Int.t -> Int.t -> V.t -> 'a -> 'a) ->
    Int.t -> t -> 'a -> 'a
  val fold :
    (Int.t -> int64 -> Int.t -> Int.t -> V.t -> 'a -> 'a) ->
    t -> 'a -> 'a
  val iter_offset :
    (Int.t -> int64 -> Int.t -> Int.t -> V.t -> 'a) -> Int.t -> t -> unit
  val iter : (Int.t -> int64 -> Int.t -> Int.t -> V.t -> 'a) -> t -> unit
  (** Common folding and iteration operations, *)

  val pretty_node :
    Format.formatter -> int64 -> int64 -> int64 -> int64 -> V.t -> unit
  val pretty_offset : int64 -> Format.formatter -> t -> unit
  val pretty : Format.formatter -> t -> unit
  val pretty_debug_offset : int64 -> Format.formatter -> t -> unit
  val pretty_debug : Format.formatter -> t -> unit
  val print_offset : int64 -> t -> unit
  val fprint : Format.formatter -> t -> unit
  val print : t -> unit
  (* Some printing functions. Some might need to be deprecated,*)

  exception Interval_not_found of Int.t * Int.t
  val subtree_from_interval : Int.t -> Int.t -> Int.t -> t -> Int.t * t
  val make_node :
    int64 ->
    Int.t ->
    Int.t -> t -> Int.t -> t -> Int.t -> Int.t -> V.t -> int64 * t

  val add_node : int64 -> int64 -> int64 -> Int.t -> V.t -> int64 -> t -> int64
    * t
  val is_included_generic_exn :
    (V.t -> V.t -> 'a) -> Int.t -> t -> Int.t -> t -> unit
  val is_included : t -> t -> bool
  val join : t -> t -> int64 * t

  val find_ival :
    conflate_bottom:bool ->
    validity:Base.validity -> with_alarms:CilE.warn_mode
    -> Ival.t -> t -> Abstract_interp.Int.t -> V.t
    (** Find a set of intervals in a given rangemap. *)

  val imprecise_find: Int.t -> Int.t -> t -> V.t
  (** imprecise_find [first_bit] [last_bit] [offsetmap] returns the value found
      in the interval ([first_bit], [last_bit]) in [offsetmap].
      If the interval ([first_bit], [last_bit]) spans multiple intervals in
      [offsetmap], all the values found are joined together. *)

  val update_ival :
    int64 ->
    int64 ->
    exact:bool ->
    mn:Int.t ->
    mx:Int.t ->
    period:Int.t -> size:Int.t -> t -> V.t -> t
(** Update a set of intervals in a given rangemap rooted a given offset.
    all offsets starting from mn ending in mx must be updated
    with value v, every period with a given size.
    Exact or imprecise update can be requested.
    Return a pair: offset, tree where the tree root begins at offset
*)

  val copy_ival:
    with_alarms:CilE.warn_mode ->
    validity:Base.validity ->
    Ival.tt -> t -> My_bigint.t 
    -> t
 (** [shift_ival with_alarms validity ival size tree] creates a new offsetmap
    from the intervals [ival] of size [size].
    This offsetmap is rooted at zero (this is a new base for the values).
 *)
end
