(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(** Memory locations.
    @plugin development guide *)

open Cil_types
open Abstract_interp

(** Association between bases and offsets in byte.
    @plugin development guide *)
module Location_Bytes : sig
  (* TODOBY: write an mli for MapLattice, and name the result. Use it there,
     and simplify *)

  module M : sig
    type key = Base.t
    type t (** Mapping from bases to bytes-expressed offsets *)
    val iter : (Base.t -> Ival.t -> unit) -> t -> unit
    val find :  key -> t -> Ival.t
    val fold : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
    val shape: t -> Ival.t Hptmap.Shape(Base.Base).t
  end

  type t =
    | Top of Base.SetLattice.t * Origin.t
       (** Garbled mix of the addresses in the set *)
    | Map of M.t (** Precice set of addresses+offsets *)
  (** This type should be considered private *)
  (* TODO: make it private when OCaml 4.01 is mandatory *)

  type size_widen_hint = Ival.size_widen_hint
  type generic_widen_hint = Base.t -> Ival.generic_widen_hint
  type widen_hint = size_widen_hint * generic_widen_hint

  (** Those locations have a lattice structure, including standard operations
      such as [join], [narrow], etc. *)
  include Lattice_type.AI_Lattice_with_cardinal_one
    with type t := t
    and type widen_hint := widen_hint
  include Lattice_type.With_Error_Top

  include Datatype.S_with_collections with type t := t

  val singleton_zero : t
    (** the set containing only the value for to the C expression [0] *)
  val singleton_one : t
    (** the set containing only the value [1] *)
  val zero_or_one : t

  val is_zero : t -> bool
  val is_bottom : t -> bool

  val top_int : t
  val top_float : t
  val top_single_precision_float : t

  val inject : Base.t -> Ival.t -> t
  val inject_ival : Ival.t -> t
  val inject_float : Fval.F.t -> t

  val add : Base.t -> Ival.t ->  t ->  t
  (** [add b i loc] binds [b] to [i] in [loc] when [i] is not {!Ival.bottom},
      and returns {!bottom} otherwise. *)

  val diff : t -> t -> t
    (** Over-approximation of difference. [arg2] needs to be exact or an
        under_approximation. *)

  val diff_if_one : t -> t -> t
      (** Over-approximation of difference. [arg2] can be an
          over-approximation. *)

  val shift : Ival.t -> t -> t
  val shift_under : Ival.t -> t -> t
  (** Over- and under-approximation of shifting the value by the given Ival. *)

  val sub_pointwise: ?factor:Int_Base.t -> t -> t -> Ival.t
  (** Subtracts the offsets of two locations [loc1] and [loc2].
      Returns the pointwise substraction of their offsets
      [off1 - factor * off2]. [factor] defaults to [1]. *)

  (** Topifying of values, in case of imprecise accesses *)
  val topify_arith_origin : t -> t
  val topify_misaligned_read_origin : t -> t
  val topify_merge_origin : t -> t
  val topify_leaf_origin : t -> t
  val topify_with_origin: Origin.t -> t -> t
  val topify_with_origin_kind: Origin.kind -> t -> t
  val inject_top_origin : Origin.t -> Base.Hptset.t -> t
    (** [inject_top_origin origin p] creates a top with origin [origin]
        and additional information [param] *)
  val top_with_origin: Origin.t -> t
    (** Completely imprecise value. Use only as last resort. *)


  (* {2 Iterators} *)

  val fold_bases : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** Fold on all the bases of the location, including [Top bases].
        @raise Error_Top in the case [Top Top]. *)
  val fold_i : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** Fold with offsets. 
        @raise Error_Top in the cases [Top Top], [Top bases]. *)
  val fold_topset_ok: (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** Fold with offsets, including in the case [Top bases]. In this case,
        [Ival.top] is supplied to the iterator.
        @raise Error_Top in the case [Top Top]. *)

  val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_enum f loc acc] enumerates the locations in [acc], and passes
        them to [f]. Make sure to call {!cardinal_less_than} before calling
        this function, as all possible combinations of bases/offsets are
        presented to [f]. Raises {!Error_Top} if [loc] is [Top _] or if
        one offset cannot be enumerated. *)

  val cached_fold:
    cache_name:string ->
    temporary:bool ->
    f:(Base.t -> Ival.t -> 'a) ->
    projection:(Base.t -> Ival.t) ->
    joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a
  (** Cached version of [fold_i], for advanced users *)

  val filter_base : (Base.t -> bool) -> t -> t


  (** {2 Number of locations} *)

  val cardinal_zero_or_one : t -> bool
  val cardinal_less_than : t -> int -> int
  (** [cardinal_less_than v card] returns the cardinal of [v] if it is less
      than [card], or raises [Not_less_than]. *)
  val cardinal: t -> Integer.t option (** None if the cardinal is unbounded *)

  val find_lonely_key : t -> Base.t * Ival.t
  (** if there is only one base [b] in the location, then returns the
      pair [b,o] where [o] are the offsets associated to [b].
      @raise Not_found otherwise. *)

  val find_lonely_binding : t -> Base.t * Ival.t
  (** if there is only one binding [b -> o] in the location (that is, only
      one base [b] with [cardinal_zero_or_one o]), returns the pair [b,o].
      @raise Not_found otherwise *)


  (** {2 Destructuring} *)
  val find: Base.t -> t -> Ival.t
  val find_or_bottom : Base.t -> M.t -> Ival.t
  val split : Base.t -> t -> Ival.t * t

  val get_bases : t -> Base.SetLattice.t
    (** Returns the bases the location may point to. Never fails, but
        may return [Base.SetLattice.Top]. *)


  (** {2 Local variables inside locations} *)

  val contains_addresses_of_locals : (M.key -> bool) -> t -> bool
    (** [contains_addresses_of_locals is_local loc] returns [true]
        if [loc] contains the adress of a variable for which
        [is_local] returns [true] *)

  val remove_escaping_locals : (M.key -> bool) -> t -> Base.SetLattice.t * t
    (**  TODO: merge with above function
         [remove_escaping_locals is_local v] removes from [v] information
         associated with bases for which [is_local] returns [true]. *)

  val contains_addresses_of_any_locals : t -> bool
    (** [contains_addresses_of_any_locals loc] returns [true] iff [loc] contains
        the adress of a local variable or of a formal variable. *)

  (** {2 Misc} *)

  val iter_on_strings :
    skip:Base.t option -> (Base.t -> string -> int -> int -> unit) -> t -> unit

  val partially_overlaps : size:Int.t -> t -> t -> bool
    (** Is there a possibly-non empty intersection between the two supplied
        locations, assuming they have size [size] *)

  (** [is_relationable loc] returns [true] iff [loc] represents a single
      memory location. *)
  val is_relationable: t -> bool

  val may_reach : Base.t -> t -> bool
    (** [may_reach base loc] is true if [base] might be accessed from [loc]. *)


  val get_garbled_mix: unit -> t list
    (** All the garbled mix that have been created so far, sorted by "temporal"
        order of emission. *)

  val clear_garbled_mix: unit -> unit
    (** Clear the information on created garbled mix. *)

  val do_track_garbled_mix: bool -> unit
  
(**/**)
  val pretty_debug: t Pretty_utils.formatter
  val clear_caches: unit -> unit
end

(** Association between bases and offsets in bits.
    @plugin development guide *)
module Location_Bits : module type of Location_Bytes


(** Association between bases and ranges of bits.
    @plugin development guide *)
module Zone : sig

  type map_t

  (** This type should be considered private *)
  (* TODO: make it private when OCaml 4.01 is mandatory *)
  type t = private Top of Base.SetLattice.t * Origin.t | Map of map_t

  include Datatype.S_with_collections with type t := t
  val pretty_debug: t Pretty_utils.formatter

  include Lattice_type.Bounded_Join_Semi_Lattice with type t := t
  include Lattice_type.With_Top with type t := t
  include Lattice_type.With_Narrow with type t := t
  include Lattice_type.With_Under_Approximation with type t := t
  include Lattice_type.With_Diff with type t := t

  val is_bottom: t -> bool
  val inject : Base.t -> Int_Intervals.t -> t

  exception Error_Top

  val map_i : (Base.t -> Int_Intervals.t -> t) -> t -> t

  val find_lonely_key : t -> Base.t * Int_Intervals.t
  val find_or_bottom : Base.t -> map_t -> Int_Intervals.t
  val find: Base.t -> t -> Int_Intervals.t

  val mem_base : Base.t -> t -> bool
    (** [mem_base b m] returns [true] if [b] is associated to something
        or topified in [t], and [false] otherwise.

        @since Carbon-20101201 *)

  val intersects : t -> t -> bool

(** Assuming that [z1] and [z2] only contain valid bases,
   [valid_intersects z1 z2] returns true iff [z1] and [z2] have a valid
    intersection. *)
  val valid_intersects : t -> t -> bool

  (** {3 Folding} *)

  val filter_base : (Base.t -> bool) -> t -> t
    (** [filter_base] can't raise Error_Top since it filters bases of [Top
        bases]. Note: the filter may give an over-approximation (in the case
        [Top Top]). *)

  val fold_bases : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_bases] folds also bases of [Top bases].
        @raise Error_Top in the case [Top Top]. *)

  val fold_i : (Base.t -> Int_Intervals.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_i f l acc] folds [l] by base.
        @raise Error_Top in the cases [Top Top], [Top bases]. *)

  val fold_topset_ok : (Base.t -> Int_Intervals.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_i f l acc] folds [l] by base.
        @raise Error_Top in the case [Top Top]. *)

  val cached_fold :
    cache_name:string ->
    temporary:bool ->
    f:(Base.t -> Int_Intervals.t -> 'b) ->
    projection:(Base.t -> Int_Intervals.t) ->
    joiner:('b -> 'b -> 'b) -> empty:'b -> t -> 'b

  val fold2_join_heterogeneous:
    cache:Hptmap_sig.cache_type ->
    empty_left:('a Hptmap.Shape(Base.Base).t -> 'b) ->
    empty_right:(t -> 'b) ->
    both:(Base.t -> Int_Intervals.t -> 'a -> 'b) ->
    join:('b -> 'b -> 'b) ->
    empty:'b ->
    t -> 'a Hptmap.Shape(Base.Base).t ->
    'b


  (** {3 Misc} *)
  val shape: map_t -> Int_Intervals.t Hptmap.Shape(Base.Base).t

(**/**)
  val clear_caches: unit -> unit
end

(** {2 Locations} *)

(** A {!Location_Bits.t} and a size in bits.
    @plugin development guide *)
type location = private {
  loc : Location_Bits.t;
  size : Int_Base.t;
}

(** @plugin development guide *)
module Location: Datatype.S with type t = location

val loc_bottom : location
val is_bottom_loc: location -> bool

val make_loc : Location_Bits.t -> Int_Base.t -> location

val loc_equal : location -> location -> bool
val loc_size : location -> Int_Base.t

val is_valid : for_writing:bool -> location -> bool
(** Is the given location entirely valid, as the destination of a write
    operation if [for_writing] is true, as the destination of a read
    otherwise. *)

val valid_part : for_writing:bool -> ?bitfield:bool -> location -> location
(** Overapproximation of the valid part of the given location. Beware that
    [is_valid (valid_part loc)] does not necessarily hold, as garbled mix
    may not be reduced by [valid_part].
    [bitfield] indicates whether the location may be the one of a bitfield, and
    is true by default. If it is set to false, the location is assumed to be
    byte aligned, and its offset (expressed in bits) is reduced to be congruent
    to 0 modulo 8. *)

val invalid_part : location -> location
(** Overapproximation of the invalid part of a location *)
(* Currently, this is the identity function *)

val cardinal_zero_or_one : location -> bool
(** Is the location bottom or a singleton? *)

val valid_cardinal_zero_or_one : for_writing:bool -> location -> bool
(** Is the valid part of the location bottom or a singleton? *)

val filter_base: (Base.t -> bool) -> location -> location
val filter_loc : location -> Zone.t -> location

val pretty : Format.formatter -> location -> unit
val pretty_english : prefix:bool -> Format.formatter -> location -> unit

(** {2 Conversion functions} *)

(* Note: the first two operations are exact (if offsets are not
   floats.) The last one can return an over-approximation, and has an
   under-approximating counterpart. *)
val loc_to_loc_without_size : location -> Location_Bytes.t
val loc_bytes_to_loc_bits : Location_Bytes.t -> Location_Bits.t
val loc_bits_to_loc_bytes : Location_Bits.t -> Location_Bytes.t
val loc_bits_to_loc_bytes_under : Location_Bits.t -> Location_Bytes.t

val enumerate_bits : location -> Zone.t
val enumerate_bits_under : location -> Zone.t

val enumerate_valid_bits : for_writing:bool -> location -> Zone.t
(** @plugin development guide *)

val enumerate_valid_bits_under : for_writing:bool -> location -> Zone.t

val zone_of_varinfo : varinfo -> Zone.t
  (** @since Carbon-20101201 *)

val loc_of_varinfo : varinfo -> location
val loc_of_base : Base.t -> location
val loc_of_typoffset : Base.t -> typ -> offset -> location

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
