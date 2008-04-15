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

(* $Id: locations.mli,v 1.75 2008/11/18 12:13:41 uid568 Exp $ *)

(** Memory locations. 
    @plugin development guide *)

open Cil_types
open Cil
open Abstract_interp
open Abstract_value
open BaseUtils

(** Association between varids and offsets in byte. 
    @plugin development guide *)
module Location_Bytes : sig

  module M : sig
    type key = Base.t
    type t
    val iter : (Base.t -> Ival.t -> unit) -> t -> unit
    val find :  key -> t -> Ival.t
    val fold : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Top_Param : Lattice_Set with type O.elt = Base.t

  type z =
    | Top of Top_Param.t * Origin.t
    | Map of M.t

  include Lattice with type t = z
                  and type widen_hint =
			  Top_Param.widen_hint * (Base.t -> Ival.widen_hint)

  val top_float : t
  val is_zero : t -> bool
  val hash : t -> int
  val zero_or_one : t

  val singleton_zero : t
    (** the set containing only the value corresponding to the
	C expression [0] *)

  val singleton_one : t
    (** the set containing only the value [1] *)

  val topify_arith_origin : t -> t
  val topify_misaligned_read_origin : t -> t
  val topify_merge_origin : t -> t
  val under_topify : t -> t

  val top_int : t
  val find_or_bottom : Base.t -> M.t -> Ival.t
  val add_or_bottom : Base.t -> Ival.t ->  M.t ->  M.t

  val inject : Base.t -> Ival.t -> t
  val inject_ival : Ival.t -> t

  val inject_top_origin : Origin.t -> Top_Param.O.t -> t
    (** [inject_top_origin origin p] creates a top with origin [origin]
	and additional information [param] *)

  val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a

  val find_exclusive : Base.t -> t -> Ival.t
    (** [find_exclusive k m] returns [v] if [m] contains only the binding [k]
	-> [v].
	@raise Not_exclusive otherwise. *)

  val split : Base.t -> t -> Ival.t * t
  exception Not_all_keys
  val get_keys_exclusive : Ival.t -> t -> Base.t list
  val find_lonely_binding : t -> Base.t * Ival.t
  val find_lonely_key : t -> Base.t * Ival.t
  val diff : t -> t -> t
  val diff_if_one : t -> t -> t
  val location_shift : Ival.t -> t -> t
  val fold_i : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_bases : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
  val top_leaf_origin : unit -> t

  val topify_with_origin : Origin.t -> t -> t
  val is_included_actual_generic :
    BaseUtils.BaseSet.t ->
    BaseUtils.BaseSet.t ref -> t BaseUtils.BaseMap.t ref -> t -> t -> unit

  val may_reach : Base.t -> t -> bool
    (** [may_reach base loc] is true if [base] might be accessed from [loc]. *)

  val cached_fold: cache:string * int ->
    f:(Base.t -> Ival.t -> 'a) ->
    projection:(Base.t -> Ival.t) ->
    joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a

  val contains_adresses_of_locals : Cil_types.fundec -> t -> bool
    (** [contains_adresses_of_locals f loc] returns [true] if [loc] contains
	the adress of a local variable of [f] or a formal of [f]*)

  val remove_escaping_locals : Cil_types.fundec -> t -> t
    (**  TODO: merge with above function *)

  val contains_adresses_of_any_locals : t -> bool
    (** [contains_adresses_of_any_locals loc] returns [true] iff [loc] contains
	the adress of a local variable or of a formal variable. *)

end

(** Association between varids and offsets in bits. 
    @plugin development guide *)
module Location_Bits : sig

  module M : sig
    type key = Base.t
    type t
    val find :  key -> t -> Ival.t
    val fold : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
  end

  module Top_Param : Lattice_Set with type O.elt = Base.t

  type t = Top of Top_Param.t * Origin.t | Map of M.t

  module Datatype: Project.Datatype.S with type t = t

  val top : t
  val bottom : t
  val find_or_bottom : Base.t -> M.t -> Ival.t
  val add_or_bottom : Base.t -> Ival.t -> M.t -> M.t
  val inject : Base.t -> Ival.t -> t
  val inject_ival : Ival.t -> t

  val join : t -> t -> t
    (** Over-approximation of union. *)

  val link : t -> t -> t
    (** Under_approximation of union. *)

  val narrow : t -> t -> t
    (** Over-approximation of intersection. *)

  val meet : t -> t -> t
    (** Under-approximation of intersection. *)

  val diff : t -> t -> t
    (** Over-approximation of difference. [arg2] needs to be exact or an
	under_approximation. *)

  val diff_if_one : t -> t -> t
      (** Over-approximation of difference. [arg2] can be an
	  over-approximation. *)

  exception Error_Bottom
  exception Error_Top

  val find_exclusive : Base.t -> t -> Ival.t
    (** [find_exclusive k m] returns [v] if [m] contains only the binding [k]
	-> [v].  @raise Not_exclusive otherwise. *)

  exception Not_all_keys
  val get_keys_exclusive : Ival.t -> t -> Base.t list
  val pretty : Format.formatter -> t -> unit

  val intersects : t -> t -> bool
    (** [intersects t1 t2] is true iff [t1] and [t2] have a nonempty
	intersection *)

  type widen_hint
  val widen : widen_hint -> t -> t -> t
    (*    val compare : t -> t -> int*)
  val equal : t -> t -> bool
  val is_included : t -> t -> bool
  val find_lonely_binding : t -> Base.t * Ival.t
  val find_lonely_key : t -> Base.t * Ival.t
  val cardinal_zero_or_one : t -> bool
  val cardinal_less_than : t -> int -> int
  val location_shift : Ival.t -> t -> t
  val filter_base : (Base.t -> bool) -> t -> t
  val fold_i : (Base.t -> Ival.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
  val is_in_set : set:Top_Param.O.t -> Base.t -> bool
  val fold_bases : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
  val get_bases : t -> Top_Param.t

end

(** Association between varids and ranges of bits. 
    @plugin development guide *)
module Zone : sig

(*  module M : Mergemap.S with type key = Base.t
                          and type 'a t = 'a Mergemap.Make(Base).t
*)
  module Top_Param: Lattice_Set with type O.elt = Base.t
  type map_t
  type t = Top of Top_Param.t * Origin.t | Map of map_t
  val top : t
  val bottom : t
  val id:string
  val tag: t -> int
  val inject : Base.t -> Int_Intervals.t -> t

  val join : t -> t -> t
    (** Over-approximation of union. *)

  val link : t -> t -> t
    (** Under_approximation of union. *)

  val narrow : t -> t -> t
    (** Over-approximation of intersection. *)

  val meet : t -> t -> t
    (** Over-approximation of intersection. *)

  val diff : t -> t -> t
    (** Over-approximation of difference.
        [arg2] needs to be exact or an under-approximation. *)

  val diff_if_one : t -> t -> t
      (** Over-approximation of difference. [arg2] can be an
	  over-approximation. *)

  exception Error_Bottom
  exception Error_Top

  val map_i : (Base.t -> Int_Intervals.t -> t) -> t -> t

  val find_exclusive : Base.t -> t -> Int_Intervals.t
    (** [find_exclusive k m] returns [v] if [m] contains only the binding [k]
	-> [v].
        @raise Not_exclusive otherwise. *)

  val find_lonely_key : t -> Base.t * Int_Intervals.t
  val find_or_bottom : Base.t -> map_t -> Int_Intervals.t
  exception Not_all_keys
  val get_keys_exclusive : Int_Intervals.t -> t -> Base.t list

  val pretty : Format.formatter -> t -> unit
  val intersects : t -> t -> bool

(** Assuming that [z1] and [z2] only contain valid bases, 
   [valid_intersects z1 z2] returns true iff [z1] and [z2] have a valid
    intersection. *)
  val valid_intersects : t -> t -> bool

  type widen_hint
  val widen : widen_hint -> t -> t -> t
    (*   val compare : t -> t -> int *)
  val equal : t -> t -> bool
  val is_included : t -> t -> bool
  val is_included_exn : t -> t -> unit
  val cardinal_zero_or_one : t -> bool
  val cardinal_less_than : t -> int -> int
  val diff : t -> t -> t

  (** {3 Folding} *)

  val filter_base : (Base.t -> bool) -> t -> t
    (** [filter_base] can't raise Error_Top since it filters bases of [Top
	bases]. Note: the filter may give an over-approximation (in the case
	[Top Top]). *)

  val fold_bases : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_bases] folds also bases of [Top bases].
	@raise Error_Top in the case [Top Top]. *)

  val get_bases : t -> Top_Param.t
    
  val fold_i : (Base.t -> Int_Intervals.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_i f l acc] folds [l] by base.
	@raise Error_Top in the cases [Top Top], [Top bases]. *)

  val fold_topset_ok : (Base.t -> Int_Intervals.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_i f l acc] folds [l] by base.
	@raise Error_Top in the case [Top Top]. *)

  val fold_enum_by_base : (t -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold_enum_by_base f l acc] folds [l] by base. It applies [f] to a
	partition of [l] by bases.
	@raise Error_Top in the case [Top Top], [Top bases]. *)

  val out_some_or_bottom : t option -> t

  val cached_fold :    
    cache:string * int ->
    f:(Base.t -> Abstract_value.Int_Intervals.t -> 'b) ->
    projection:(Base.t -> Abstract_value.Int_Intervals.t) ->
    joiner:('b -> 'b -> 'b) -> empty:'b -> t -> 'b

  module Datatype: Project.Datatype.S with type t = t

  (** {3 Lmap_bitwise utilities} *)

  (** The functions default and default_all are intended to be called by the
      functor Lmap_bitwise. *)

  val default :  Base.t -> Int.t -> Int.t -> t
  val defaultall :  Base.t -> t

end

(** {2 Locations} *)

(** A {!Location_Bits.t} and a size in bits. 
    @plugin development guide *)
type location = private {
  loc : Location_Bits.t;
  size : Int_Base.t;
}

val loc_bottom : location
val make_loc : Location_Bits.t -> Int_Base.t -> location

val loc_equal : location -> location -> bool
val loc_size : location -> Int_Base.t

val can_be_accessed : location -> bool
val is_valid : location -> bool
val cardinal_zero_or_one : location -> bool
val valid_cardinal_zero_or_one : location -> bool

val valid_part : location -> location
val invalid_part : location -> location

val pretty : Format.formatter -> location -> unit

(** {2 Conversion functions} *)

val loc_to_loc_without_size : location -> Location_Bytes.t
val loc_bytes_to_loc_bits : Location_Bytes.t -> Location_Bits.t
val loc_bits_to_loc_bytes : Location_Bits.t -> Location_Bytes.t

val loc_without_size_to_loc :
  Cil_types.lval -> Location_Bytes.t -> location
val loc_bits_to_loc : Cil_types.lval -> Location_Bits.t -> location

val valid_enumerate_bits : location -> Zone.t
  (** @plugin development guide *)

val loc_of_varinfo : varinfo -> location
val loc_of_base : Base.t -> location
val loc_of_typoffset : Base.t -> typ -> offset -> location
val filter_loc : location -> Zone.t -> location

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
