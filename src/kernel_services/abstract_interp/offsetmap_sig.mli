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

(** Signature for {!Offsetmap} module, that implement efficient maps from
    intervals to arbitrary values. *)
(* This module is declared as a pure mli to avoid duplicating the
   interface of [Offsetmap] in the .ml and in the .mli files. *)

open Abstract_interp

type v (** Type of the values stored in the offsetmap *)
type widen_hint
type alarm = bool (** [true] indicates that an alarm may have occurred *)
include Datatype.S (** Datatype for the offsetmaps *)
type t_bottom = [ `Bottom | `Map of t]
type t_top_bottom = [ `Bottom | `Map of t | `Top ]


(** {2 Pretty-printing} *)

val pretty_generic:
  ?typ:Cil_types.typ ->
  ?pretty_v:(Cil_types.typ option -> Format.formatter -> v -> unit) ->
  ?skip_v:(v -> bool) ->
  ?sep:string ->
  unit ->
  Format.formatter -> t -> unit

(** {2 Creating basic offsetmaps} *)

val create: size:Int.t -> v -> size_v:Int.t -> t
(** [create ~size v ~size_v] creates an offsetmap of size [size] in which the
    intervals [k*size_v .. (k+1)*size_v-1] with [0<= k <= size/size_v] are all
    mapped to [v].  *)

val create_isotropic: size:Int.t -> v -> t
(** Same as {!create}, but for values that are isotropic. In this case,
    [size_v] is automatically computed. *)

val of_list: ((t -> v -> t) -> t -> 'l -> t) -> 'l -> Int.t -> t
(** [from_list fold c size] creates an offsetmap by applying the iterator
    [fold] to the container [c], the elements of [c] being supposed to
    be of size [size]. [c] must be such that [fold] is called at least
    once. *)


(** {2 Iterators} *)

val iter:
  ((Int.t * Int.t) -> (v * Int.t * Rel.t) -> unit) ->
  t -> unit
(** [iter f m] calls [f] on all the intervals bound in [m], in increasing
    order. The arguments of [f (min, max) (v, size, offset)] are as follows:
    - [(start, stop)] are the bounds of the interval, inclusive.
    - [v] is the value bound to the interval, and [size] its size;  if [size]
      is less than [stop-start+1], [v] repeats itself until [stop].
    - [offset] is the offset at which [v] starts in the interval;
      it ranges over [0..size-1]. If [offset] is [0], [v] starts
      at the beginning of the interval. Otherwise, it starts at [offset-size].
 *)

val fold:
  ((Int.t * Int.t) -> (v * Int.t * Rel.t) -> 'a -> 'a) ->
  t -> 'a -> 'a
(** Same as [iter], but with an accumulator. *)

val fold_between:
  ?direction:[`LTR | `RTL] ->
  entire:bool ->
  Int.t * Int.t ->
  ((Int.t * Int.t) -> (v * Int.t * Rel.t) -> 'a -> 'a) ->
  t -> 'a -> 'a
(** [fold_between ~direction:`LTR ~entire (start, stop) m acc] is similar to
    [fold f m acc], except that only the intervals that intersect [start..stop]
    (inclusive) are presented. If [entire] is true, intersecting intervals are
    presented whole (ie. they may be bigger than [start..stop]). If [entire]
    is [false], only the intersection with [ib..ie] is presented.
    [fold_between ~direction:`RTL] reverses the iteration order: interval
    are passed in decreasing order. Default is [`LTR]. *)


(** {2 Interval-unaware iterators} *)

val iter_on_values: (v -> unit) -> t -> unit
(** [iter_on_values f m] iterates on the entire contents of [m], but [f]
    receives only the value bound to each interval. Interval bounds, the
    alignment of the value and its size are not computed. *)

val fold_on_values: (v -> 'a -> 'a) -> t -> 'a -> 'a
(** Same as [iter_on_values] but with an accumulator *)

val map_on_values: (v -> v) -> t -> t
(** [map_on_values f m ] creates the map derived from [m] by applying [f] to
    each interval. For each interval, the size of the new value and its offset
    relative to the beginning of the interval is kept unchanged. *)

type map2_decide =
   ReturnLeft | ReturnRight | ReturnConstant of v | Recurse
(** This type describes different possibilities to accelerate a simultaneous
    iteration on two offsetmaps. {!ReturnLeft} (resp. {!ReturnRight}) means
    'return the left (resp. right) operand unchanged, and stop the recursive
    descent'. [ReturnConstant v] means 'return a constant offsetmap of the good
    size and that contains [v] everywhere'. It is always correct to return
    {!Recurse}, which will force the recursion until the maps have been fully
    decomposed.

    Typical usage include functions that verify [f v v = v], maps [m] such that
    [f m m' = m'], etc. *)

val map2_on_values:
  Hptmap_sig.cache_type -> (t -> t -> map2_decide) -> (v -> v -> v) -> t -> t -> t
(** [map2_on_values cache decide join m1 m2] applies [join] pointwise to
    all the elements of [m1] and [m2] and buils the resulting map. This function
    can only be called if [m1] and [m2] contain isotropic values. [decide]
    is called during the iteration, and can be used to return early; it is
    always correct to return {!Recurse}. Depending on [cache], the results of
    the partially applied function [map2_on_values cache decide join] will be
    cached between different calls. *)


(** {2 Join and inclusion testing} *)

include Lattice_type.Join_Semi_Lattice with type t := t
include Lattice_type.With_Narrow with type t := t

val join_top_bottom: [< t_top_bottom] -> [<  t_top_bottom] -> [> t_top_bottom]

val widen : widen_hint -> t -> t -> t
(** [widen wh m1 m2] performs a widening step on [m2], assuming that
    [m1] was the previous state. The relation [is_included m1 m2] must hold *)


(** {2 Searching values} *)

val find :
  validity:Base.validity ->
  ?conflate_bottom:bool ->
  offsets:Ival.t -> size:Integer.t ->
  t -> bool * v
(** Find the value bound to a set of intervals, expressed as an ival, in the
    given rangemap. The returned boolean (alarm) indicates that at least one
    of the offsets does not comply with [validity]. *)

val find_imprecise: validity:Base.validity-> t -> v
(** [find_imprecise ~validity m] returns an imprecise join of the values bound
    in [m], in the range described by [validity].  *)

val find_imprecise_everywhere: t -> v
(** Returns an imprecise join of all the values bound in the offsetmap. *)

val copy_slice:
  validity:Base.validity ->
  offsets:Ival.t -> size:Integer.t ->
  t -> alarm * [`Map of t | `Bottom]
(** [copy_slice ~validity ~offsets ~size m] copies and merges the slices of
    [m] starting at offsets [offsets] and of  size [size]. Offsets invalid
    according to [validity] are removed. [size] must be strictly greater
    than zero. *)

(** {2 Adding values} *)

val add : ?exact:bool -> (Int.t * Int.t) -> (v * Int.t * Rel.t) -> t -> t
(** [add (min, max) (v, size, offset) m] maps the interval
    [min..max] (inclusive) to the value [v] in [m]. [v] is assumed as having
    size [size]. If [stop-start+1] is greater than [size], [v] repeats itself
    until the entire interval is filled. [offset] is the offset at which [v]
    starts in the interval, interpreted as for {!iter}. Offsetmaps cannot
    contain holes, so [m] must already bind at least the intervals [0..start-1].
*)

val update :
  ?origin:Origin.t ->
  validity:Base.validity ->
  exact:bool ->
  offsets:Ival.t ->
  size:Int.t ->
  v ->
  t -> alarm * t_bottom
(** [update ?origin ~validity ~exact ~offsets ~size v m] writes [v],
    of size [size], each [offsets] in [m]; [m] must be of the size implied by
    [validity]. [~exact=true] results in a strong update, while
    [~exact=false] performs a weak update. If [offsets] contains too many
    offsets, or if [offsers] and [size] are not compatible, [offsets] and/or
    [v] are over-approximated. In this case, [origin] is used as the source of
    the resulting imprecision. Returns [`Bottom] when all offsets are invalid.
    The boolean returned indicates a potential alarm. *)

val update_under :
  validity:Base.validity ->
  exact:bool ->
  offsets:Ival.t ->
  size:Int.t ->
  v ->
  t -> alarm * t_bottom
(** Same as {!update}, except that no over-approximation on the set
    of offsets or on the value written occurs. In case of imprecision,
    [m] is not updated. *)


val update_imprecise_everywhere:
  validity:Base.validity ->
  Origin.t -> v ->
  t -> t_bottom
(** [update_everywhere ~validity o v m] computes the offsetmap resulting
    from imprecisely writing [v] potentially anywhere where [m] is valid
    according to [validity]. If a value becomes too imprecise, [o] is used
    as origin. *)

val paste_slice:
  validity:Base.validity ->
  exact:bool ->
  from:t ->
  size:Int.t ->
  offsets:Ival.t ->
  t -> alarm * t_bottom


(** {2 Shape} *)

val cardinal_zero_or_one: t -> bool
(** Returns [true] if and only if all the interval bound in the
    offsetmap are mapped to values with cardinal at most 1. *)

(** [is_single_interval ?f o] is true if
    (1) the offsetmap [o] contains a single binding
    (2) either [f] is [None], or the bound value [v] verifies [f v]. *)
val is_single_interval: ?f:(v -> bool) -> t -> bool

val single_interval_value: t -> v option
(** [single_interval_value o] returns [Some v] if [o] contains a single
    interval, to which [v] is bound, and [None] otherwise. *)


(** {2 Misc} *)

val imprecise_write_msg: string ref
(** The message "more than N <imprecise_msg_write>. Approximating." is displayed
    when the offsetmap must update too many locations in one operation. *)

(** Clear the caches local to this module. Beware that they are not
    project-aware, and that you must call them at every project switch. *)
val clear_caches: unit -> unit

(**/**)

val pretty_debug: t Pretty_utils.formatter

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
