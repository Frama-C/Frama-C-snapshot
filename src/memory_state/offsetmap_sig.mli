(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
include Datatype.S (** Datatype for the offsetmaps *)

(** {2 Pretty-printing} *)

val pretty : Format.formatter -> t -> unit
val pretty_typ: Cil_types.typ option -> Format.formatter -> t -> unit


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
    be of size [size]. *)

(** {2 Empty offsetmap} *)

val empty : t
val is_empty: t -> bool


(** {2 Iterators} *)

val iter:
  ((Int.t * Int.t) -> (v * Int.t * Rel.t) -> unit) ->
  t -> unit
(** [iter f m] calls [f] on all the intervals bound in [m], in increasing
    order. The arguments of [f (min, max) (v, size, offset)] are as follow:
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
  entire:bool ->
  Int.t * Int.t ->
  ((Int.t * Int.t) -> (v * Int.t * Rel.t) -> 'a -> 'a) ->
  t -> 'a -> 'a
(** [fold_between ~entire (start, stop) m acc] is similar to [fold f m acc],
    except that only the intervals that intersect [start..stop] (inclusive)
    are presented. If [entire] is true, intersecting intervals are presented
    whole (ie. they may be bigger than [start..stop]). If [entire] is [false],
    only the intersection with [ib..ie] is presented. *)
  
val iter_on_values:
  (v -> Int.t -> unit) ->
  t -> unit
(** [iter_on_values f m] iterates on the entire contents of [m], but [f]
    receives only the value bound to each interval and the size of this value.
    Interval bounds and the offset of the value are not computed. *)

val fold_on_values: (v -> Int.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Same as [iter_on_values] but with an accumulator *)


(** {2 Join and inclusion testing} *)

val join : t -> t -> t

val is_included : t -> t -> bool
(** [is_included m1 m2] tests whether [m1] is included in [m2]. *)

val widen : widen_hint -> t -> t -> t
(** [widen wh m1 m2] performs a widening step on [m2], assuming that
    [m1] was the previous state. The relation [is_included m1 m2] must hold *)


(** {2 Searching values} *)

val find :
  with_alarms:CilE.warn_mode ->
  validity:Base.validity ->
  conflate_bottom:bool ->
  offsets:Ival.t -> size:Integer.t ->
  t -> v
(** Find the value bound to a set of intervals, expressed as an ival, in the
    given rangemap. *)

val find_imprecise: validity:Base.validity-> t -> v
(** [find_imprecise ~validity m] returns an imprecise join of the values bound
    in [m], in the range described by [validity].  *)

val find_imprecise_everywhere: t -> v
(** Returns an imprecise join of all the values bound in the offsetmap. *)

val copy_slice:
  with_alarms:CilE.warn_mode ->
  validity:Base.validity ->
  offsets:Ival.t -> size:Integer.t ->
  t -> t
(** [copy_slice ~with_alarms ~validity ~offsets ~size m] copies and merges
    the slices of [m] starting at offsets [offsets] and of  size [size].
    Offsets invalid according to [validity] are removed. *)
(* TODOBY: clarify and document return convention *)

(** {2 Adding values} *)

val add : (Int.t * Int.t) -> (v * Int.t * Rel.t) -> t -> t
(** [add (min, max) (v, size, offset) m] maps the interval
    [min..max] (inclusive) to the value [v] in [m]. [v] is assumed as having
    size [size]. If [stop-start+1] is greater than [size], [v] repeats itself
    until the entire interval is filled. [offset] is the offset at which [v]
    starts in the interval, interpreted as for {!iter}. Offsetmaps cannot
    contain holes, so [m] must already bind at least the intervals [0..start-1].
*)

exception Result_is_bottom

val update :
  with_alarms:CilE.warn_mode ->
  validity:Base.validity ->
  exact:bool ->
  offsets:Ival.t ->
  size:Int.t ->
  v ->
  t -> t
(** Can raise [Result_is_bottom] *)

val update_imprecise_everywhere:
  validity:Base.validity ->
  Origin.t -> v ->
  t -> t
(** [update_everywhere ~validity o v m] computes the offsetmap resulting
    from imprecisely writing [v] potentially anywhere where [m] is valid
    according to [validity]. If a value becomes too imprecise, [o] is used
    as origin. *)

val paste_slice:
  with_alarms:CilE.warn_mode ->
  validity:Base.validity ->
  exact:bool ->
  (t * Int.t) (** Source *)->
  size:Int.t ->
  offsets:Ival.t ->
  t -> t


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

(** Clear the caches local to this module. Beware that they are not
    project-aware, and that you must call them at every project switch. *)
val clear_caches: unit -> unit


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
