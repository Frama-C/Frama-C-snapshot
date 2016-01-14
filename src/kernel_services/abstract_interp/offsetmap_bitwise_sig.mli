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

(** Signature for {!Offsetmap_bitwise} module, that implement efficient maps
    from intervals to values.

    Values are simpler than those of the {!Offsetmap_sig} module: given a value
    [v] bound to an interval [i], all sub-intervals of [i] are implicitly also
    bound to [v]. If you need e.g. to extract the k-th bit of the interval to
    retrieve a more precise value, you must use the {!Offsetmap} module
    instead. *)


type v (** Type of the values stored in the offsetmap *)
include Datatype.S (** Datatype for the offsetmap *)

type intervals

(** {2 Pretty-printing} *)

val pretty: t Pretty_utils.formatter
val pretty_generic :
  ?typ:Cil_types.typ ->
  ?pretty_v:(Format.formatter -> v -> unit) ->
  ?skip_v:(v -> bool) ->
  ?sep:string ->
  unit ->
  Format.formatter -> t -> unit

val pretty_debug: t Pretty_utils.formatter


(** {2 Join and inclusion testing} *)

val join : t -> t -> t
val is_included : t -> t -> bool


(** {2 Finding values} *)

val find : Int_Intervals_sig.itv -> t -> v
val find_iset : validity:Base.validity -> intervals -> t -> v


(** {2 Adding values} *)

val add_binding_intervals :
  validity:Base.validity -> exact:bool -> intervals -> v -> t -> [`Map of t | `Bottom]

val add_binding_ival :
  validity:Base.validity ->
  exact:bool -> Ival.t -> size:Int_Base.t -> v -> t -> [`Map of t | `Bottom]


(** {2 Creating an offsetmap} *)

(** [size] must be strictly greater than zero. *)
val create: size:Integer.t -> v -> t


(** {2 Iterators} *)

val map : (v -> v) -> t -> t

type map2_decide =
   ReturnLeft | ReturnRight | ReturnConstant of v | Recurse
(** See the documentation of type {!Offsetmap_sig.map2_decide} *)

val map2:
  Hptmap_sig.cache_type -> (t -> t -> map2_decide) -> (v -> v -> v) -> t -> t -> t
(** See the documentation of function {!Offsetmap_sig.map2_on_values}. *)


val fold :          (intervals -> v -> 'a -> 'a) -> t -> 'a -> 'a
val fold_fuse_same: (intervals -> v -> 'a -> 'a) -> t -> 'a -> 'a
(** Same behavior as [fold], except if two disjoint intervals [r1] and [r2]
    are mapped to the same value and boolean. In this case, [fold] will call
    its argument [f] on [r1], then on [r2]. [fold_fuse_same] will call it
    directly on [r1 U r2], where U is the join on sets of intervals. *)

val fold_itv:
  ?direction:[`LTR | `RTL] ->
  entire:bool ->
  (Int_Intervals_sig.itv -> v -> 'a -> 'a) ->
  Int_Intervals_sig.itv ->
  t -> 'a -> 'a
(** See documentation of {!Offsetmap_sig.fold_between}. *)

(** [fold_join f join vempty itvs m] is an implementation of [fold] that
    restricts itself to the intervals in [itvs]. Unlike in [fold] (where the
    equivalent of [f] operates on an accumulator), [f] returns a value on each
    sub-interval independently. The results are joined using [joined].
    [vempty] is the value that must be returned on {!Int_Intervals.bottom}.
    This function uses a cache internally. Hence, it must be partially
    applied to its first three arguments. If you do not need a cache, use
    [fold] instead. *)
val fold_join_itvs:
  cache:Hptmap_sig.cache_type ->
  (Integer.t -> Integer.t -> v -> 'a) ->
  ('a -> 'a -> 'a) ->
  'a ->
  intervals -> t -> 'a


(** {2 Shape} *)

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


(**/**)

val imprecise_write_msg: string ref
