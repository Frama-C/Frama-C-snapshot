(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Signature for maps from bases to memory maps. The memory maps are intended
    to be those of the [Offsetmap] module. *)

open Locations

type v (** type of the values associated to a location *)
type offsetmap (** type of the maps associated to a base *)
type widen_hint_base (** widening hints for each base *)

type map (** Maps from {!Base.t} to {!offsetmap} *)
type lmap = private Bottom | Top | Map of map

include Datatype.S_with_collections with type t = lmap

val pretty: Format.formatter -> t -> unit
val pretty_debug: Format.formatter -> t -> unit
val pretty_filter: Format.formatter -> t -> Zone.t -> unit
(** [pretty_filter m z] pretties only the part of [m] that correspond to
    the bases present in [z] *)
val pretty_diff: Format.formatter -> t -> t -> unit

(** {2 General shape} *)

val top: t
val is_top: t -> bool

(** Empty map. Casual users do not need this except to create a custom
    initial state. *)
val empty_map : t
val is_empty_map : t -> bool

val bottom : t
(** Every location is associated to the value [bottom] of type [v] in this
    state. This state can be reached only in dead code. *)
val is_reachable : t -> bool


(** {2 Join and inclusion} *)

val join : t -> t -> t
val is_included : t -> t -> bool

module Make_Narrow(X: sig
    include Lattice_type.With_Top with type t := v
    include Lattice_type.With_Narrow with type t := v
    val bottom_is_strict: bool
  end): sig
  val narrow : t -> t -> t
end

(** Bases that must be widening in priority, plus widening hints for each
    base. *)
type widen_hint = Base.Set.t * (Base.t -> widen_hint_base)

val widen : widen_hint-> t -> t -> t


(** {2 Finding values} *)

val find: ?conflate_bottom:bool -> t -> location -> v
(** @raise Error_Top when the location or the state are Top, and there
    is no Top value in the type {!v}. *)

val copy_offsetmap :
  Location_Bits.t -> Integer.t -> t -> offsetmap Bottom.or_bottom
(** [copy_offsetmap alarms loc size m] returns the superposition of the
    ranges of [size] bits starting at [loc] within [m]. [size] must be
    strictly greater than zero. Return [None] if all pointed addresses are
    invalid in [m].

    @raise Error_Top when the location or the state are Top, and there
    is no Top value in the type {!v}. *)

val find_base : Base.t -> t -> offsetmap Bottom.Top.or_top_bottom
(** @raise Not_found if the varid is not present in the map. *)

val find_base_or_default : Base.t -> t -> offsetmap Bottom.Top.or_top_bottom
(** Same as [find_base], but return the default values for bases
    that are not currently present in the map. Prefer the use of this function
    to [find_base], unless you explicitly want to see if the base is bound. *)


(** {2 Binding variables} *)

(** [add_binding ~exact initial_mem loc v] simulates the effect of
    writing [v] at location [loc], in the initial memory state given by
    [initial_mem].
    If [loc] is not writable, {!bottom} is returned.
    If [exact] is true, and [loc] is a precise location, a strong update
    is performed.
    Only locations that may be valid are written.
    Returns the resulting memory after the write. *)
val add_binding: exact:bool -> t -> location -> v -> t

(** [paste_offsetmap ~from ~dst_loc ~size ~exact m]
    copies [from], which is supposed to be exactly [size] bits, and pastes
    them at [dst_loc] in [m]. The copy is exact if and only if
    [dst_loc] is exact, and [exact] is true. Only the locations that
    may be valid are written. *)
val paste_offsetmap :
  from:offsetmap ->
  dst_loc:Location_Bits.t ->
  size:Integer.t ->
  exact:bool ->
  t -> t

val add_base :  Base.t -> offsetmap -> t -> t
(** [add_base b o m] adds base [b] bound to [o], replacing any
    previous bindings of [b]. No effect on [Top] or [Bottom]. *)

val add_base_value:
  Base.t -> size:Integer.t -> v -> size_v:Integer.t -> t -> t
(** Creates the offsetmap described by [size], [v] and [size_v],
    and binds it to the base. No effect on [Top] or [Bottom]. *)


(** {2 Filters} *)

val filter_base : (Base.t -> bool) -> t -> t
(** Remove from the map all the bases that do not satisfy the predicate. *)

val filter_by_shape: 'a Hptmap.Shape(Base.Base).t -> t -> t
(** Remove from the map all the bases that are not also present in
    the given [Base.t]-indexed tree. *)

(** Removes the base if it is present. Does nothing otherwise. *)
val remove_base : Base.t -> t -> t


(** {2 Iterators} *)

(** Notice that some iterators require an argument of type {!map}: the
    cases {!Top} and {!Bottom} must be handled separately. All the iterators
    below only present bases that are bound to non-default values, according
    to the function [is_default_offsetmap] of the function {!Lmap.Make_Loffset}.
*)

val iter:  (Base.base -> offsetmap -> unit) -> map -> unit
val fold : (Base.t -> offsetmap -> 'a -> 'a) -> map -> 'a -> 'a

(** {3 Cached iterators} *)

(** These functions are meant to be partially applied to all their arguments
    but the final one (the map). They must be called at the toplevel of OCaml
    modules, as they create persistent caches. *)

val cached_fold :
  f:(Base.t -> offsetmap -> 'a) ->
  cache_name:string -> temporary:bool ->
  joiner:('a -> 'a -> 'a) -> empty:'a -> map -> 'a

val cached_map :
  f:(Base.t -> offsetmap -> offsetmap) ->
  cache:string * int -> temporary:bool ->
  t -> t


(** {2 Misc} *)

val remove_variables: Cil_types.varinfo list -> t -> t

val shape: map -> offsetmap Hptmap.Shape(Base.Base).t
(** Shape of the map. This can be used for simultaneous iterations
    on other maps indexed by type {!Base.Base.t}. *)

(** Clear the caches local to this module. Beware that they are not
    project-aware, and that you must call them at every project switch. *)
val clear_caches: unit -> unit


(**/**)

(** {2 Prefixes. To be used by advanced users only} *)

type subtree
val comp_prefixes: t -> t -> unit
val find_prefix : t -> Hptmap.prefix -> subtree option
val hash_subtree : subtree -> int
val equal_subtree : subtree -> subtree -> bool

exception Found_prefix of Hptmap.prefix * subtree * subtree




(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
