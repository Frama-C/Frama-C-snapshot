(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

type v (** type of the values associated to the locations *)
type offsetmap (** type of the maps associated to a base *)
type widen_hint_y (** widening hints associated to values *)

module LBase :
sig
  type t
  val iter : (Base.base -> offsetmap -> unit) -> t -> unit
end

type tt = private Bottom | Top | Map of LBase.t

include Datatype.S_with_collections with type t = tt

type widen_hint = bool * Base.Set.t * (Base.t -> widen_hint_y)

val add_base :  Base.t -> offsetmap -> t -> t

val pretty_without_null : Format.formatter -> t -> unit
val pretty_filter:
  Format.formatter -> t -> Zone.t -> (Base.t -> bool) -> unit
val pretty_diff:
  Format.formatter -> t -> t -> unit
val add_binding:
  with_alarms:CilE.warn_mode -> exact:bool -> t -> location -> v -> t

val find:
  conflate_bottom:bool -> with_alarms:CilE.warn_mode -> t -> location -> v

val join : t -> t -> t
val is_included : t -> t -> bool

val top: t
val is_top: t -> bool

(** Empty map. Casual users do not need this.*)
val empty_map : t
val is_empty_map : t -> bool

(** Every location is associated to [VALUE.bottom] in [bottom]. This state
    can be reached only in dead code. *)
val bottom : t
val is_reachable : t -> bool

val widen : widen_hint-> t -> t -> (bool * t)

val filter_base : (Base.t -> bool) -> t -> t

(** @raise Not_found if the varid is not present in the map. *)
val find_base : Base.t -> t -> offsetmap

val find_base_or_default : Base.t -> t -> offsetmap

(** Removes the base if it is present. Does nothing otherwise. *)
val remove_base : Base.t -> t -> t

val reduce_previous_binding :
  with_alarms:CilE.warn_mode -> t -> location -> v -> t
val reduce_binding :
  with_alarms:CilE.warn_mode -> t -> location -> v -> t

(** [paste_offsetmap ~from:offmap ~dst_loc ~start ~size ~exact m]
    copies [size] bits starting at [start] in [offmap], and pastes
    them at [dst_loc] in [m]. The copy is exact if and only if
    [dst_loc] is exact, and [exact is true] *)
val paste_offsetmap :
  with_alarms:CilE.warn_mode ->
  from:offsetmap ->
  dst_loc:Location_Bits.t ->
  start:Integer.t ->
  size:Integer.t ->
  exact:bool ->
  t -> t

(** [copy_offsetmap alarms loc m] returns the superposition of the
    bits pointed to by [loc] within [m]. [loc.size] must not be top.
    Return [None] if all pointed adresses are invalid in [m]. *)
val copy_offsetmap :
  with_alarms:CilE.warn_mode -> location -> t -> offsetmap option

(** [fold_base f m] calls [f] on all bases bound to non top
    offsetmaps in the non bottom map [m].
    @raise Error_Bottom if [m] is bottom. *)
val fold_base : (Base.t -> 'a -> 'a) -> t -> 'a -> 'a

(** [fold_base_offsetmap f m] calls [f] on all bases bound to non
    top offsetmaps in the non bottom map [m].
    @raise Error_Bottom if [m] is bottom.*)
val fold_base_offsetmap : (Base.t -> offsetmap -> 'a -> 'a) -> t -> 'a -> 'a

val add_new_base:
  Base.t ->
  size:Integer.t ->
  v -> size_v:Integer.t ->
  t -> t

exception Error_Bottom

(** Cached iterators *)

val cached_fold :
  f:(Base.t -> offsetmap -> 'a) ->
  cache:string * int -> temporary:bool ->
  joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a

val cached_map :
  f:(Base.t -> offsetmap -> offsetmap) ->
  cache:string * int -> temporary:bool ->
  t -> t


(** Prefixes. To be used by advanced users only *)

type subtree
val comp_prefixes: t -> t -> unit
val find_prefix : t -> Hptmap.prefix -> subtree option
val hash_subtree : subtree -> int
val equal_subtree : subtree -> subtree -> bool

exception Found_prefix of Hptmap.prefix * subtree * subtree


(** Clear the caches local to this module. Beware that they are not
    project-aware, and that you must call them at every project switch. *)
val clear_caches: unit -> unit



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
