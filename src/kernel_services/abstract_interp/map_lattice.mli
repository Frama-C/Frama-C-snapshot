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

(** Maps equipped with a lattice structure. *)

module type Value = sig
  include Datatype.S
  val top: t
  val bottom: t
end

(** Complete semi-bounded lattice with over- and under-approximation,
    intersection and difference. No top value. *)
module type Lattice = sig
  include Lattice_type.Bounded_Join_Semi_Lattice
  include Lattice_type.With_Narrow with type t := t
  include Lattice_type.With_Under_Approximation with type t := t
  include Lattice_type.With_Intersects with type t := t
  include Lattice_type.With_Diff with type t := t
end

(** Complete lattice as above, plus a notion of cardinality on the values. *)
module type Lattice_with_cardinality = sig
  include Lattice_type.With_Cardinal_One
  include Lattice_type.With_Diff_One with type t := t
  include Lattice_type.With_Enumeration with type t := t
end

(** A map with a complete lattice structure.  *)
module type Map_Lattice = sig
  include Hptmap_sig.S
  include Lattice with type t := t

  (** [find key t] returns the value bound to [key] in [t], or Value.bottom if
      [key] does not belong to [t].  *)
  val find_or_bottom: key -> t -> v

  (** If [t] is a singleton map binding [k] to [v], then returns the pair (k,v).
      @raise Not_found otherwise. *)
  val find_lonely_key: t -> key * v
end

(** A notion of cardinality for maps with a complete lattice structure. *)
module type Map_Lattice_with_cardinality = sig
  include Lattice_with_cardinality
  type key
  type v

  (** If [t] is a singleton map binding [k] to [v], and if
      [cardinal_zero_or_one v] holds, returns the pair (k,v).
      @raise Not_found otherwise. *)
  val find_lonely_binding: t -> key * v
end

(** A lattice structure on top of maps from keys to values and sets of keys.
    The maps and the sets have their own lattice structure (see
    abstract_interp.ml for the lattice of sets). The sets are implicitly
    considered as maps binding all their keys to top.  Any map is included
    in the set of its keys (and in any larger set). *)
module type MapSet_Lattice = sig
  type set
  type map
  type t = Top of set * Origin.t | Map of map
  include Datatype.S_with_collections with type t := t
  include Lattice with type t := t

  val bottom: t
  val top: t

  type key
  type v

  (** [add key v t] binds the value [v] to [key] in [t]. If [t] is a set, it
      adds [key] to the set. If [v] is bottom, then it removes the [key] from
      [t] instead. *)
  val add: key -> v -> t -> t

  (** [find key t] returns the value bound to [key] in [t]. It returns Value.top
      if [t] is a set that contains [key]. It returns Value.bottom if [key] does
      not belong to [t].  *)
  val find: key -> t -> v

  (** If [t] is a singleton map binding [k] to [v], then returns the pair (k,v).
      @raise Not_found otherwise. *)
  val find_lonely_key: t -> key * v

  (** [split key t] is equivalent to [find key t], [add key bottom t]. *)
  val split : key -> t -> v * t

  (** Returns the singleton map binding [key] to [v]. *)
  val inject : key -> v -> t

  (** Returns the set of keys in [t]. *)
  val get_keys : t -> set

  val filter_keys : (key -> bool) -> t -> t

  val map: (v -> v) -> t -> t

  val fold_keys : (key -> 'a -> 'a) -> t -> 'a -> 'a
  val fold : (key -> v -> 'a -> 'a) -> t -> 'a -> 'a

  val cached_fold:
    cache_name:string -> temporary:bool ->
    f:(key -> v -> 'a) ->
    projection:(key -> v) -> joiner:('a -> 'a -> 'a) -> empty:'a -> t -> 'a

  (** [for_all p t] checks if all binding of [t] satisfy [p] . Always false if
      [t] is top.  *)
  val for_all: (key -> v -> bool) -> t -> bool

  (** [exists p t] checks if one binding of [t] satisfies [p]. Always true if
      [t] is top. *)
  val exists: (key -> v -> bool) -> t -> bool

  val pretty_debug : Format.formatter -> t -> unit
end


(** A notion of cardinality for mapset lattice. *)
module type MapSet_Lattice_with_cardinality = sig
  include Lattice_with_cardinality
  type key
  type v

  (** If [t] is a singleton map binding [k] to [v], and if
      [cardinal_zero_or_one v] holds, returns the pair (k,v).
      @raise Not_found otherwise. *)
  val find_lonely_binding: t -> key * v
end


(** Equips an Hptmap with a lattice structure, provided that the values
    have a lattice structure. *)
module Make_Map_Lattice
    (Key: Hptmap.Id_Datatype)
    (Value : Lattice_type.Full_Lattice)
    (KVMap : Hptmap_sig.S with type key = Key.t
                           and type v = Value.t)
  : sig

    include Map_Lattice with type t = KVMap.t
                         and type key = Key.t
                         and type v = Value.t

    module With_Cardinality
        (Value :
           Lattice_type.Full_AI_Lattice_with_cardinality with type t := Value.t)
      : Map_Lattice_with_cardinality with type t := t
                                      and type key := key
                                      and type v := v

  end


(** Builds a lattice mixing maps and sets, provided that each one has a lattice
    structure. *)
module Make_MapSet_Lattice
    (Key: Hptmap.Id_Datatype)
    (KSet: Lattice_type.Lattice_Set with type O.elt = Key.t)
    (Value : Value)
    (KVMap : Map_Lattice with type key = Key.t
                          and type v = Value.t)
  : sig

    include MapSet_Lattice with type set := KSet.t
                            and type map := KVMap.t
                            and type key := Key.t
                            and type v := Value.t

    module With_Cardinality
        (KVMap : Map_Lattice_with_cardinality with type t := KVMap.t
                                               and type key := Key.t
                                               and type v := Value.t)
      : MapSet_Lattice_with_cardinality with type t := t
                                         and type key := Key.t
                                         and type v := Value.t

  end

