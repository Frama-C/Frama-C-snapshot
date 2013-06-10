(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Menhir                               *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in the file licences/Q_MODIFIED_LICENSE.             *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Undocumented. *)

exception Found_inter

module Tag_comp : sig
  type t
  val get_tag : t -> int
  val get_comp : t -> bool
  val encode : int -> bool -> t
end

module Comp_unused : sig 
  val e : bool
  val f : 'a -> 'b -> bool
  val compose : bool -> bool -> bool
  val default : bool
end

type prefix
val sentinel_prefix : prefix

type ('k, 'v, 't) tree = private
    | Empty
    | Leaf of 'k * 'v * bool
    | Branch of int * int * ('k, 'v, 't) tree * ('k, 'v, 't) tree * 't

module Make
  (Key:sig
    include Datatype.S
    val id: t -> int
  end)
  (V : Datatype.S)
  (Comp : sig
     val e: bool
     val f : Key.t -> V.t -> bool
     val compose : bool -> bool -> bool
     val default:bool
   end)
  (Initial_Values : sig val v : (Key.t*V.t) list list end) 
  (Datatype_deps: sig val l : State.t list end)
  :
sig

  type key = Key.t
  type leaf_annot = bool
  type branch_annot = Tag_comp.t

  type tt = (Key.t, V.t, Tag_comp.t) tree

  include Datatype.S_with_collections with type t = tt

  val self : State.t 

  val empty : t

(* the tag is no longer guaranteed to uniquely identify a Patricia tree, so
this function will be renamed "hash" in the future *)
  val tag : t -> int
  val hash_debug : t -> int

  val is_empty : t -> bool
  (** [is_empty m] returns [true] if and only if the map [m] defines no
      bindings at all. *)

  val add : key -> V.t -> t -> t
  (** [add k d m] returns a map whose bindings are all bindings in [m], plus
      a binding of the key [k] to the datum [d]. If a binding already exists
      for [k], it is overridden. *)

  val find : key -> t -> V.t
  val remove : key -> t -> t
  (** [remove k m] returns the map [m] deprived from any binding involving
      [k]. *)

  val mem :  key -> t -> bool
  val iter : (Key.t -> V.t -> unit) -> t -> unit

  val map : (V.t -> V.t) -> t -> t
  (** [map f m] returns the map obtained by composing the map [m] with the
      function [f]; that is, the map $k\mapsto f(m(k))$. *)

  val fold : (Key.t -> V.t -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold f m seed] invokes [f k d accu], in turn, for each binding from
      key [k] to datum [d] in the map [m]. Keys are presented to [f] in
      increasing order according to the map's ordering. The initial value of
      [accu] is [seed]; then, at each new call, its value is the value
      returned by the previous invocation of [f]. The value returned by
      [fold] is the final value of [accu]. *)

  val fold_rev : (Key.t -> V.t -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold_rev] performs exactly the same job as [fold], but presents keys
      to [f] in the opposite order. *)


  val comp_prefixes : t -> t -> unit
  val pretty_prefix : prefix -> Format.formatter -> t -> unit

  type subtree
  exception Found_prefix of prefix * subtree * subtree
  val find_prefix : t -> prefix -> subtree option
  val hash_subtree : subtree -> int
  val equal_subtree : subtree -> subtree -> bool

  val generic_merge : cache:(string * int) ->
    decide:(Key.t -> V.t option -> V.t option -> V.t) -> t -> t -> t

  val symetric_merge : cache:(string * int) ->
    decide_none:(Key.t -> V.t -> V.t) ->
      decide_some:(V.t -> V.t -> V.t) -> t -> t -> t

  val generic_is_included : exn -> cache:(string * int) ->
    decide_fst:(Key.t -> V.t  -> unit) ->
    decide_snd:(Key.t -> V.t  -> unit) ->
    decide_both:(V.t -> V.t -> unit) -> t -> t -> unit

  val generic_symetric_existential_predicate : exn -> 
    (t -> t -> bool) ->
    decide_one:(Key.t -> V.t  -> unit) ->
    decide_both:(V.t -> V.t -> unit) -> t -> t -> unit

  val do_it_intersect : t -> t -> bool

  val cached_fold :
    cache:string * int ->
    temporary:bool ->
    f:(key -> V.t -> 'b) ->
    joiner:('b -> 'b -> 'b) -> empty:'b -> t -> 'b

  val cached_map :
    cache:string * int ->     temporary:bool ->
    f:(key -> V.t -> V.t) -> t -> t

  val singleton: key -> V.t -> t
  (** [singleton k d] returns a map whose only binding is from [k] to [d]. *)

  val is_singleton: t -> (key * V.t) option
  (** [is_singleton m] returns [Some (k, d)] if [m] is a singleton map
      that maps [k] to [d]. Otherwise, it returns [None]. *)

  val cardinal: t -> int
  (** [cardinal m] returns [m]'s cardinal, that is, the number of keys it
      binds, or, in other words, its domain's cardinal. *)

  val min_binding: t -> key * V.t
  val max_binding: t -> key * V.t

  val split: key -> t -> t * V.t option * t

  (** Clear all the caches used internally by the functions of this module.
      Those caches are not project-aware, so this function must be called
      at least each a project switch occurs. *)
  val clear_caches: unit -> unit
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
