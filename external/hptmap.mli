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

module type Tagged_type = sig
  include Datatype.S
  val tag : t -> int
end

module Tag_comp : 
sig
  type t
  val get_tag : t -> int
  val get_comp : t -> bool
  val encode : int -> bool -> t
end

module Comp_unused :
sig 
  val e : bool
  val f : 'a -> 'b -> bool
  val compose : bool -> bool -> bool
  val default : bool
end

type prefix
val sentinel_prefix : prefix

module Make
  (Key:sig
    include Datatype.S
    val id: t -> int
  end)
  (V : Tagged_type)
  (Comp : sig val e: bool val f : Key.t -> V.t -> bool val compose : bool -> bool -> bool val default:bool end)
  (Initial_Values : sig val v : (Key.t*V.t) list list end) 
  (Datatype_deps: sig val l : State.t list end)
  :
sig

  type key = Key.t
  type leaf_annot = bool
  type branch_annot = Tag_comp.t

  type tt = private
	    | Empty
	    | Leaf of key * V.t * bool
	    | Branch of int * int * tt * tt * Tag_comp.t

  include Datatype.S with type t = tt

  val self : State.t 

  val empty : t

(* the tag is no longer guaranteed to uniquely identify a Patricia tree, so
this function will be renamed "hash" in the future *)
  val tag : t -> int
  val hash_debug : t -> int

  val is_empty : t -> bool
  val comp : t -> bool

  val add : key -> V.t -> t -> t
  val find : key -> t -> V.t
  val remove : key -> t -> t
  val mem :  key -> t -> bool
  val iter : (Key.t -> V.t -> unit) -> t -> unit

  val map : (V.t -> V.t) -> t -> t
  (*val mapi : (int -> 'a -> 'b) -> t -> 'b t*)

  val fold : (Key.t -> V.t -> 'b -> 'b) -> t -> 'b -> 'b

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
    decide_one:(Key.t -> V.t  -> unit) ->
    decide_both:(V.t -> V.t -> unit) -> t -> t -> unit

  val cached_fold :
    cache:string * int ->
    temporary:bool ->
    f:(key -> V.t -> 'b) ->
    joiner:('b -> 'b -> 'b) -> empty:'b -> t -> 'b

  val cached_map :
    cache:string * int ->     temporary:bool ->
    f:(key -> V.t -> V.t) -> t -> t

  val is_singleton: t -> (key * V.t) option

  val min_binding: t -> key * V.t
  val max_binding: t -> key * V.t

  val split: key -> t -> t * V.t option * t
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
