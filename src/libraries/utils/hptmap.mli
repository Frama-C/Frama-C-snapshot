(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Menhir                               *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in the file licenses/Q_MODIFIED_LICENSE.             *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Efficient maps from hash-consed trees to values, implemented as
    Patricia trees. *)

(** This implementation of big-endian Patricia trees follows Chris
    Okasaki's paper at the 1998 ML Workshop in Baltimore.  Maps are
    implemented on top of Patricia trees. A tree is big-endian if it
    expects the key's most significant bits to be tested first. *)


(**/**) (* Undocumented. Needed for advanced users only *)
type prefix
val sentinel_prefix : prefix

(**/**)

type tag

(** Type of the keys of the map. *)
module type Id_Datatype = sig
    include Datatype.S
    val id: t -> int (** Identity of a key. Must verify [id k >= 0] and
                         [equal k1 k2 ==> id k1 = id k2] *)
end

(** Values stored in the map *)
module type V = sig
  include Datatype.S
  val pretty_debug: t Pretty_utils.formatter
end

(** This functor exports the {i shape} of the maps indexed by keys [Key].
    Those shapes can be used by various functions to efficiently build
    new maps whose shape are already known. *)
module Shape (Key : Id_Datatype): sig
  type 'value t
end

module Make
  (Key : Id_Datatype)
  (V : V)
  (Compositional_bool : sig
     (** A boolean information is maintained for each tree, by composing the
         boolean on the subtrees and the value information present on each leaf.
         See {!Comp_unused} for a default implementation. *)

     val e: bool  (** Value for the empty tree *)
     val f : Key.t -> V.t -> bool  (** Value for a leaf *)
     val compose : bool -> bool -> bool
       (** Composition of the values of two subtrees *)
     val default:bool
   end)
  (Initial_Values : sig
    val v : (Key.t*V.t) list list
    (** List of the maps that must be shared between all instances of Frama-C
        (the maps being described by the list of their elements).
        Must include all maps that are exported at Caml link-time when the
        functor is applied. This usually includes at least the empty map, hence
        [v] nearly always contains [[]]. *)
  end)
  (Datatype_deps: sig
    val l : State.t list
    (** Dependencies of the hash-consing table. The table will be cleared
        whenever one of those dependencies is cleared. *)
  end)
  : Hptmap_sig.S with type key = Key.t
                  and type v = V.t
                  and type 'a shape = 'a Shape(Key).t
                  and type prefix = prefix

(** Default implementation for the [Compositional_bool] argument of the functor
  {!Make}. To be used when no interesting compositional bit can be computed. *)
module Comp_unused : sig
  val e : bool
  val f : 'a -> 'b -> bool
  val compose : bool -> bool -> bool
  val default : bool
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
