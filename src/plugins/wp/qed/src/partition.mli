(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(** Union-find based partitions *)

module type S =
sig
  type t (** Partitions *)
  type elt (** Elements *)
  type explain (** Explanations *)

  val empty : t
  val join : ?explain:explain -> elt -> elt -> t -> t (** Immediate. *)
  val class_of : t -> elt -> elt (** Amortized. *)
  val is_equal : t -> elt -> elt -> bool
  (** Returns [true] is the two elements are in the same class. *)
  val members : t -> elt -> elt list
  (** All members, including self. Explanations can be recover from [explain]. *)


  val repr : t -> elt -> elt * explain
  (** Returns [class_of] with explaination *)
  val equal : t -> elt -> elt -> explain option
  (** Returns [Some e] if equal with explanation [e]. Amortized. *)
  val explain : t -> elt -> elt -> explain
  (** Returns [e] is [equal] returns [Some e], and [bot] otherwise. *)

  val iter : (elt -> elt list -> unit) -> t -> unit (** Including selves. *)
  val map : (elt -> elt) -> t -> t (** Rebuild all the classes. *)
end

(** Type of Explanations *)
module type Explain =
sig
  type t
  val bot : t
  val cup : t -> t -> t
end

(** Partitions without Explanations *)
module Make(A : Map.OrderedType) : S
  with type elt = A.t and type explain = unit

(** Partitions with Explanations *)
module MakeExplain(A : Map.OrderedType)(E : Explain) : S
  with type elt = A.t and type explain = E.t
