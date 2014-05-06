(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(** Generalized Substitutions *)

module type S =
sig
  type t (** Substitution *)
  type term (** Terms *)
  type explain (** Explanations *)

  exception Contradiction of explain

  val empty : t

  val assume : ?explain:explain -> term -> t -> t 
  (** Raises [Contradiction]. *)

  val rewrite : ?explain:explain -> term -> term -> t -> t
  (** Raises [Contradiction]. *)

  val reduce : t -> term -> term * explain
  (** Produces a normalized form, with its explanation. *)

  val is_true : t -> term -> explain option
  (** Checks whether [reduce] returns [e_true] and returns the explanation. *)

  val is_false : t -> term -> explain option
  (** Checks whether [reduce] returns [e_false] and returns the explanation. *)

  val iter : (term -> term -> explain -> unit) -> t -> unit
  (** Iterates over all core equalities. *)

end

(** Type of Explanations *)
module type Explain =
sig
  type t
  val bot : t 
  val cup : t -> t -> t
end

(** Substitution Factory *)
module Make(T : Logic.Term)(E : Explain) : S 
  with type term = T.term and type explain = E.t
