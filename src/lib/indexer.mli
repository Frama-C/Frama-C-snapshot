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

(** Indexer implements ordered collection of items with 
    random access. It is suitable for building fast access operations
    in GUI tree and list widgets. *) 

module type Elt = sig
  type t
  val compare : t -> t -> int
end

module Make(E : Elt) : sig

  type t

  val size : t -> int
  (** Number of elements in the collection. Constant time. *)

  val mem : E.t -> t -> bool (** Log complexity. *)
  val get : int -> t -> E.t (** raises Not_found. Log complexity. *)
  val index : E.t -> t -> int (** raise Not_found. Log complexity. *)

  val empty : t 
  val add : E.t -> t -> t (** Log complexity. *)
  val remove : E.t -> t -> t (** Log complexity. *)
  val filter : (E.t -> bool) -> t -> t (** Linear. *)
  val update : E.t option -> E.t option -> t -> int * int * t
    (** [update x y t] replaces [x] by [y] 
	and returns the range [a..b] of modified indices. 
	Log complexity. *)

  val iter : (E.t -> unit) -> t -> unit (** Linear. *)
  val iteri : (int -> E.t -> unit) -> t -> unit (** Linear. *)

end
