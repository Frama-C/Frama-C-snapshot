(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

open Logic

(** Dependencies *)

module Make(T : Term) :
sig

  open T

  (** {3 Set} *)

  type depends
  val depends : unit -> depends
  val union : depends -> depends -> depends
  val subset : depends -> depends -> bool

  (** {3 Iterators} *)

  val iter_types : (ADT.t -> unit) -> depends -> unit
  val iter_fields : (Field.t -> unit) -> depends -> unit
  val iter_functions : (Fun.t -> unit) -> depends -> unit
  val iter_predicates : (Fun.t -> unit) -> depends -> unit

  val mem_type : depends -> ADT.t -> bool
  val mem_field : depends -> Field.t -> bool
  val mem_function : depends -> Fun.t -> bool
  val mem_predicate : depends -> Fun.t -> bool

  (** {3 Accumulators} *)

  val add_type : depends -> ADT.t -> unit
  val add_field : depends -> Field.t -> unit
  val add_function : depends -> Fun.t -> unit
  val add_predicate : depends -> Fun.t -> unit
  val add_depend : target:depends -> source:depends -> unit

  val add_tau : depends -> tau -> unit
  val add_term : depends -> term -> unit
  val add_prop : depends -> term -> unit

  (** {3 Topological Sort} *)

end

