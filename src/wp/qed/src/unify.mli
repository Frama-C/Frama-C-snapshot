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

(* -------------------------------------------------------------------------- *)
(** Datatypes Unifier                                                         *)
(* -------------------------------------------------------------------------- *)

open Logic

module Make(ADT:Data)(Field:Field) : 
sig

  type mgu
  type t
  type tau = (Field.t,ADT.t) datatype
  type signature = (Field.t,ADT.t) funtype
 
  val create : (ADT.t -> tau option) -> mgu
  val fresh  : mgu -> t

  val int : t
  val real : t
  val bool : t
  val prop : t
  val quoted : mgu -> string -> t
  val array : t -> t -> t
  val record : (Field.t * t) list -> t
  val data : ADT.t -> t list -> t
  val typedef : t array -> tau -> t

  val of_tau : mgu -> tau -> t
  val of_sig : mgu -> signature -> t * t list

  val unify  : mgu -> t -> t -> unit
  val sort   : mgu -> t -> sort
  val fields : mgu -> t -> (Field.t * t) list

  val generalize : mgu -> t -> tau
  val final_degree : mgu -> int
    (** Number of polymorphic variables yet computed by [generalize] *)

  val pretty : mgu -> Format.formatter -> t -> unit

end
