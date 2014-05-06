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

(* -------------------------------------------------------------------------- *)
(* --- Letifications                                                      --- *)
(* -------------------------------------------------------------------------- *)

open Lang.F

module Sigma :
sig
  type t
  val equal : t -> t -> bool
  val pretty : string -> Format.formatter -> t -> unit
  val e_apply : t -> term -> term
  val p_apply : t -> pred -> pred
  val empty : t
  val add : var -> term -> t -> t
  val assume : t -> pred -> t
  val find : var -> t -> term
  val iter : (var -> term -> unit) -> t -> unit
  val domain : t -> Vars.t
  val codomain : t -> Vars.t
end

module Defs :
sig
  type t
  val empty : t
  val merge : t -> t -> t
  val extract : pred -> t
  val domain : t -> Vars.t 
end

val bind : Sigma.t -> Defs.t -> Vars.t -> Sigma.t
  (** [bind sigma defs xs] select definitions in [defs]
      targeting variables [xs]. The result is a new substitution that
      potentially augment [sigma] with definitions for [xs] (and others). *)

val add_definitions : Sigma.t -> Defs.t -> Vars.t -> pred list -> pred list
  (** [add_definitions sigma defs xs ps] keep all
      definitions of variables [xs] from [sigma] that comes from [defs].
      They are added to [ps]. *)

(** Pruning strategy ; selects most occuring literals to split cases. *)
module Split :
sig
	
  type occur
    
  val create : unit -> occur
  val add : occur -> pred -> unit
  val select : occur -> (pred * int) list

end
