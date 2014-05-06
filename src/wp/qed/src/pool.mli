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
(* ---     Variable Manegement                                            --- *)
(* -------------------------------------------------------------------------- *)

open Hcons
open Logic

module type Type =
sig
  type t
  val dummy : t
  val equal : t -> t -> bool
end

module Make(T : Type) :
sig

  type var = (** Hashconsed *)
    private {
    vid : int ;
    vbase : string ;
    vrank : int ;
    vtau : T.t ;
  }

  val dummy : var (** null vid *)

  val hash : var -> int (** [vid] *)
  val equal : var -> var -> bool (** [==] *)
  val compare : var -> var -> int
  val pretty : Format.formatter -> var -> unit

  type pool
  val create : ?copy:pool -> unit -> pool
  val add : pool -> var -> unit
  val fresh : pool -> string -> T.t -> var
  val alpha : pool -> var -> var

end
