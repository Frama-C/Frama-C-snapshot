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
