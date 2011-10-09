(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(** Model for the interpretation of ACSL/C                                    *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Clabels
open Formula
open Cil_types

module type S =
sig

  (** {2 Term Values} *)

  include Mvalues.S
    
  val lvar : mem -> logic_var -> F.var-> loc
    (** [lvar m lv p] returns a location associated to the 
        location variable [lv] with variable root name [x].
        [x] for all model except in funvar. *)

  val inner_loc : loc -> F.abstract
    (** [inner_loc l] returns the location corresponding
	to [l] in the inner memory model in funvar.
	Not implemented in other models. *)
   

  (** {2 Pointers} *)

  val base_address : mem -> loc -> loc
    (** [base_address m l] return the [base address] of [l].*)

  val block_length : mem -> loc -> F.integer
    (** [block_length m l t] return the [block_length]
        of the location [l]. *)

  (** {2 Validity }*)

  val valid : mem -> loc F.assigned -> F.pred

  (** {2 Separation} *)

  val separated : mem -> loc F.assigned -> loc F.assigned -> F.pred

  (** {2 User-defined Predicates} *)

  type formal
  val pp_formal : Format.formatter -> ( formal * logic_var ) -> unit

  val userdef_ref_has_cvar : logic_var -> bool
    (** [userdef_ref_has_cvar p] tests if the by reference 
	logic formal parameter [p] needs a C addresses.*)

  val userdef_is_ref_param  : logic_var -> bool
    (** [userdef_ref_param p] tests if [p] is a by reference logic 
        formal parameter.*)

  val userdef_ref_signature : mem -> ( F.var * logic_var * formal ) list
  val userdef_ref_apply : mem -> formal -> loc -> value

  type closure
  val pp_closure : Format.formatter -> closure -> unit

  val userdef_mem_signature : mem -> ( F.var * closure ) list
  val userdef_mem_apply : mem -> closure -> F.abstract

end
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
