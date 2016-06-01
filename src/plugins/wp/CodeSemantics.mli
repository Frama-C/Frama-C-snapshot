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
(* --- C-Code Translation                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Lang.F

module Make(M : Memory.Model) :
sig

  open M

  type loc = M.loc
  type value = loc Memory.value
  type sigma = Sigma.t

  val cval : value -> term
  val cloc : value -> loc

  val cast : typ -> typ -> value -> value
  val equal_typ : typ -> value -> value -> pred
  val equal_obj : c_object -> value -> value -> pred

  val exp : sigma -> exp -> value
  val cond : sigma -> exp -> pred
  val lval : sigma -> lval -> loc

  val call : sigma -> exp -> loc
  val loc_of_exp : sigma -> exp -> loc
  val val_of_exp : sigma -> exp -> term

  val return : sigma -> typ -> exp -> term

  val is_zero : sigma -> c_object -> loc -> pred
  val is_exp_range :
    sigma -> loc -> c_object -> term -> term ->
    value option -> (** None means equal to zero/null *)
    pred

  val instance_of : loc -> kernel_function -> pred

end
