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
  val is_zero_range : sigma -> loc -> c_object -> term -> term -> pred

  val instance_of : loc -> kernel_function -> pred

end
