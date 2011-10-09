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
(** Integer Model                                                             *)
(* -------------------------------------------------------------------------- *)

open Formula
open Ctypes

module type S =
sig

  module F : Formula.S

  (** {2 Integer Operators} *)

  val i_neg : c_int -> F.integer -> F.integer
  val i_op  : c_int -> int_op -> F.integer -> F.integer -> F.integer
  val i_cmp : c_int -> cmp_op -> F.integer -> F.integer -> F.boolean

  (** {2 Bitwise Operators} *)

  val bits_not    : c_int -> F.integer -> F.integer
  val bits_and    : c_int -> F.integer -> F.integer -> F.integer
  val bits_or     : c_int -> F.integer -> F.integer -> F.integer
  val bits_xor    : c_int -> F.integer -> F.integer -> F.integer
  val bits_lshift : c_int -> F.integer -> F.integer -> F.integer
  val bits_rshift : c_int -> F.integer -> F.integer -> F.integer

end
