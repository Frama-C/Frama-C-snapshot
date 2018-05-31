(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
(* --- Typed Memory Model                                                 --- *)
(* -------------------------------------------------------------------------- *)

include Sigs.Model

type pointer = NoCast | Fits | Unsafe
val pointer : pointer Context.value
val p_havoc : Lang.lfun
val p_separated : Lang.lfun
val p_included : Lang.lfun
val p_valid_rd : Lang.lfun
val p_valid_rw : Lang.lfun
val p_invalid : Lang.lfun
val a_base : Lang.F.term -> Lang.F.term
val a_offset : Lang.F.term -> Lang.F.term
