(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Cil_types
open Region

(* -------------------------------------------------------------------------- *)

val cc_lval : map -> lval -> region
val cc_read : map -> exp -> unit
val cc_assign : map -> stmt -> lval -> exp -> unit
val cc_init : map -> stmt -> lval -> init -> unit
val cc_instr : map -> stmt -> instr -> unit
val cc_fundec : map -> fundec -> unit

val cc_pred : map -> predicate -> unit
val cc_term : map -> term -> unit
val cc_spec : map -> spec -> unit

open RegionAnnot
val cc_region : map -> region_spec -> unit

(* -------------------------------------------------------------------------- *)
