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

open Lang.F
open Sigs

(* -------------------------------------------------------------------------- *)
(* --- L-Val Utility                                                      --- *)
(* -------------------------------------------------------------------------- *)

val index : s_lval -> term -> s_lval
val field : s_lval -> Cil_types.fieldinfo -> s_lval
val equal : s_lval -> s_lval -> bool

(* -------------------------------------------------------------------------- *)
(* --- Memory State Pretty Printing Information                           --- *)
(* -------------------------------------------------------------------------- *)

type 'a model
type state

val create : (module Model with type Sigma.t = 'a) -> 'a model
val state : 'a model -> 'a -> state

val lookup : state -> term -> mval
val apply : (term -> term) -> state -> state
val iter : (mval -> term -> unit) -> state -> unit
val updates : state sequence -> Vars.t -> update Bag.t
