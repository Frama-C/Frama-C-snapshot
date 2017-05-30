(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
(** Term Footprints *)
(* -------------------------------------------------------------------------- *)

open Lang.F

(** Width-first full iterator. *)
val iter : (term -> unit) -> term -> unit

(** Width-first once iterator. *)
val once : (term -> unit) -> term -> unit

(** Head only footprint *)
val head : term -> string

(** Generate head footprint up to size *)
val pattern : term -> string

(** Head match *)
val matches : string -> term -> bool

(** [k]-th occurrence of the footprint in a term *)
type occurrence = int * string

(** Locate the occurrence of [select] footprint inside a term. *)
val locate : select:term -> inside:term -> occurrence

(** Retrieve back the [k]-th occurrence of a footprint inside a term. *)
val lookup : occur:occurrence -> inside:term -> term
