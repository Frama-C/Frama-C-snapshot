(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Intervals of integers. *)

exception Cannot_compare_intervals

include Datatype.S with type t = Integer.t * Integer.t

(** Locates (b2, e2) with respect to (b1, e1).
    Therefore the meaning of "Above" and "Below" may look as if it
    is reversed, beware. *)
val fuzzy_order: t -> t -> Rangemap.fuzzy_order

val shift: Integer.t -> t -> t
val clip_itv: t -> t -> t

exception Not_fully_included
val check_coverage: t -> (t * 'a) list -> unit


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
