(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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


open Promelaast

val setLtl_expressions : (string, (Cil_types.exp* string*Cil_types.predicate)) Hashtbl.t -> unit

(** Given a condition, this function does some logical simplifications. *)
val simplifyCond: condition -> condition

(** Given a transition list, this function returns the same transition list with simplifyCond done on each cross condition. Uncrossable transition are removed. *)
val simplifyTrans: trans list -> trans list



(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
