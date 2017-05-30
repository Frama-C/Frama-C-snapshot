(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** E-ACSL built-in database. *)

val mem: string -> bool
(** @return true iff the given function name is an E-ACSL built-in *)

val find: string -> Cil_types.varinfo
(** Get the varinfo corresponding to the given E-ACSL built-in name.
    @raise Not_found if it is not a built-in *)

val update: string -> Cil_types.varinfo -> unit
(** If the given name is an E-ACSL built-in, change its old varinfo by the given
    new one. *)

(*
Local Variables:
compile-command: "make"
End:
*)
