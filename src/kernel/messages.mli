(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

val enable_collect: unit -> unit 
  (** Do collect messages *)

val disable_echo: unit -> unit 
  (** Do not display messages on stderr *)

val dump_messages: unit -> unit
  (** Dump collected messages to standard channels *)

val iter: (int -> Log.event -> unit) -> unit
val clear: unit -> unit

val depend: Project.Computation.t -> unit
  (** Add a dependency from the internal state of the messages manager to the
      given internal state. *)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
