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

(** Call [start_doing] when starting analyzing a new function. The new
   function is on the top of the call stack.*)
val start_doing: Value_types.callstack -> unit

(** Call [start_doing] when finishing analyzing a function. The
   function must still be on the top of the call stack. *)
val stop_doing: Value_types.callstack -> unit

(** Display a complete summary of performance informations. Can be
    called during the analysis. *)
val display: Format.formatter -> unit

(** Reset the internal state of the module; to call at the very
    beginning of the analysis. *)
val reset: unit -> unit
