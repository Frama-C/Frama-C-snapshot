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

(** Internal Cil printer.

    Must not be used by plug-in developers: use module {!Printer} instead.
    In particular, this pretty-printer is incorrect regarding annotations. 
    It should only be used by modules linked before {!Annotations}. 

    @since Fluorine-20130401 *)

include Printer_api.S

val get_termination_kind_name: Cil_types.termination_kind -> string

val register_shallow_attribute: string -> unit
(** Register an attribute that will never be pretty printed. *)

val register_behavior_extension: 
  string -> 
  (Printer_api.extensible_printer_type -> Format.formatter -> 
   int * Cil_types.identified_predicate list -> unit) -> unit
(** Register a pretty-printer used for behavior extensione. *)

val state: Printer_api.state

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
