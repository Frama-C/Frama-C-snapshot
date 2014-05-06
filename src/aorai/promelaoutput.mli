(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

val print_raw_automata : 
  Format.formatter -> Promelaast.typed_automaton -> unit

val print_parsed_expression: Format.formatter -> Promelaast.expression -> unit

val print_parsed_condition: Format.formatter -> Promelaast.condition -> unit

val print_seq_elt: Format.formatter -> Promelaast.seq_elt -> unit

val print_sequence: Format.formatter -> Promelaast.sequence -> unit

val print_parsed: Format.formatter -> Promelaast.parsed_condition -> unit

val print_condition: Format.formatter -> Promelaast.typed_condition -> unit

val print_action: Format.formatter -> Promelaast.action -> unit

val print_transition:
  Format.formatter -> 
  (Promelaast.typed_condition * Promelaast.action) Promelaast.trans -> unit

val print_transitionl:
  Format.formatter ->
  (Promelaast.typed_condition * Promelaast.action) Promelaast.trans list -> unit

val print_state : Format.formatter -> Promelaast.state -> unit
val print_statel : Format.formatter -> Promelaast.state list -> unit

val output_dot_automata : Promelaast.typed_automaton -> string -> unit

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
