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

(* $Id: promelaoutput.mli,v 1.2 2008-10-02 13:33:29 uid588 Exp $ *)

open Promelaast

val print_raw_automata : Promelaast.buchautomata -> unit
val print_automata : Promelaast.buchautomata -> unit

val print_transition : Promelaast.trans -> unit
val print_transitionl : Promelaast.trans list -> unit

val print_state : Promelaast.state -> unit
val print_statel : Promelaast.state list -> unit


val print_automata_axiomatization : Promelaast.buchautomata -> unit

val print_automata_specification : Promelaast.buchautomata -> string list -> string -> string -> unit


val output_dot_automata : Promelaast.buchautomata -> string -> unit




(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
