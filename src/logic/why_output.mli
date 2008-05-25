(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* $Id: why_output.mli,v 1.10 2008/04/01 09:25:21 uid568 Exp $ *)
(** Why interface *)

open Fol
open Format

val pure_type : formatter -> pure_type -> unit
val term : formatter -> term -> unit
val predicate : formatter -> predicate -> unit
val decl : formatter -> decl -> unit

(** Output to file [file] the given predicate in why syntax *)
val output : ?prelude:string -> file:string -> decl list -> unit

(*                      
Local Variables:
compile-command: "make -C ../.."
End:
*)
