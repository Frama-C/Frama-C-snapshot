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

open VCS

type 'a process =
  ?valid:bool -> (** Play provers with valid result (default: true) *)
  ?failed:bool -> (** Play provers with invalid result (default: true) *)
  ?provers:prover list -> (** Additional list of provers to {i try} when stuck *)
  ?start:(Wpo.t -> unit) ->
  ?callin:(Wpo.t -> prover -> unit) ->
  ?callback:(Wpo.t -> prover -> result -> unit) ->
  ?success:(Wpo.t -> prover option -> unit) ->
  Wpo.t -> 'a

val prove : unit Task.task process
val spawn : unit process

val get : Wpo.t -> [ `Script | `Proof | `Saved | `None ]
val save : Wpo.t -> unit
