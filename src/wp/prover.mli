(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Prover Implementation against Task API                             --- *)
(* -------------------------------------------------------------------------- *)

val server : unit -> Task.server

(** {1 Prover Implementations} *)

type verdict = Valid | Invalid | Unknown

val prove :
  ?callin:(Wpo.t -> Wpo.prover -> unit) ->
  ?callout:(Wpo.t -> Wpo.prover -> Wpo.result -> unit) ->
  Wpo.t -> interactive:bool -> Wpo.prover -> unit Task.task

(** The task will run the prover and update the Wpo base accordingly. *)

val check :
  ?callout:(Wpo.t -> Wpo.language -> Wpo.result -> unit) ->
  Wpo.t -> Wpo.language -> unit Task.task
