(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

open VCS

(* -------------------------------------------------------------------------- *)
(* --- Prover Implementation against Task API                             --- *)
(* -------------------------------------------------------------------------- *)

val prove : Wpo.t ->
  ?mode:mode ->
  ?start:(Wpo.t -> unit) ->
  ?callin:(Wpo.t -> prover -> unit) ->
  ?callback:(Wpo.t -> prover -> result -> unit) ->
  prover -> bool Task.task

val spawn : Wpo.t ->
  ?start:(Wpo.t -> unit) ->
  ?callin:(Wpo.t -> prover -> unit) ->
  ?callback:(Wpo.t -> prover -> result -> unit) ->
  ?success:(Wpo.t -> prover option -> unit) ->
  (mode * prover) list -> unit
