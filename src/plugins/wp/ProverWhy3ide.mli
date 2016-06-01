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

val run :
  includes:string list ->
  files:string list ->
  session:string ->
  bool Task.task

val prove :
  ?callback:(Wpo.t -> VCS.prover -> VCS.result -> unit) ->
  iter:((Wpo.t -> unit) -> unit) ->
  unit Task.task
