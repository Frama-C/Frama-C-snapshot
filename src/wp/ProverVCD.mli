(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

open VCS
open Wpo

(* -------------------------------------------------------------------------- *)
(* --- Prover Implementation for Legacy Models                            --- *)
(* -------------------------------------------------------------------------- *)

val prove : Wpo.t -> VC_Legacy.t -> ?interactive:bool -> prover -> result Task.task

val check : Wpo.t -> VC_Legacy.t -> 
  ?callback:(Wpo.t -> VCS.language -> VCS.result -> unit) ->
  VCS.language -> unit Task.task

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
