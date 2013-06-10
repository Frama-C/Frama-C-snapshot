(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(** Syntactic loop unrolling. *)
(** Performs and closes all syntactic transformations, including syntactic
   loop unrolling. *)
val compute : Cil_types.file -> unit

(** Hook for transformation to be applied just before unrolling loops.
    The boolean value indicates if the CFG has to be recomputed.
    @since Oxygen-20120901 *)
val add_syntactic_transformation : (Cil_types.file * bool -> Cil_types.file * bool) -> unit

(** Performs only unrolling transformation without using -ulevel option.
    Loop invariant \false can be emmitted on total unrolling request.
    Do not forget to apply  [transformations_closure] afterwards. 
    @since Oxygen-20120901 *)
val apply_transformation: int -> Emitter.t -> (Cil_types.file * bool) ->
  (Cil_types.file * bool)
    
(** Close syntactic transformations. 
    @since Oxygen-20120901 *)
val transformations_closure: (Cil_types.file * bool) -> (Cil_types.file * bool)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
