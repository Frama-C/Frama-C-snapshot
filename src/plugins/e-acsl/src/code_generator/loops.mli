(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

(** Loop specific actions. *)

open Cil_types

(**************************************************************************)
(************************* Loop invariants ********************************)
(**************************************************************************)

val apply_after_transformation: Project.t -> unit

val mv_invariants: Env.t -> old:stmt -> stmt -> unit
(** Transfer the loop invariants from the [old] loop to the new one.
    Both statements must be loops. *)

val preserve_invariant:
  Project.t -> Env.t -> Kernel_function.t -> stmt -> stmt * Env.t * bool
(** modify the given stmt loop to insert the code which preserves its loop
    invariants. Also return the modify environment and a boolean which
    indicates whether the annotations corresponding to the loop invariant must
    be moved from the new statement to the old one. *)

(**************************************************************************)
(**************************** Nested loops ********************************)
(**************************************************************************)

val mk_nested_loops:
  loc:location -> (Env.t -> stmt list * Env.t) -> kernel_function -> Env.t ->
  Lscope.lscope_var list -> stmt list * Env.t
(** [mk_nested_loops ~loc mk_innermost_block kf env lvars] creates nested
    loops (with the proper statements for initializing the loop counters)
    from the list of logic variables [lvars]. Quantified variables create
    loops while let-bindings simply create new variables.
    The [mk_innermost_block] closure creates the statements of the innermost
    block. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

val translate_named_predicate_ref:
  (kernel_function -> Env.t -> predicate -> Env.t) ref

val named_predicate_ref:
  (kernel_function -> Env.t -> predicate -> exp * Env.t) ref

val term_to_exp_ref:
  (kernel_function -> Env.t -> term -> exp * Env.t) ref

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
