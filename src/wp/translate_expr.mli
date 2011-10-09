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
(** Translation of Expressions                                                *)
(* -------------------------------------------------------------------------- *)

open Formula

module Create (M : Mvalues.S):
sig


  (** [addr mem lv] interprets the left value [lv] as
      an address (memory location) in the memory [mem].**)
  val addr : M.mem -> Cil_types.lval -> M.loc


  (** [expr mem e] interprets the expression[e] as a value
      in  memory [mem] **)
  val expr : M.mem -> Cil_types.exp -> M.value

  (** [cond mem e] interprets [e] as a boolean
      in memory [mem]. **)
  val cond : M.mem -> Cil_types.exp -> M.F.boolean

 (** [prop mem e] interprets the expression[e] as a predicate
      in  memory [mem] **)
  val prop : M.mem -> Cil_types.exp -> M.F.pred

 (**[expr_cast mem ty_to ty_from e] casts [e] of type [ty_from] to type
    type [ty_to] in memory [mem] **)

  val expr_cast : Cil_types.typ -> Cil_types.typ -> M.value -> M.value

end


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
