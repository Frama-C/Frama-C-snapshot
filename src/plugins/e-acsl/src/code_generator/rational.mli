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

(** Generation of rational numbers. *)

open Cil_types

val create: loc:location -> ?name:string -> exp -> Env.t -> term option ->
  exp * Env.t
(** Create a real *)

val init_set: loc:location -> lval -> exp -> exp -> stmt
(** [init_set lval lval_as_exp exp] sets [lval] to [exp] while guranteeing that
    [lval] is properly initialized wrt the underlying real library. *)

val normalize_str: string -> string
(** Normalize the string so that it fits the representation used by the
    underlying real library. For example, "0.1" is a real number in ACSL
    whereas it is considered as a double by `libgmp` because it is written in
    decimal expansion. In order to make `libgmp` consider it to be a rational,
    it must be converted into "1/10". *)

val cast_to_z: loc:location -> ?name:string -> exp -> Env.t -> exp * Env.t
(** Assumes that the given exp is of real type and casts it into Z *)

val add_cast: loc:location -> ?name:string -> exp -> Env.t -> typ ->
  exp * Env.t
(** Assumes that the given exp is of real type and casts it into
    the given typ *)

val binop: loc:location -> binop -> exp -> exp -> Env.t -> term option ->
  exp * Env.t
(** Applies [binop] to the given expressions. The optional term
    indicates whether the comparison has a correspondance in the logic. *)

val cmp: loc:location -> binop -> exp -> exp -> Env.t -> term option ->
  exp * Env.t
(** Compares two expressions according to the given [binop]. The optional term
    indicates whether the comparison has a correspondance in the logic. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
