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

open Cil_types

(* Create calls to a few memory builtins.
   Partial support for ranges is provided. *)

val call:
  loc:location -> kernel_function -> string -> typ -> Env.t -> term ->
  exp * Env.t
(* [call ~loc kf name ctx env t] creates a call to the E-ACSL memory builtin
   identified by [name] which only requires a single argument, namely the
   pointer under study. The supported builtins are:
   [base_addr], [block_length], [offset] and [freeable]. *)

val call_with_size:
  loc:location -> kernel_function -> string -> typ -> Env.t -> term ->
  predicate -> exp * Env.t
(* [call_with_size ~loc kf name ctx env t p] creates a call to the E-ACSL
   memory builtin identified by [name] which requires two arguments, namely
   the pointer under study and a size in bytes.
   The only supported builtin is: [initialized].
   [t] can denote ranges of memory locations.
   [p] is the predicate under testing. *)

val call_valid:
  loc:location -> kernel_function -> string -> typ -> Env.t -> term ->
  predicate -> exp * Env.t
(* [call_valid ~loc kf name ctx env t p] creates a call to the E-ACSL memory
   builtin [valid] or [valid_read] according to [name].
   [t] can denote ranges of memory locations.
   [p] is the predicate under testing. *)

(**************************************************************************)
(********************** Forward references ********************************)
(**************************************************************************)

val predicate_to_exp_ref:
  (kernel_function -> Env.t -> predicate -> exp * Env.t) ref

val term_to_exp_ref:
  (kernel_function -> Env.t -> term -> exp * Env.t) ref