(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(* -------------------------------------------------------------------------- *)

(** Compound Loader *)

open Cil_types
open Definitions
open Ctypes
open Lang.F
open Sigs

val cluster : unit -> cluster

(** Loader Model for Atomic Values *)
module type Model =
sig

  module Chunk : Chunk
  module Sigma : Sigma with type chunk = Chunk.t

  val name : string

  type loc
  val sizeof : c_object -> int
  val field : loc -> fieldinfo -> loc
  val shift : loc -> c_object -> term -> loc

  (** Conversion among loc, t_pointer terms and t_addr terms *)

  val to_addr : loc -> term
  val to_region_pointer : loc -> int * term
  val of_region_pointer : int -> c_object -> term -> loc

  val domain : c_object -> loc -> Sigma.domain
  val frames : c_object -> loc -> Chunk.t -> frame list

  val last : Sigma.t -> c_object -> loc -> term

  val havoc : c_object -> loc -> length:term ->
    Chunk.t -> fresh:term -> current:term -> term

  val eqmem : c_object -> loc -> Chunk.t -> term -> term -> pred

  val eqmem_forall :
    c_object -> loc -> Chunk.t -> term -> term -> var list * pred * pred

  val load_int : Sigma.t -> c_int -> loc -> term
  val load_float : Sigma.t -> c_float -> loc -> term
  val load_pointer : Sigma.t -> typ -> loc -> loc

  val store_int : Sigma.t -> c_int -> loc -> term -> Chunk.t * term
  val store_float : Sigma.t -> c_float -> loc -> term -> Chunk.t * term
  val store_pointer : Sigma.t -> typ -> loc -> term -> Chunk.t * term

end

(** Generates Loader for Compound Values *)
module Make (M : Model) :
sig

  val load : M.Sigma.t -> c_object -> M.loc -> M.loc Sigs.value
  val loadvalue : M.Sigma.t -> c_object -> M.loc -> term

  val havoc : M.Sigma.t sequence -> c_object -> M.loc -> equation list
  val havoc_length : M.Sigma.t sequence -> c_object -> M.loc -> term -> equation list

  val stored : M.Sigma.t sequence -> c_object -> M.loc -> term -> equation list
  val copied : M.Sigma.t sequence -> c_object -> M.loc -> M.loc -> equation list

  val assigned : M.Sigma.t sequence -> c_object -> M.loc sloc -> equation list

end

(* -------------------------------------------------------------------------- *)
