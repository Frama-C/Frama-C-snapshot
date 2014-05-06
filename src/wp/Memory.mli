(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(* --- Memory Model Interface                                             --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Ctypes
open Lang.F

type 'a sequence = { pre : 'a ; post : 'a }

(** Memory Values *)

type acs = 
  | RW (** Read-Write Access *)
  | RD (** Read-Only Access *)

type 'a value =
  | Val of term
  | Loc of 'a

type 'a rloc =
  | Rloc of c_object * 'a
  | Rarray of 'a * c_object * int
  | Rrange of 'a * c_object * term option * term option

type 'a sloc = 
  | Sloc of 'a
  | Sarray of 'a * c_object * int (** full sized-array range *)
  | Srange of 'a * c_object * term option * term option
  | Sdescr of var list * 'a * pred

type 'a logic =
  | Vexp of term
  | Vloc of 'a
  | Vset of Vset.set
  | Lset of 'a sloc list

(** Memory Variables *)

module type Chunk =
sig

  type t
  val self : string
  val hash : t -> int
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
  val tau_of_chunk : t -> tau
  val basename_of_chunk : t -> string
  val is_framed : t -> bool
  (** Whether the Chunk is local to a function.
      Means the chunk is separated from any call side-effect. *)
    
end

(** Memory Environment *)

module type Sigma =
sig
  
  type chunk
  type domain
  type t

  val create : unit -> t
  val copy : t -> t
  val merge : t -> t -> t * Passive.t * Passive.t
  val join : t -> t -> Passive.t (** pairwise equal *)
  val assigned : t -> t -> domain -> pred Bag.t (** equal chunks outside domain *)

  val mem : t -> chunk -> bool
  val get : t -> chunk -> var
  val value : t -> chunk -> term
  val iter : (chunk -> var -> unit) -> t -> unit
  val iter2 : (chunk -> var option -> var option -> unit) -> t -> t -> unit
  val havoc : t -> domain -> t
  val havoc_chunk : t -> chunk -> t
  val havoc_any : call:bool -> t -> t
  val domain : t -> domain

  val pretty : Format.formatter -> t -> unit

end

(** Memory Model *)

module type Model =
sig

  val configure : Model.tuning
  val datatype : string (** for projectification *)

  module Chunk : Chunk

  module Heap : Qed.Collection.S 
    with type t = Chunk.t

  module Sigma : Sigma
    with type chunk = Chunk.t
    and type domain = Heap.set

  type loc
  type chunk = Chunk.t
  type sigma = Sigma.t
  type segment = loc rloc

  val pretty : Format.formatter -> loc -> unit
  val vars : loc -> Vars.t
  val occurs : var -> loc -> bool

  val null : loc
  val literal : eid:int -> Cstring.cst -> loc
  val cvar : varinfo -> loc
  val pointer_loc : term -> loc
  val pointer_val : loc -> term
  val field : loc -> fieldinfo -> loc
  val shift : loc -> c_object -> term -> loc
  val base_addr : loc -> loc
  val block_length : sigma -> c_object -> loc -> term

  val cast : c_object sequence -> loc -> loc
  val loc_of_int : c_object -> term -> loc
  val int_of_loc : c_int -> loc -> term

  val domain : c_object -> loc -> Heap.set

  val load : sigma -> c_object -> loc -> loc value
  val copied : sigma sequence -> c_object -> loc -> loc -> pred list
  val stored : sigma sequence -> c_object -> loc -> term -> pred list
  val assigned : sigma sequence -> c_object -> loc sloc -> pred list

  val is_null : loc -> pred
  val loc_eq : loc -> loc -> pred
  val loc_lt : loc -> loc -> pred
  val loc_neq : loc -> loc -> pred
  val loc_leq : loc -> loc -> pred
  val loc_diff : c_object -> loc -> loc -> term

  val valid : sigma -> acs -> segment -> pred
  val scope : sigma -> Mcfg.scope -> varinfo list -> sigma * pred list

  val included : segment -> segment -> pred
  val separated : segment -> segment -> pred

end
