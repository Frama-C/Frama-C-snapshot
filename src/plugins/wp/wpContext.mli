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

(** Model Registration *)

type model
type scope = Global | Kf of Kernel_function.t
type tuning = (unit -> unit)
type hypotheses = unit -> MemoryContext.clause list

val register :
  id:string ->
  ?descr:string ->
  ?tuning:tuning list ->
  ?hypotheses:hypotheses ->
  unit -> model

val get_descr : model -> string
val get_emitter : model -> Emitter.t

val compute_hypotheses : model -> Kernel_function.t -> MemoryContext.clause list

type context = model * scope
type t = context

module S :
sig
  type t = context
  val id : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module MODEL :
sig
  type t = model
  val id : t -> string
  val descr : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val repr : t
end

module SCOPE :
sig
  type t = scope
  val id : t -> string
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

val is_defined : unit -> bool
val on_context : context -> ('a -> 'b) -> 'a -> 'b
val get_model : unit -> model
val get_scope : unit -> scope
val get_context : unit -> context

val directory : unit -> string (** Current model in ["-wp-out"] directory *)

module type Entries =
sig
  type key
  type data
  val name : string
  val compare : key -> key -> int
  val pretty : Format.formatter -> key -> unit
end

module type Registry =
sig

  module E : Entries
  type key = E.key
  type data = E.data

  val id : basename:string -> key -> string
  val mem : key -> bool
  val find : key -> data
  val get : key -> data option
  val clear : unit -> unit
  val remove : key -> unit
  val define : key -> data -> unit
  (** no redefinition ; circularity protected *)
  val update : key -> data -> unit
  (** set current value, with no protection *)
  val memoize : (key -> data) -> key -> data
  (** with circularity protection *)
  val compile : (key -> data) -> key -> unit
  (** with circularity protection *)

  val callback : (key -> data -> unit) -> unit

  val iter : (key -> data -> unit) -> unit
  val iter_sorted : (key -> data -> unit) -> unit
end

module Index(E : Entries) : Registry with module E = E
(** projectified, depend on the model, not serialized *)

module Static(E : Entries) : Registry with module E = E
(** projectified, independent from the model, not serialized *)

module type Key =
sig
  type t
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

module type Data =
sig
  type key
  type data
  val name : string
  val compile : key -> data
end

module type IData =
sig
  type key
  type data
  val name : string
  val basename : key -> string
  val compile : key -> string -> data
end

module type Generator =
sig
  type key
  type data
  val get : key -> data
  val mem : key -> bool
  val clear : unit -> unit
  val remove : key -> unit
end

(** projectified, depend on the model, not serialized *)
module Generator(K : Key)(D : Data with type key = K.t) : Generator
  with type key = D.key
   and type data = D.data

(** projectified, independent from the model, not serialized *)
module StaticGenerator(K : Key)(D : Data with type key = K.t) : Generator
  with type key = D.key
   and type data = D.data

(** projectified, depend on the model, not serialized *)
module GeneratorID(K : Key)(D : IData with type key = K.t) : Generator
  with type key = D.key
   and type data = D.data

(** projectified, independent from the model, not serialized *)
module StaticGeneratorID(K : Key)(D : IData with type key = K.t) : Generator
  with type key = D.key
   and type data = D.data
