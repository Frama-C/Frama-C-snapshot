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

(* ------------------------------------------------------------------------ *)
(**{2 Indexed API} *)
(* ------------------------------------------------------------------------ *)

type property =
  | Later of Property.t
  | Proxy of Property.t * Emitter.t * Property.t list

module type Info =
sig
  include State_builder.Info_with_size
  type key
  val property : key -> property
end

module type Indexed =
sig
  type key
  val mem : key -> bool
  val property : key -> Property.t
  val add_hook : (key -> Property.t -> unit) -> unit
    (** Hooks are executed once at property creation *)
end

module type Indexed2 =
sig
  type key1
  type key2
  val mem : key1 -> key2 -> bool
  val property : key1 -> key2 -> Property.t
  val add_hook : (key1 -> key2 -> Property.t -> unit) -> unit
    (** Hooks are executed once at property creation *)
end

(* ------------------------------------------------------------------------ *)
(**{2 Indexes} *)
(* ------------------------------------------------------------------------ *)

module Indexed
  (Key:Datatype.S_with_collections)
  (Info:Info with type key = Key.t) :
  Indexed with type key = Key.t

module Indexed2
  (Key1:Datatype.S_with_collections)
  (Key2:Datatype.S_with_collections)
  (Info:Info with type key = Key1.t * Key2.t) :
  Indexed2 with type key1 = Key1.t and type key2 = Key2.t
