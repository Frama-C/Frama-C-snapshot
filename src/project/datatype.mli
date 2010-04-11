(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

(** Datatype implementations and builders.
    Provide ways to implement signature [Project.Datatype.OUTPUT] without
    directly apply functor [Project.Datatype.Register].
    @plugin development guide *)

open Project.Datatype

module Unit: S with type t = unit

(** @plugin development guide *)
module Int: S with type t = int

(** @plugin development guide *)
module Bool: S with type t = bool

module String: S with type t = string
module BigInt : S with type t = Big_int.big_int

module Formatter: S with type t = Format.formatter
  (** @since Beryllium-20090901 *)
module OutChannel: S with type t = Pervasives.out_channel
  (** @since Beryllium-20090901 *)
module InChannel: S with type t = Pervasives.in_channel
  (** @since Beryllium-20090901 *)

(** {2 Builders} *)

(** {3 References} *)

(** @plugin development guide *)
module Ref(Data:S) : S with type t = Data.t ref

(** @plugin development guide *)
module Option(Data:S) : S with type t = Data.t option

module OptionRef(Data:S) : S with type t = Data.t option ref

(** {3 Lists} *)

(** Generic functor building a list of data.
    @plugin development guide *)
module List(Data:S) : S with type t = Data.t list

(** {3 Hashtables} *)

(** Sub-signature of [Hashtbl.S]. *)
module type HASHTBL = sig
  type key
  type 'a t
  val create: int -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val add: 'a t -> key -> 'a -> unit
  val replace: 'a t -> key -> 'a -> unit
  val length: 'a t -> int
  val find_all: 'a t -> key -> 'a list
end

module Make_Hashtbl(H: HASHTBL)(Data:S) : S with type t = Data.t H.t
  (** Must not be used if type [H.key] is equal to type [Project.t] *)

(** {3 Sets} *)

(** Sub-signature of [Set.S]. *)
module type SET = sig
  type elt
  type t
  val descr: Unmarshal.t
  val empty: t
  val singleton: elt -> t
  val add: elt -> t -> t
  val iter: (elt -> unit) -> t -> unit
    (** @since Beryllium-20090901 *)
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

(** Generic functor building a set datatype. *)
module Make_Set(Set:SET)(Data:S with type t = Set.elt) :
  S with type t = Set.t

(** Generic functor building a datatype for a reference on a set. *)
module Make_SetRef(Set:SET)(Data:S with type t = Set.elt) :
  S with type t = Set.t ref

(** Functor building a set datatype. *)
module Set(Data: S) : S with type t = Set.Make(Data).t

(** {3 Maps} *)

(** Sub-signature of [Map.S]. *)
module type MAP = sig
  type key
  type 'a t
  val descr:Unmarshal.t -> Unmarshal.t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** @since Beryllium-20090901 *)
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

(** Generic functor building a map datatype. *)
module Make_Map(Map:MAP)(Data:S) : S with type t = Data.t Map.t
  (** Must not be used if type [H.key] is equal to type [Project.t] *)

(** {3 Queues} *)

module Queue(Data:S) : S with type t = Data.t Queue.t

(** {3 Tuples} *)

(** @plugin development guide *)
module Couple(D1:S)(D2:S) : S with type t = D1.t * D2.t

module Triple(D1:S)(D2:S)(D3:S) : S with type t = D1.t * D2.t * D3.t

(** {3 Project} *)

module Project : S with type t = Project.t
  (** Equivalent to {!Project.Datatype.Own}. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
