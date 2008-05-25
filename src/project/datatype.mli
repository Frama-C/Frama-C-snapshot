(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: datatype.mli,v 1.6 2008/04/10 15:48:06 uid562 Exp $ *)

(** Datatype implementations and builders. 
    Provide ways to implement signature [Project.Datatype.OUTPUT] without
    directly apply functor [Project.Datatype.Register]. *)

(** Implementation of [before_load] and [after_load] when those functions do
    nothing special. *)
module Nop : sig
  val before_load: unit -> unit
  val after_load : unit -> unit
end

module Int: Project.Datatype.OUTPUT with type t = int
module Bool: Project.Datatype.OUTPUT with type t = bool
module String: Project.Datatype.OUTPUT with type t = string
module BigInt : Project.Datatype.OUTPUT with type t = Big_int.big_int

(** {2 Builders} *)

(** Input signature of builders: all the builders depend on another datatype
    which have to implement this signature. *)
module type INPUT = sig
  include Project.Datatype.INPUT
  val self : Project.Datatype.t
end

(** {3 References} *)

module Ref(Data:INPUT) : Project.Datatype.OUTPUT with type t = Data.t ref

module Option(Data:INPUT) :
  Project.Datatype.OUTPUT with type t = Data.t option

module OptionRef(Data:INPUT) :
  Project.Datatype.OUTPUT with type t = Data.t option ref

(** {3 Hashtables} *)

(** Sub-signature of [Hashtbl.S]. *)
module type HASHTBL = sig
  type key
  type 'a t
  val create: int -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val add: 'a t -> key -> 'a -> unit
  val replace: 'a t -> key -> 'a -> unit
  val length: 'a t -> int
end

(** Generic functor. *)

(** {3 Lists} *)

(** Generic functor building a list of data. *)
module List(Data:INPUT) : Project.Datatype.OUTPUT with type t = Data.t list

(** {3 Hashtables} *)

module Make_Hashtbl(H: HASHTBL)(Data:INPUT) :
  Project.Datatype.OUTPUT with type t = Data.t H.t

(** {3 Sets} *)

module type SET_INPUT = sig
  include INPUT
  val compare: t -> t -> int
end

(** Sub-signature of [Set.S]. *)
module type SET = sig
  type elt
  type t
  val empty: t
  val singleton: elt -> t
  val add: elt -> t -> t
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

(** Generic functor building a set datatype. *)
module Make_Set(Set:SET)(Data:INPUT with type t = Set.elt) :
  Project.Datatype.OUTPUT with type t = Set.t

(** Generic functor building a datatype for a reference on a set. *)
module Make_SetRef(Set:SET)(Data:INPUT with type t = Set.elt) :
  Project.Datatype.OUTPUT with type t = Set.t ref

(** Functor building a set datatype. *)
module Set(Data:sig include INPUT val compare:t -> t -> int end) : 
  Project.Datatype.OUTPUT with type t = Set.Make(Data).t

(** {3 Maps} *)

(** Sub-signature of [Map.S]. *)
module type MAP = sig
  type key
  type 'a t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

(** Generic functor building a map datatype. *)
module Make_Map(Map:MAP)(Data:INPUT) :
  Project.Datatype.OUTPUT with type t = Data.t Map.t

(** {3 Queues} *)

module Queue(Data:INPUT) : Project.Datatype.OUTPUT with type t = Data.t Queue.t

(** {3 Tuples} *)

module Couple(D1:INPUT)(D2:INPUT) : 
  Project.Datatype.OUTPUT with type t = D1.t * D2.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
