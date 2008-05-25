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

(* $Id: computation.mli,v 1.4 2008/04/10 15:48:06 uid562 Exp $ *)

(** Internal state builders. 
    Provide ways to implement signature [Project.Computation.OUTPUT] without
    directly apply functor [Project.Computation.Register]. 
    Depending on the builder, also provide some additional useful
    information. *)

(** {3 Useful operations} *)

val apply_once: 
  Project.Computation.Name.t -> Project.Computation.t list -> (unit -> unit) -> 
  (unit -> unit) * Project.Computation.t
    (** [apply_once name dep f] returns a closure applying [f] only once and the
	state internally used. [name] and [dep] are respectively the name and
	the dependencies of the local state created by this function.  Should
	be used partially applied. If [f] raises an exception, then it is
	considered as not applied. *)

(** {2 Builders} *)

(** {3 References} *)

(** Signature of the stored data. *)
module type REF_INPUT = sig
  include Datatype.INPUT
  val default: t
end

(** Output signature of [Ref]. *)
module type REF_OUTPUT = sig
  include Project.Computation.OUTPUT
  type data
    (** Type of the referenced value. *)
  val set: data -> unit
    (** Change the referenced value. *)
  val get: unit -> data
    (** Get the referenced value. *)
  val clear: unit -> unit
    (** Reset the reference to its default value. *)
end

module Ref(Data:REF_INPUT)(Info:Signature.NAME_DPDS) 
  : REF_OUTPUT with type data = Data.t

(** Output signature of [OptionRef]. *)
module type OPTION_REF_OUTPUT = sig
  include REF_OUTPUT
  val memo: ?change:(data -> data) -> (unit -> data) -> data
    (** Memoization. Compute on need the stored value. 
	If the data is already computed (i.e. is not [None]), 
	it is possible to change with [change]. *)
  val map: (data -> data) -> data option
  val may: (data -> unit) -> unit
end

module OptionRef(Data:Datatype.INPUT)(Info:Signature.NAME_DPDS) : 
  OPTION_REF_OUTPUT with type data = Data.t

(** {3 Hashtables} *)

module type HASHTBL = sig
  include Datatype.HASHTBL
  val clear: 'a t -> unit
  val find: 'a t -> key -> 'a
  val find_all: 'a t -> key -> 'a list
  val remove: 'a t -> key -> unit
  val mem: 'a t -> key -> bool
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

(** Output signature of builders of hashtables. *)
module type HASHTBL_OUTPUT = sig
  include Project.Computation.OUTPUT
  type key
  type data
  val replace: key -> data -> unit 
    (** Add a new binding. The previous one is removed. *)
  val add: key -> data -> unit
    (** Add a new binding. The previous one is only hidden. *)
  val clear: unit -> unit
    (** Clear the table. *)
  val iter: (key -> data -> unit) -> unit
  val fold: (key -> data -> 'a -> 'a) -> 'a -> 'a
  val memo: ?change:(data -> data) -> (key -> data) -> key -> data
    (** Memoization. Compute on need the data associated to a given key using
	the given function. 
	If the data is already computed, it is possible to change with
	[change]. *)
  val find: key -> data
    (** Return the current binding of the given key.
	@raise Not_found if the key is not in the table. *)
  val find_all: key -> data list
    (** Return the list of all data associated with the given key. *)
  val unsafe_find: key -> data
    (** Unsafe version of [find]. Do not raise [Not_found]. 
	You can use it if you can prove that the given key belongs to the
	state. *)
  val mem: key -> bool
  val remove: key -> unit
end

module Make_Hashtbl
  (H:HASHTBL)(Data:Datatype.INPUT)(Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = H.key and type data = Data.t

module Hashtbl
  (Key:Hashtbl.HashedType)(Data:Datatype.INPUT)(Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = Key.t and type data = Data.t

(** {3 References on a set} *)

module type SET = sig
  type elt
  type t
  val empty: t
  val singleton: elt -> t
  val is_empty: t -> bool
  val add: elt -> t -> t
  val mem: elt -> t -> bool
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (elt -> unit) -> t -> unit
end

(** Output signature of builders of references on a set. *)
module type SET_REF_OUTPUT = sig
  include Project.Computation.OUTPUT
  type elt
  val add: elt -> unit
  val mem: elt -> bool
  val is_empty: unit -> bool
  val fold: (elt -> 'a -> 'a) -> 'a -> 'a
  val iter: (elt -> unit) -> unit
end

module Make_SetRef
  (Set:SET)
  (Data:Datatype.SET_INPUT with type t = Set.elt)
  (Info:Signature.NAME_DPDS) :
  SET_REF_OUTPUT with type elt = Data.t

module SetRef(Data:Datatype.SET_INPUT)(Info:Signature.NAME_DPDS) :
  SET_REF_OUTPUT with type elt = Data.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
