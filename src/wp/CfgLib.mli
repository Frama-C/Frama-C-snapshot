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
(* --- Control Flow Graph Library                                         --- *)
(* -------------------------------------------------------------------------- *)

open CfgTypes

(** Creating CFG *)

module Make(T : Transition) : Cfg with module T = T
    
(** Labeling nodes *)

module Labels(C : Cfg)(H : Hashtbl.S) :
sig
  type t
  type label = H.key
  val create : ?size:int -> C.cfg -> t
  val label : t -> label -> C.node
    (** Retrieve (or create) the node associated to the label. *)
  val set_label : t -> label -> C.node -> unit
    (** Register the label to points to the given node. *)
  val iter : (H.key -> C.node -> unit) -> t -> unit
end

(** Associating attributes to nodes. *)

module Attr(C : Cfg) :
sig
  type 'a t
  val create : C.cfg -> 'a -> 'a t
  val get : 'a t -> C.node -> 'a (** Returns default if not found. *)
  val set : 'a t -> C.node -> 'a -> unit (** Replace old value. *)
  val merge : 'a t -> C.node -> ('a -> 'a) -> unit (** Update with old value. *)
  val merged : 'a t -> C.node -> ('a -> 'a) -> 'a (** Helper for [merge] and finally [get]. *)
  val merge_op : ('a -> 'b -> 'a) -> 'a t -> C.node -> 'b -> unit (** Helper for [merge] with a binary operator. *)
  val merged_op : ('a -> 'b -> 'a) -> 'a t -> C.node -> 'b -> 'a (** Helper for [merged] with a binary operator. *)
  val iter : (C.node -> 'a -> unit) -> 'a t -> unit
end

(** Tracing nodes during CFG transformations. *)

module Transform(A : Cfg)(B : Cfg) :
sig
  type t

  val create : A.cfg -> B.cfg -> t
  (** Graph [A] should be static : further nodes in [A] can not be indexed. [B] is free of constraint. *)
    
  val image : t -> A.node -> B.node
  val set_image : t -> A.node -> B.node -> unit
    
  (** Duplicates [A] into [B] with the provided morphism. *)
  val copy : t -> (A.node -> A.transition -> B.transition) -> unit
end

