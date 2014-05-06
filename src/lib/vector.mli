(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* -------------------------------------------------------------------------- *)
(** Extensible Arrays *)
(* -------------------------------------------------------------------------- *)

type 'a t

val create : unit -> 'a t

val length : 'a t -> int
val size : 'a t -> int (** Same as [length] *)

val get : 'a t -> int -> 'a (** Raise [Not_found] if out-of-bounds. *)
val set : 'a t -> int -> 'a -> unit (** Raise [Not_found] if out-of-bounds. *)
val add : 'a t -> 'a -> unit (** Element will be added at index [size]. After addition, it is at index [size-1]. *)
val addi : 'a t -> 'a -> int (** Return index of added (last) element. *)
val clear : 'a t -> unit (** Do not modify actual capacity. *)
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t (** Result is shrinked. *)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t (** Result is shrinked. *)

val find : 'a t -> ?default:'a -> ?exn:exn -> int -> 'a
  (** Default exception is [Not_found]. 
      If a [default] value is provided, no exception is raised. *)

val update : 'a t -> default:'a -> int -> 'a -> unit
  (** Set value at index. The vector is resized if necessary 
      and empty cells are populated with the [default] value. *)

val to_array : 'a t -> 'a array (** Makes a copy. *)
val of_array : 'a array -> 'a t (** Makes a copy. *)

(** Low-level interface. Internal capacity. *)
val capacity : 'a t -> int

(** Low-level interface. Sets internal capacity. Extra elements are removed. *)
val resize : 'a t -> int -> unit

(** Low-level interface. Sets capacity to content. *)
val shrink : 'a t -> unit

