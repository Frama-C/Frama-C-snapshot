(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(** Logical Language *)
(* -------------------------------------------------------------------------- *)

open LogicId

(* -------------------------------------------------------------------------- *)
(** {2 Types} *)
(* -------------------------------------------------------------------------- *)

type tau =
  | Integer   (** Mathematical Z numbers *)
  | Real      (** Mathematical R numbers *)
  | Boolean   (** Finite set [{true,false}] *)
  | Pointer   (** Pointer datatype in current model (see LogicDef.register) *)
  | Set of tau (** [Set t]: Mathematical sets with elements of type [t] *)
  | Array of tau * tau (** [Array(ta,tb)]: Total functions from [ta] to [tb] *)
  | Record of id (** Tuples [(fi,vi)] with [vi] of type [fi.ftype] and [fi] in [rfields] *)
  | ADT of id * tau list (** Polymorphic instance of datatype [a] with parameters [ti] *)
  | ALPHA of int (** [i-th] parameter of a polymorphic type in its definition. Starts with [0]. *)

type field = {
  f_record : id ;
  f_name : id ;
  f_type : tau ;
}

val compare_tau : tau -> tau -> int
val compare_sig : tau list -> tau list -> int
val compare_field : field -> field -> int

val depend : Iset.t -> tau -> Iset.t
