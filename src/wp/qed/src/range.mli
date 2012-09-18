(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

(** Intervals over {!module:Z} *)

exception Empty

type t
val pretty : Format.formatter -> t -> unit

(** {2 Constructors} *)

val top : t
val bot : t
val value : Z.t -> t (** Singleton [[a]] *)
val range : Z.t -> Z.t -> t (** Interval [[a..b]] *)
val sup_to : Z.t -> t (** Right-opened interval [[a..]] *)
val inf_to : Z.t -> t (** Left-opened interval [[..b]] *)

(** {2 Operations} *)

val upper : t -> t (** Extends to left: [[..sup]] *)
val lower : t -> t (** Extends to right: [[inf..]] *)
val union : t -> t -> t
val inter : t -> t -> t
val subset : t -> t -> bool
val leq : t -> t -> bool (** Partial order: [[?..a]] <= [[b..?]] when [a<=b] *)
val lt  : t -> t -> bool (** Partial order: [[?..a]] < [[b..?]] when [a<b] *)
val add : t -> t -> t
val sub : t -> t -> t
val opp : t -> t
val shift : Z.t -> t -> t

(** {2 Elements} *)

val inf : t -> Z.t option (** raises Empty if bottom *)
val sup : t -> Z.t option (** raises Empty if bottom *)
val mem : Z.t -> t -> bool
val is_top : t -> bool
val is_bot : t -> bool
val singleton : t -> Z.t option (** raises Empty if bottom *)
