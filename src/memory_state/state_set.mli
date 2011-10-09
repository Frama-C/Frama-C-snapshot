(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

type t
exception Unchanged
val pretty : Format.formatter -> t -> unit
val add : Cvalue.Model.t -> t -> t
val unsafe_add : Cvalue.Model.t -> t -> t
val fold : ('a -> Cvalue.Model.t -> 'a) -> 'a -> t -> 'a
val iter : (Cvalue.Model.t -> unit) -> t -> unit
val merge_into : t -> t -> t
val join : t -> Cvalue.Model.t
val join_dropping_relations : t -> Cvalue.Model.t
val exists : (Cvalue.Model.t -> bool) -> t -> bool
val is_empty : t -> bool
val length : t -> int
val empty : t
val singleton : Cvalue.Model.t -> t
val of_list : Cvalue.Model.t list -> t
(*
val filter : (Cvalue.Model.t -> bool) -> t -> t




val length : t -> int
val nth : t -> int -> Cvalue.Model.t
*)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
