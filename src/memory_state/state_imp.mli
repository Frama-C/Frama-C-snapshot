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

(* $Id: state_set.mli,v 1.6 2009-02-24 17:53:39 uid527 Exp $ *)

type t
exception Unchanged
val pretty : Format.formatter -> t -> unit
val add : Relations_type.Model.t -> t -> unit
val fold : ( Relations_type.Model.t -> 'a -> 'a) -> t -> 'a  -> 'a
val iter : (Relations_type.Model.t -> unit) -> t -> unit
val merge_into : t -> t -> unit
val merge_set_into : State_set.t -> t -> unit
val merge_set_return_new : State_set.t -> t -> State_set.t
val join : t -> Relations_type.Model.t
val join_dropping_relations : t -> Relations_type.Model.t
val exists : (Relations_type.Model.t -> bool) -> t -> bool
val is_empty : t -> bool
val length : t -> int
val empty : unit -> t
val singleton : Relations_type.Model.t -> t
val to_set : t -> State_set.t
(*
val filter : (Relations_type.Model.t -> bool) -> t -> t




val length : t -> int
val nth : t -> int -> Relations_type.Model.t
*)
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
