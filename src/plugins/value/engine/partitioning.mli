(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

open Eval

module type StateSet = sig
  type state
  type t

  val empty: t
  val is_empty: t -> bool
  val singleton: state -> t
  val singleton': state or_bottom -> t
  val uncheck_add: state -> t -> t
  val add: state -> t -> t
  val add': state or_bottom -> t -> t

  val length: t -> int

  val merge: into:t -> t -> t * bool
  val join: ?into:state or_bottom -> t -> state or_bottom
  val fold: (state -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (state -> unit) -> t -> unit
  val map: (state -> state) -> t -> t

  val reorder: t -> t
  val of_list: state list -> t
  val to_list: t -> state list

  val pretty : Format.formatter -> t -> unit
end

module Make_Set
    (Domain: Abstract_domain.S)
  : StateSet with type state = Domain.t


module type Partition = sig
  type state
  type state_set
  type t

  val empty: unit -> t

  val fold: (state -> 'a -> 'a) -> t -> 'a -> 'a

  val merge_set_return_new: state_set -> t -> state_set
  val join: t -> state or_bottom

  val to_set: t -> state_set
  val to_list: t -> state list

  val pretty : Format.formatter -> t -> unit
end

module Make_Partition
    (Domain: Abstract_domain.External)
    (States : StateSet with type state = Domain.t)
  : Partition with type state = Domain.t
               and type state_set = States.t


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
