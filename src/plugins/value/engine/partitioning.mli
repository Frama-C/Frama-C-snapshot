(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

module type Domain = sig
  include Abstract_domain.Lattice
  include Datatype.S_with_collections with type t = state
  include Abstract_domain.Interface with type t := state
end

module type S = sig
  type state
  type state_set
  type t

  val empty: unit -> t

  val merge_set_return_new: state_set -> t -> state_set
  val join: t -> state or_bottom

  val to_set: t -> state_set
  val to_list: t -> state list

  val pretty : Format.formatter -> t -> unit
end

module Make
    (Domain: Domain)
    (States : Powerset.S with type state = Domain.t)
  : S with type state = Domain.t
       and type state_set = States.t


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
