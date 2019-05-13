(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

open Cil_types
open Eval

val current_kf_inout: unit -> Inout_type.t option

module type S = sig

  type state
  type value
  type location

  val assign: state -> kinstr -> lval -> exp -> state or_bottom

  val assume: state -> stmt -> exp -> bool -> state or_bottom

  val call:
    stmt -> lval option -> exp -> exp list -> state ->
    state list or_bottom * Value_types.cacheable

  val check_unspecified_sequence:
    Cil_types.stmt ->
    state ->
    (* TODO *)
    (stmt * lval list * lval list * lval list * stmt ref list) list ->
    unit or_bottom

  val enter_scope: kernel_function -> varinfo list -> state -> state

  type call_result = {
    states: state list or_bottom;
    cacheable: Value_types.cacheable;
    builtin: bool;
  }

  val compute_call_ref:
    (stmt -> (location, value) call -> state -> call_result) ref
end

module Make (Abstract: Abstractions.Eva)
  : S with type state = Abstract.Dom.t
       and type value = Abstract.Val.t
       and type location = Abstract.Loc.location

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
