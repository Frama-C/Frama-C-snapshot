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

open Cil_types
open Eval

module type Domain = sig
  include Datatype.S_with_collections

  val filter_by_bases: Base.Hptset.t -> t -> t
  val reuse: current_input:t -> previous_output:t -> t
end

(** Counter that must be used each time a new call is analyzed, in order
    to refer to it later *)
val new_counter : unit -> int

(** Clean all previously stored results *)
val cleanup_results: unit -> unit

module Make
    (Value : Datatype.S)
    (Domain : Domain)
  : sig

    (** [store_computed_call kf init_state args call_results] memoizes the fact
        that calling [kf] with initial state [init_state] and arguments [args]
        resulted in the results [call_results]. Those information are intended
        to be reused in subsequent calls *)
    val store_computed_call:
      kernel_function -> Domain.t -> Value.t or_bottom list ->
      Domain.t list or_bottom ->
      unit

    (** [reuse_previous_call kf init_state args] searches amongst the previous
        analyzes of [kf] one that matches the initial state [init_state] and the
        values of arguments [args]. If none is found, [None] is returned.
        Otherwise, the results of the analysis are returned, together with the
        index of the matching call. (This last information is intended to be used
        by the plugins that have registered Value callbacks.) *)
    val reuse_previous_call:
      kernel_function -> Domain.t -> Value.t or_bottom list ->
      (Domain.t list or_bottom * int) option
  end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
