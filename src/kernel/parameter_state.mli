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

(* ************************************************************************* *)
(** {2 Handling groups of parameters} *)
(* ************************************************************************* *)

val get_selection_context: ?is_set:bool -> unit -> State_selection.t
(** Selection of all the parameters which may have an impact on some
    analysis. *)

val get_selection: ?is_set:bool -> unit -> State_selection.t
  (** Selection of all the settable parameters.
      [is_set] is [true] by default (for backward compatibility): in such a
      case, for each option, the extra internal state indicating whether it is
      set also belongs to the selection.
      @plugin development guide *)

val get_reset_selection: ?is_set:bool -> unit -> State_selection.t
  (** Selection of resettable parameters in case of copy with a visitor.
      Not for casual user.
      @since Neon-20130301 *)

(**/**)

val extend_selection: bool -> State.t -> unit
val extend_no_reset_selection: bool -> State.t -> unit

(* ************************************************************************* *)
(** {2 Generic implementation of command line option} *)
(* ************************************************************************* *)

module Make
  (P: sig val shortname: string end)
  (X:sig
    include Datatype.S
    val default: unit -> t
    val option_name: string
    val functor_name: string
   end):
sig
  include Parameter_sig.S_no_parameter with type t = X.t
  module Is_set: State_builder.S
  val group: Cmdline.Group.t
  val stage: Cmdline.stage
  val is_visible: bool
  val is_dynamic: bool
  val register_dynamic: 
    string -> 'arg Type.t -> 'ret Type.t -> ('arg -> 'ret) -> 'arg -> 'ret
  val gen_journalized: string -> 'arg Type.t -> ('arg -> unit) -> 'arg -> unit
end

(**/**)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
