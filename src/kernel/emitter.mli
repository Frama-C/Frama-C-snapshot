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

(** Emitter. An emitter is the Frama-C entity which is able to emit annotations
    and property status. Thus you have to create (at least) one of your own if
    you want to do such tasks.  
    @since Nitrogen-20111001 *)

(**************************************************************************)
(** {2 API for Plug-ins Developers} *)
(**************************************************************************)

type emitter

include Datatype.S_with_collections with type t = emitter

val create: 
  string -> correctness:Parameter.t list -> tuning:Parameter.t list -> t
(** [Emitter.create name ~correctness ~tuning] creates a new emitter with the
    given name. The given parameters are the ones which impact the generated
    annotations/status. A "correctness" parameter may fully change a generated
    element when its value changes (for instance, a valid status
    may become invalid and conversely). A "tuning" parameter may improve a
    generated element when its value changes (for instance, a "dont_know" status
    may become valid or invalid, but a valid status cannot become invalid)
    @raise Invalid_argument if an emitter with the given name already exist *)

val get_name: t -> string

val correctness_parameters: t -> string list
val tuning_parameters: t -> string list

(** Usable emitters are the ones which can really emit something.
    Use {!get} to get the one corresponding to a (standard) emitter. *)
module Usable_emitter: sig
  include Datatype.S_with_collections
  val get_name: t -> string
  val get_unique_name: t -> string
  val compare_with_emitter: t -> emitter -> int
  val correctness_parameters: t -> string list
  val tuning_parameters: t -> string list
  val pretty_parameter: Format.formatter -> tuning:bool -> t -> string -> unit
(** Pretty print the parameter (given by its name) with its value.
    @raise Not_found if the parameter is not one of the given emitter *)
end

(* ********************************************************************** *)
(** {2 Kernel Internal API} *)
(* ********************************************************************** *)

val get: t -> Usable_emitter.t
(** Get the emitter which is really able to emit something. 
    This function must be called at the time of the emission. No action must
    occur between the call to [get] and the emission (in particular no update
    of any parameter of the emitter. *)

val self: State.t
val property_status_state: State.t ref

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
