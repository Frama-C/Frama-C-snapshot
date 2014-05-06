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

(** Emitter. An emitter is the Frama-C entity which is able to emit annotations
    and property status. Thus you have to create (at least) one of your own if
    you want to do such tasks.  
    @since Nitrogen-20111001 
    @plugin development guide *)

(**************************************************************************)
(** {2 API for Plug-ins Developers} *)
(**************************************************************************)

type emitter
type kind = Property_status | Alarm | Code_annot | Funspec | Global_annot
(** When selecting [Alarm], [Code_annot] is also automatically selected *)

include Datatype.S_with_collections with type t = emitter

val create: 
  string -> 
  kind list -> 
  correctness:Typed_parameter.t list -> 
  tuning:Typed_parameter.t list -> t
(** [Emitter.create name kind ~correctness ~tuning] creates a new emitter with
    the given name. The given parameters are the ones which impact the generated
    annotations/status. A "correctness" parameter may fully change a generated
    element when its value changes (for instance, a valid status may become
    invalid and conversely). A "tuning" parameter may improve a generated
    element when its value changes (for instance, a "dont_know" status may
    become valid or invalid, but a valid status cannot become invalid).
    The given name must be unique.
    @raise Invalid_argument if an emitter with the given name already exist 
    @plugin development guide *)

val get_name: t -> string

val correctness_parameters: t -> string list
val tuning_parameters: t -> string list

val end_user: t
(** The special emitter corresponding to the end-user. Only the kernel should
    use this emitter when emitting annotations or statuses.
    @since Oxygen-20120901 *)

val kernel: t
(** The special emitter corresponding to the kernel. Only the kernel should
    use this emitter when emitting annotations or statuses.
    @since Oxygen-20120901 *)

(** Usable emitters are the ones which can really emit something. *)
module Usable_emitter: sig
  include Datatype.S_with_collections
  val get: t -> emitter
  (** Get the emitter from an usable emitter. Not so efficient. 
      @since Oxygen-20120901 *)

  val get_name: t -> string
  val get_unique_name: t -> string
  val correctness_parameters: t -> string list
  val tuning_parameters: t -> string list
  val pretty_parameter: Format.formatter -> tuning:bool -> t -> string -> unit
(** Pretty print the parameter (given by its name) with its value.
    @raise Not_found if the parameter is not one of the given emitter *)
end

val distinct_tuning_parameters: Usable_emitter.t -> Datatype.String.Set.t
(** Return the tuning parameter which distinguishes this usable emitter from the
    other ones.
    @since Oxygen-20120901 *)

val distinct_correctness_parameters: Usable_emitter.t -> Datatype.String.Set.t
(** Return the correctness_parameters which distinguishes this usable emitter
    from the other ones.
    @since Oxygen-20120901 *)

(* ********************************************************************** *)
(** {2 Kernel Internal API} *)
(* ********************************************************************** *)

val get: t -> Usable_emitter.t
(** Get the emitter which is really able to emit something. 
    This function must be called at the time of the emission. No action must
    occur between the call to [get] and the emission (in particular no update
    of any parameter of the emitter. *)

val self: State.t

(** Table indexing: key -> emitter (or equivalent data) -> value.
    Quick access + handle cleaning in the right way (only remove relevant
    bindings when required. 
    @since Oxygen-20120901 *)
module Make_table
  (H: Datatype.Hashtbl)
  (E: sig 
    include Datatype.S_with_collections
    val local_clear: H.key -> 'a Hashtbl.t -> unit
    val usable_get: t -> Usable_emitter.t 
    val get: t -> emitter
  end)
  (D: Datatype.S) 
  (Info: sig include State_builder.Info_with_size val kinds: kind list end) : 
sig
  type internal_tbl = D.t E.Hashtbl.t
  val self: State.t
  val add: H.key -> internal_tbl -> unit
  val find: H.key -> internal_tbl
  val mem: H.key -> bool
  val iter: (H.key -> internal_tbl -> unit) -> unit
  val fold: (H.key -> internal_tbl -> 'a -> 'a) -> 'a -> 'a
  val remove: H.key -> unit
  val add_hook_on_remove: (E.t -> H.key -> D.t -> unit) -> unit
(** Register a hook to be applied whenever a binding is removed from the table.
    @since Fluorine-20130401 *)
  val apply_hooks_on_remove: E.t -> H.key -> D.t -> unit
(** This function must be called on each binding which is removed from the
    table without directly calling the function {!remove}.
    @since Fluorine-20130401 *)
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
