(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Verification Conditions Interface                                  --- *)
(* -------------------------------------------------------------------------- *)

open VCS

(** {2 Proof Obligations} *)

type t (** elementary proof obligation *)

val get_id : t -> string
val get_model : t -> Model.t
val get_description : t -> string
val get_property : t -> Property.t
val get_result : t -> prover -> result
val get_results : t -> (prover * result) list
val get_logout : t -> prover -> string (** only file name, might not exists *)
val get_logerr : t -> prover -> string (** only file name, might not exists *)
val get_sequent : t -> Conditions.sequent
val get_formula: t -> Lang.F.pred
val is_trivial : t -> bool
val is_proved : t -> bool

(** {2 Database} 
    Notice that a property or a function have no proof obligation until you 
    explicitly generate them {i via} the [generate_xxx] functions below.
*)

val clear : unit -> unit
val proof : Property.t -> t list
(** List of proof obligations computed for a given property. Might be empty if you 
    don't have used one of the generators below. *)
    
val remove : Property.t -> unit
val iter_ip : (t -> unit) -> Property.t -> unit
val iter_kf : (t -> unit) -> ?bhv:string list -> Kernel_function.t -> unit

(** {2 Generators} 
    The generated VCs are also added to the database, so they can be
    accessed later. The default value for [model] is what has been
    given on the command line ([-wp-model] option)
*)

val generate_ip : ?model:string -> Property.t -> t Bag.t
val generate_kf : ?model:string -> ?bhv:string list -> Kernel_function.t -> t Bag.t
val generate_call : ?model:string -> Cil_types.stmt -> t Bag.t

(** {2 Prover Interface} *)
    
val prove : t ->
  ?config:config ->
  ?mode:mode ->
  ?start:(t -> unit) ->
  ?callin:(t -> prover -> unit) ->
  ?callback:(t -> prover -> result -> unit) ->
  prover -> bool Task.task
(** Returns a ready-to-schedule task. *)

val spawn : t ->
  ?config:config ->
  ?start:(t -> unit) ->
  ?callin:(t -> prover -> unit) ->
  ?callback:(t -> prover -> result -> unit) ->
  ?success:(t -> prover option -> unit) ->
  ?pool:Task.pool ->
  (mode * prover) list -> unit
(** Same as [prove] but schedule the tasks into the global server returned 
    by [server] function below. 
    
    The first succeeding prover cancels the other ones. *)

val server : ?procs:int -> unit -> Task.server
(** Default number of parallel tasks is given by [-wp-par] command-line option.
    The returned server is global to Frama-C, but the number of parallel task 
    allowed will be updated to fit the [~procs] or command-line options. *)

val command : t Bag.t -> unit
(** Run the provers with the command-line interface *)

(* -------------------------------------------------------------------------- *)
