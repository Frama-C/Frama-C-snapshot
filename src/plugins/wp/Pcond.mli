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

open Qed.Plib
open Conditions

(** {2 All-in-one printers} *)

val dump : bundle printer
val bundle : ?clause:string -> bundle printer
val sequence : ?clause:string -> sequence printer
val pretty : sequent printer

(** {2 Low-level API} *)

open Lang.F
type env = Plang.Env.t

val alloc_hyp : Plang.pool -> (var -> unit) -> sequence -> unit
val alloc_seq : Plang.pool -> (var -> unit) -> sequent -> unit

(** Sequent Printer Engine. Uses the following [CSS]:
    - ["wp:clause"] for all clause keywords
    - ["wp:comment"] for descriptions
    - ["wp:warning"] for warnings
    - ["wp:property"] for properties
    - ["wp:"]
*)

class engine : #Plang.engine ->
  object

    (** {2 Printer Components} *)
    method name : env -> term -> string (** Generate a name for marked term *)
    method mark : marks -> step -> unit (** Marks terms to share in step *)
    method pp_clause : string printer (** Default: ["@{<wp:clause>...}"] *)
    method pp_stmt : string printer (** Default: ["@{<wp:stmt>...}"] *)
    method pp_comment : string printer (** Default: ["@{<wp:comment>(* ... *)}"] *)
    method pp_property : Property.t printer (** Default: ["@{<wp:property>(* ... *)}"] *)
    method pp_warning : Warning.t printer (** Default: ["@{<wp:warning>Warning}..."] *)
    method pp_name : string printer (** Default: [Format.pp_print_string] *)
    method pp_core : term printer (** Default: [plang#pp_sort] *)

    method pp_definition : Format.formatter -> string -> term -> unit
    method pp_intro : step:step -> clause:string -> ?dot:string -> pred printer
    method pp_condition : step:step -> condition printer
    method pp_block : clause:string -> sequence printer
    method pp_goal : pred printer

    method pp_step : step printer
    (** Assumes an "<hv>" box is opened. *)

    method pp_sequence : clause:string -> sequence printer
    (** Assumes an "<hv>" box is opened {i and} all variables are declared.
        (recursively used) *)

    method pp_sequent : sequent printer
    (** Print the sequent in global environment. *)

    method pp_esequent : env -> sequent printer
    (** Print the sequent in the given environment.
        The environment is enriched with the shared terms. *)

  end

(* -------------------------------------------------------------------------- *)
(* --- State-Aware Printers                                               --- *)
(* -------------------------------------------------------------------------- *)

class state :
  object
    inherit Plang.engine
    inherit Pcfg.engine
    method clear : unit
    method set_sequence : Conditions.sequence -> unit
    method set_domain : Vars.t -> unit (** Default is sequence's domain *)
    method domain : Vars.t
    method label_at : id:int -> Pcfg.label
    method updates : Pcfg.label Memory.sequence -> Memory.update Bag.t
    method pp_at : Format.formatter -> Pcfg.label -> unit
    method pp_update : Pcfg.label -> Format.formatter -> Memory.update -> unit
    method pp_value : Format.formatter -> term -> unit
  end

class sequence : #state ->
  object
    inherit engine
    method set_sequence : Conditions.sequence -> unit
    (** Initialize state with this sequence *)
    method set_goal : pred -> unit
    (** Adds goal to state domain *)
    method set_sequent : sequent -> unit
    (** Set sequence and goal *)
    method get_state : bool
    (** If [true], states are rendered when printing sequences. *)
    method set_state : bool -> unit
    (** If set to [false], states rendering is deactivated. *)
  end
