(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

open Cil_types
open Lang.F
type env = Plang.Env.t

val xmark_hyp : Plang.pool -> (var -> unit) -> sequence -> unit
val xmark_seq : Plang.pool -> (var -> unit) -> sequent -> unit

class engine : Plang.engine -> 
  object
    (** {2 Printer Components} *)
    method name : env -> term -> string (** Generate a name for marked term *)
    method mark : marks -> step -> unit (** Marks terms to share in step *)
    method pp_clause : string printer (** Default: ["@{<wp:clause>...}"] *)
    method pp_comment : string printer (** Default: ["@{<wp:comment>(* ... *)}"] *)
    method pp_property : Property.t printer (** Default: ["@{<wp:property>(* ... *)}"] *)
    method pp_warning : Warning.t printer (** Default: ["@{<wp:warning>Warning}..."] *)
    method pp_name : string printer (** Default: [Format.pp_print_string] *)
    method pp_core : term printer (** Default: [plang#pp_sort] *)

    method pp_definition : Format.formatter -> string -> term -> unit
    method pp_intro : step:step -> clause:string -> ?dot:string -> pred printer
    method pp_condition : step:step -> condition printer
        
    method pp_step : step printer
    (** Assumes an "<hv>" box is opened. *)
        
    method pp_sequence : clause:string -> sequence printer
    (** Assumes an "<hv>" box is opened. *)

    method pp_sequent : sequent printer
    (** Print the sequent in global environment. *)
    
    method pp_esequent : env -> sequent printer
    (** Print the sequent in the given environment.
        The environment is enrich with the shared terms. *)
        
  end
    
