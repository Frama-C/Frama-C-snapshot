(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: journal.mli,v 1.8 2008/08/27 13:27:02 uid568 Exp $ *)

(** Journalization of functions *)

(* ****************************************************************************)
(** {2 Journal management} *)
(* ****************************************************************************)

val get_name: unit -> string
  (** @return the filename which the journal will be written into. *)

val set_name: string -> unit
  (** [set_journal_name name] changes the filename into the journal will be
      written. The new filename is [name ^ ".ml"]. *)

val start: unit -> unit
  (** [start ()] starts writing in the journal [j]. *)

val stop: unit -> unit
  (** [pause ()] stops writing in the journal [j]. *)

val clear: ?restart:bool -> unit -> unit
  (** [clear ()] clears the data which has been written yet in the journal.
      If restart argument is set to [false], there is no more journal 
      continued after clearing (default is [true]). *)

val write: unit -> unit
  (** [write ()] writes the content of the journal into the given file 
      (without clearing the journal). *)

val push: ('a, Format.formatter, unit) format -> 'a
  (** [push fmt arg1 ... argN] works like Format.printf (output in current
      journal). *)

val is_running: unit -> bool
  (** @return true if the journal is running, false otherwise *)

(* ****************************************************************************)
(** {2 Type pretty printers registration} *)
(* ****************************************************************************)

exception PrettyPrinterNotRegistered of string

val register_printer: 'a Type.t -> (Format.formatter -> 'a -> unit) -> unit
  (** [register ty pp] associate the pretty printer [pp] to the type [ty] *)

module type POLY_INPUT = sig
  
  include Type.POLY_OUTPUT

  val fprintf: (Format.formatter -> 'a -> unit) -> 
    Format.formatter -> 'a poly -> unit
    (** [fprintf pp fmt p] prints the journalization result of the polymorph
	container. [pp] is a pretty printer for elements type. *)
end

module type POLY_OUTPUT = sig

  type 'a poly

  val register_printer: 'a Type.t -> unit
    (** [register ty] register the container with elements of type [ty]. 
	
	@raise PrettyPrinterNotRegistered if the given type does not have a
	pretty printer registered for it before. *)
    
end

module Polymorphic (X:POLY_INPUT) : POLY_OUTPUT with type 'a poly = 'a X.poly

module List : POLY_OUTPUT with type 'a poly = 'a list

(* ****************************************************************************)
(** {2 Journalization} *)
(* ****************************************************************************)

exception NotJournalizable

val register: 
  string -> 
  'a Type.t -> 
  ?comment:(Format.formatter -> unit) ->
  ?use_apply:bool ->
  'a -> 
  'a
    (** [register n ty ~comment:pp ~register:r v] journalizes the value [v] 
	of type [ty] with the name [n]. [n] must exactly  match the FQN
	of the value (i.e. "List.iter" and not "iter" even though the module
	List is already opened). If the [comment] argument is set, the given 
	pretty printer [pp]would be applied when the is journalized. 

	If [use_apply] is set to true, then the given value will be used in the
	journal through [Journal.apply] and not directly through its name
	(especially useful when the function is not present in any interface). 
	
	@raise PrettyPrinterNotRegistered if the given type [ty] does not have a
	pretty printer registered for it before. In the case of a function the
	pretty printers of input arguments types must be registered too. 
	@raise NotJournalizable when the function could not be journalized. 
	For instance, when an unjournalized function is passed as argument. *)

(* ****************************************************************************)
(** {2 Internal use only} *)
(* ****************************************************************************)

(** This function has not to be used explictely. Only offers functions 
    retrieving when running a journal file. *)

exception NotJournalized of string
exception LoadingError of string

val run: unit -> unit
val finished: unit -> unit
val apply: string -> 'a

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
