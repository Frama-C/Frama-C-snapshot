(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: journal.mli,v 1.15 2009-01-28 14:34:54 uid568 Exp $ *)

(** Journalization of functions *)

(* ****************************************************************************)
(** {2 Journal management} *)
(* ****************************************************************************)

val get_name: unit -> string
  (** @return the filename which the journal will be written into. *)

val set_name: string -> unit
  (** [set_journal_name name] changes the filename into the journal will be
      written. The new filename is [name ^ ".ml"]. *)

val write: unit -> unit
  (** [write ()] writes the content of the journal into the given file 
      (without clearing the journal). *)

val prevent: ('a -> 'b) -> 'a -> 'b

val save: unit -> unit
  (** Save the current state of the journal for future restauration.
      @since Beryllium-20090901 *)

val restore: unit -> unit
  (** Restore a previously saved journal.
      @since Beryllium-20090901 *)

(* ****************************************************************************)
(** {2 Journalization} *)
(* ****************************************************************************)

val register: 
  string -> 
  'a Type.t ->
  ?comment:(Format.formatter -> unit) ->
  ?is_dyn:bool ->
  'a -> 
  'a
    (** [register n ty ~comment:pp ~register:r v] journalizes the value [v] 
	of type [ty] with the name [n]. [n] must exactly  match the FQN
	of the value (i.e. "List.iter" and not "iter" even though the module
	List is already opened). If the [comment] argument is set, the given 
	pretty printer [pp]would be applied when the is journalized. 

	Set [is_dyn] to true to journalize dynamic function. *)

val never_write: string -> 'a -> 'a
  (** [never_write name f] returns a closure [g] observationaly equal to [f]
      except that trying to write a call to [g] in the journal is an error. If
      [f] is not a closure, then [never_write name f] raises
      [Invalid_argument]. *)

(* ****************************************************************************)
(** {2 Internal use only} *)
(* ****************************************************************************)

val keep_file: string -> unit

(** This function has not to be used explictely. Only offers functions 
    retrieving when running a journal file. *)

exception LoadingError of string

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
