(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(** Proof Script Database                                                     *)
(* -------------------------------------------------------------------------- *)

(** {2 Database} *)

val delete_script : string -> unit
val add_script : string -> string list -> string -> string -> unit
(** [new_script goal keys proof qed] registers the script [proof] terminated by [qed]
    for goal [goal] and keywords [keys] *)

val find_script_for_goal : string -> (string * string) option
(** Retrieve script file for one specific goal.
    The file specified by [-wp-script f] is loaded if necessary. *)

val update_hints_for_goal : string -> string list -> unit
(** Update the hints for one specific goal. The script file will be saved if hints
    are different. *)

val find_script_with_hints : string list -> string list -> (int * string * string * string) list
(** Retrieve matchable script files for w.r.t provided required and hints keywords.
    Most suitable scripts comes first, with format [(n,g,p,q)] where [p,q] is a script
    matching [n] hints from possibly deprecated goal [g]. *)

val clear : unit -> unit

val loadscripts : unit -> unit
(** Load scripts from [-wp-script f]. Automatically invoked by [find_xxx] unless
    [loadscripts] flags is unset. *)

val savescripts : unit -> unit
(** If necessary, dump the scripts database into the file
    specified by [-wp-script f]. *)

(** {2 Low-level Parsers and Printers} *)

val is_empty : string -> bool

val parse_coqproof : string -> (string * string) option
(** [parse_coqproof f] parses a coq-file [f] and fetch the first proof. *)

val parse_scripts : string -> unit
(** [parse_scripts f] parses all scripts from file [f] and put them in the database. *)

val dump_scripts : string -> unit
(** [dump_scripts f] saves all scripts from the database into file [f]. *)

(* -------------------------------------------------------------------------- *)
(** Proof Script Interaction                                                  *)
(* -------------------------------------------------------------------------- *)

open WpPropId

val script_for : pid:prop_id -> gid:string -> (string * string) option
val script_for_ide : pid:prop_id -> gid:string -> string * string
val hints_for : pid:prop_id -> (string * string * string) list
