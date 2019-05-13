(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(** Coq Proof Scripts *)
(* -------------------------------------------------------------------------- *)

open WpPropId

val is_empty_script : string -> bool
(** Check a proof script text for emptyness *)

val delete_script_for : gid:string -> unit
(** [delete_script ~gid] remove known script for goal. *)

val add_script_for : gid:string -> string list -> string -> string -> unit
(** [new_script goal keys proof qed] registers the script [proof]
    terminated by [qed] for goal [gid] and keywords [keys] *)

val parse_coqproof : string -> (string * string) option
(** [parse_coqproof f] parses a coq-file [f] and fetch the first proof. *)

val savescripts : unit -> unit
(** If necessary, dump the scripts database into the file
    specified by [-wp-script f]. *)

val script_for : pid:prop_id -> gid:string -> legacy:string -> (string * string) option
val script_for_ide : pid:prop_id -> gid:string -> legacy:string -> string * string
val hints_for : pid:prop_id -> (string * string * string) list
