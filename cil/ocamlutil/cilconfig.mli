(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)


(** Reading and storing configuration files from the filesystem. Currently
    only used in Frama-C's GUI.*)


(************************************************************************ *)
(** {2 Configuration} *)
(* ************************************************************************)

(** The configuration data can be of several types **)
type configData =
    ConfInt of int
  | ConfBool of bool
  | ConfFloat of float
  | ConfString of string
  | ConfList of configData list

(** Load the configuration from a file *)
val loadConfiguration: string -> unit

(** Save the configuration in a file. Overwrites the previous values *)
val saveConfiguration: string -> unit

(** Clear all configuration data *)
val clearConfiguration: unit -> unit

(** Set a configuration element, with a key. Overwrites the previous values *)
val setConfiguration: string -> configData -> unit

(** Find a configuration elements, given a key. Raises Not_found if it cannot
    * find it *)
val findConfiguration: string -> configData

(** Like findConfiguration but extracts the integer *)
val findConfigurationInt: string -> int

(** Looks for an integer configuration element, and if it is found, it uses
 * the given function. Otherwise, does nothing *)
val useConfigurationInt: string -> (int -> unit) -> unit

val findConfigurationFloat: string -> float
val useConfigurationFloat: string -> (float -> unit) -> unit

val findConfigurationBool: string -> bool
val useConfigurationBool: string -> (bool -> unit) -> unit

val findConfigurationString: string -> string
val useConfigurationString: string -> (string -> unit) -> unit

val findConfigurationList: string -> configData list
val useConfigurationList: string -> (configData list -> unit) -> unit


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
