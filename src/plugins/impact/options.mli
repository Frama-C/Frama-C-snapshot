(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

include Plugin.S

module Pragma: Parameter_sig.Kernel_function_set
  (** Use pragmas of given function. *)

module Print: Parameter_sig.Bool
  (** Print the impacted stmt on stdout. *)

module Reason: Parameter_sig.Bool
  (** Build the graphs that explains why a node is impacted. *)

module Slicing: Parameter_sig.Bool
  (** Slicing from the impacted stmt. *)

module Skip: Parameter_sig.String_set
  (** Consider that the variables in the string are not impacted *)

module Upward: Parameter_sig.Bool
  (** Also compute impact within callers *)

val is_on: unit -> bool

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
