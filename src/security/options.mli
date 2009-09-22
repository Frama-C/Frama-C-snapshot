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

(* ************************************************************************* *)
(** {2 Security parameters} *)
(* ************************************************************************* *)

open Plugin

include S

module LogicAnnotation: STRING
  (** Which kind of security logical annotations are recognized. *)

module Analysis: BOOL
  (** Whether perform security analysis or not. *)

module Lattice: STRING
  (** Security lattice to use. *)

module PropagateAssertions: BOOL
  (** Propagate security assertions when possible. *)

module Slicing: BOOL
  (** Perform the security slicing pre-analysis. *)
  
val is_on: unit -> bool

val get_selection_after_slicing: unit -> Project.Selection.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
