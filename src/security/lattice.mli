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

(* $Id: lattice.mli,v 1.10 2008-04-01 09:25:22 uid568 Exp $ *)

module type S = sig
  include Abstract_interp.Lattice
  type annotation = string
  val possible_annotations: annotation list
  val annotations2state: t -> annotation list -> t
    (** [annotations2state dft l] returns the state corresponding to the given
	annotations. Return [dft] if there is no annotation. *)

  val constant: t
    (** State for a constant. *)

  val variable: t 
    (** Default state for a variable. *)

  val use_ctrl_dependencies: bool
    (** Are control dependencies relevant? *)
end

module Weak: S
module Medium: S
module Strong: S

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
