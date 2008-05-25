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

(* $Id: signature.mli,v 1.2 2008/04/10 15:48:06 uid562 Exp $ *)

module type NAME = sig val name: string end

module type SIZE = sig val size: int end

module type NAME_SIZE = sig
  val name: string
  val size: int
end

(** {2 Signatures to provide to some computation builders} *)

module type NAME_DPDS = sig
  val name: Project.Computation.Name.t
    (** Name of the generated internal state. *)
  val dependencies: Project.Computation.t list
    (** Dependencies of the generated internal state. *)
end

module type NAME_SIZE_DPDS = sig
  include NAME_DPDS
  val size:int
    (** Initial size of the generated internal state. *)
end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
