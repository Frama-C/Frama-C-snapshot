(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(* $Id: kf_state.mli,v 1.10 2008-04-01 09:25:20 uid568 Exp $ *)

module type S = sig
  type data
  val memo: 
    (Db_types.kernel_function -> data) -> Db_types.kernel_function -> data
  val self: Project.Computation.t
end

(** Build an internal state for inputs, outputs and derefs. *)
module Make(Info:Signature.NAME_DPDS) : S with type data = Locations.Zone.t

(** Build an internal state for inout context. *)
module Context(Info:Signature.NAME_DPDS) 
  : S with type data = Inout_type.t

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
