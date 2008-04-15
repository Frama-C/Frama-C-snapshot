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

(* $Id: kf_state.ml,v 1.14 2008/10/03 13:09:16 uid568 Exp $ *)

module type S = sig
  type data
  val memo: 
    (Db_types.kernel_function -> data) -> Db_types.kernel_function -> data
  val self: Project.Computation.t
end

module type INFO = sig
  val name: string
  val dependencies: Project.Computation.t list
    (** Additional dependencies of the built state. *)
end

module Build(X:Project.Datatype.S)(Info:Signature.NAME_DPDS) = struct
  
  include Kernel_function.Make_Table(X)(struct include Info let size = 97 end)

  let memo f = memo f
    (* eta-expansion in order to ignore the optional argument of [memo] *)

end

module Make = Build(Locations.Zone.Datatype)
module Context = Build(Inout_type.Datatype)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
