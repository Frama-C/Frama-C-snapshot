(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

module type S = sig
  type data
  val memo:
    (Cil_types.kernel_function -> data) -> Cil_types.kernel_function -> data
  val self: State.t
end

module Build(X: Datatype.S)(Info: State_builder.Info) = struct
  include Kernel_function.Make_Table(X)(struct include Info let size = 97 end)
  let memo f = memo f
    (* eta-expansion in order to ignore the optional argument of [memo] *)
end

module Make = Build(Locations.Zone)
module Context = Build(Inout_type)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
