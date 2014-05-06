(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open Cil_types

type call_site = kernel_function * kinstr

module Callsite = struct
  include Datatype.Pair_with_collections(Kernel_function)(Cil_datatype.Kinstr)
    (struct let module_name = "Value_callbacks.Callpoint" end)

  let pretty fmt (kf, ki) =
    Format.fprintf fmt "%a@@%t" Kernel_function.pretty kf
      (fun fmt ->
        match ki with
        | Kglobal -> Format.pp_print_string fmt "<main>"
        | Kstmt stmt -> Format.pp_print_int fmt stmt.sid
      )
end


type callstack = call_site list

module Callstack = Datatype.With_collections
  (Datatype.List(Callsite))
  (struct let module_name = "Value_types.Callstack" end)

type 'a callback_result =
  | Normal of 'a
  | NormalStore of 'a * int
  | Reuse of int

type cacheable =
  | Cacheable
  | NoCache
  | NoCacheCallers


type call_result = {
  c_values: (Cvalue.V_Offsetmap.t option * Cvalue.Model.t) list;
  c_clobbered: Base.SetLattice.t;
  c_cacheable: cacheable;
}

type logic_dependencies = Locations.Zone.t Cil_datatype.Logic_label.Map.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)

