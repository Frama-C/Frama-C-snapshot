(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Transfer functions for the main domain of the Value analysis. *)

open Eval

type value = Main_values.CVal.t
type location = Main_locations.PLoc.location

module Transfer
    (Valuation: Abstract_domain.Valuation with type value = value
                                           and type origin = bool
                                           and type loc = location)
  : sig

    include Abstract_domain.Transfer
      with type state = Cvalue.Model.t
       and type value := value
       and type location := location
       and type valuation := Valuation.t


    val start_call:
      Cil_types.stmt -> value call -> Valuation.t -> state ->
      state call_action * Base.SetLattice.t

  end


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
