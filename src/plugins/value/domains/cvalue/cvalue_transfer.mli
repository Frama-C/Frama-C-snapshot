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

(** Transfer functions for the main domain of the Value analysis. *)

open Eval

type value = Main_values.CVal.t
type location = Main_locations.PLoc.location

val find_right_value:
  Cil_types.typ -> Cvalue.V_Offsetmap.t -> value or_bottom flagged_value

module Transfer
    (Valuation: Abstract_domain.Valuation with type value = value
                                           and type origin = bool
                                           and type loc = location)
  : sig

    include Abstract_domain.Transfer
      with type state = Cvalue.Model.t
       and type summary = Cvalue.V_Offsetmap.t option
       and type value := value
       and type location := location
       and type valuation := Valuation.t


    val call_action:
      Cil_types.stmt -> value call -> Valuation.t -> state ->
      (state, summary, value) action * Base.SetLattice.t

  end


(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
