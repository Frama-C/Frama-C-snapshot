(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Lang.F
type env = Plang.Env.t
type 'a printer = 'a Qed.Plib.printer

(* -------------------------------------------------------------------------- *)
(* --- Sequent Pretty-Printer                                             --- *)
(* -------------------------------------------------------------------------- *)

type target

class focused : Wtext.text ->
  object
    method reset : unit
    method get_focus_mode : bool
    method set_focus_mode : bool -> unit
    method get_state_mode : bool
    method set_state_mode : bool -> unit

    method selected : unit
    method unselect : target
    method restore : target -> unit
    method on_selection : (unit -> unit) -> unit

    method sequent : Conditions.sequent
    method selection : Tactical.selection
    method set_target : Tactical.selection -> unit

    method popup : unit
    method on_popup : (Widget.popup -> unit) -> unit

    method pp_term : term printer
    method pp_pred : pred printer
    method pp_selection : Tactical.selection printer
    method pp_sequent : Conditions.sequent -> Format.formatter -> unit
    method goal : Wpo.t -> Format.formatter -> unit
    method button : title:string -> callback:(unit -> unit) ->
      Format.formatter -> unit
  end
