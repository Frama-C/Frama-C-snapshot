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

open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Selection Composer                                                 --- *)
(* -------------------------------------------------------------------------- *)

class type composer =
  object
    method title : string
    method descr : string
    method target : selection
    method ranged : bool
    method is_valid : selection -> bool
    method get_value : selection
    method set_value : selection -> unit
  end

(* -------------------------------------------------------------------------- *)
(* --- Search                                                             --- *)
(* -------------------------------------------------------------------------- *)

class type browser =
  object
    method title : string
    method descr : string
    method target : selection
    method search : (unit named -> unit) -> int -> unit
    method choose : string option -> unit
  end

(* -------------------------------------------------------------------------- *)
(* --- Tactical Dongle                                                    --- *)
(* -------------------------------------------------------------------------- *)

class tactic : Tactical.t -> (Format.formatter -> Tactical.selection -> unit) ->
  object
    inherit Wpalette.tool
    inherit feedback
    method clear : unit
    method targeted : bool
    method select :
      process:(tactical -> selection -> process -> unit) ->
      browser:(browser -> unit) ->
      composer:(composer -> unit) ->
      selection -> unit
  end

class strategies : unit ->
  object
    inherit Wpalette.tool
    method register : Strategy.heuristic -> unit
    method connect : (Strategy.heuristic list -> unit) option -> unit
  end
