(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Source Interaction for WP                                          --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types

type selection =
  | S_none
  | S_fun of Kernel_function.t
  | S_prop of Property.t
  | S_call of call

and call = {
  s_caller : Kernel_function.t ;
  s_called : Kernel_function.t ;
  s_stmt : stmt ;
}

class popup : unit ->
  object
    method on_click : (selection -> unit) -> unit
    method on_prove : (selection -> unit) -> unit
    method register :
      GMenu.menu GMenu.factory ->
      Design.main_window_extension_points ->
      button:int -> Pretty_source.localizable -> unit
  end

class highlighter : Design.main_window_extension_points ->
  object

    method set : Wpo.t option -> unit
    method update : unit
    method highlight :
      Design.reactive_buffer -> Pretty_source.localizable ->
      start:int -> stop:int -> unit

  end


