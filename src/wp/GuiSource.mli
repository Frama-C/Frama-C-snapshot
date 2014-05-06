(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
  method highlight : GSourceView2.source_buffer -> Pretty_source.localizable ->
    start:int -> stop:int -> unit

end


