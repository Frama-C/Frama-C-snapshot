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
(* --- PO List View                                                       --- *)
(* -------------------------------------------------------------------------- *)

class pane : GuiConfig.provers ->
object

  method show : Wpo.t -> unit
  method on_click : (Wpo.t -> VCS.prover option -> unit) -> unit
  method on_right_click : (Wpo.t -> VCS.prover option -> unit) -> unit
  method on_double_click : (Wpo.t -> VCS.prover option -> unit) -> unit
  method reload : unit
  method update : Wpo.t -> unit
  method update_all : unit
  method count_selected : int
  method on_selection : (int -> unit) -> unit
  method iter_selected : (Wpo.t -> unit) -> unit
  method add : Wpo.t -> unit
  method size : int
  method index : Wpo.t -> int 
  method get : int -> Wpo.t
  method coerce : GObj.widget

end
