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
