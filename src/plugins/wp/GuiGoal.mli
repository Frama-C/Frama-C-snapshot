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
(* --- PO Details View                                                    --- *)
(* -------------------------------------------------------------------------- *)

class pane : unit ->
  object

    method select : Wpo.t option -> unit
    method update : unit
    method coerce : GObj.widget
    method on_run : (Wpo.t -> VCS.prover -> unit) -> unit
    method on_src : (Wpo.t option -> unit) -> unit

  end
