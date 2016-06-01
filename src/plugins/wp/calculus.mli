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

(** Generic WP calculus *)

module Cfg(W : Mcfg.S) :
sig

  val compute :
    Cil2cfg.t ->
    WpStrategy.strategy ->
    W.t_prop list * (Format.formatter -> Cil2cfg.edge -> unit)

end
