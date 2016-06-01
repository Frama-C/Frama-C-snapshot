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
(* --- WP Calculus                                                        --- *)
(* -------------------------------------------------------------------------- *)

module VC( M : Memory.Model ) : Mcfg.S
module Computer( M : Memory.Model ) :
sig
  class wp : Model.t -> Generator.computer
end

val computer : Factory.setup -> Factory.driver -> Generator.computer
