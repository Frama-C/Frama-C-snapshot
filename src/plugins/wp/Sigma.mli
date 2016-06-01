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
(* --- Generic Sigma Factory                                              --- *)
(* -------------------------------------------------------------------------- *)

module Make
    (C : Memory.Chunk)
    (H : Qed.Collection.S with type t = C.t) :
  Memory.Sigma with type chunk = C.t
                and type domain = H.set
