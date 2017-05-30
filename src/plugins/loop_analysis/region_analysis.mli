(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(* An algorithm for region analysis, similar to the one in the
   dragon book ("Compilers: Principles, Techniques, and Tools (2nd
   Edition)", by Aho, Lam, Sethi and Ullman).

   The main difference compared to dataflow analysis is the handling
   of loops: the "mu" construction for handling loops allows to
   perform different computations, especially they can perform actions
   when first entering the loop or after the fixpoint has been
   reached.

   TODO: The algorithm does not handle non-natural loops for now. *)
include module type of Region_analysis_sig;;

module Make(N:Node):sig
  (* Function computing from an entry abstract value the "after"
     state, which is a map from each outgoing edge to its respective
     value. *)
  val after: N.abstract_value -> N.abstract_value N.Edge_Dict.t
end
