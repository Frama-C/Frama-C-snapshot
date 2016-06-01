(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

include Callgraph_api.S

module Graphviz_attributes: Graph.Graphviz.GraphWithDotAttrs
  with type t = G.t
  and type V.t = Kernel_function.t
  and type E.t = G.E.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
