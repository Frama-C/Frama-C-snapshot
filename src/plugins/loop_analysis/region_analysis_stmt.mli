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

open Region_analysis

(* Helper function to make region analysis on Frama-C stmts. Produces
   a Node suitable as an argument to the [Region_analysis.Make]
   functor.*)
module MakeNode(M:sig
    val kf: Kernel_function.t

    open Cil_types
    type abstract_value
    val compile_node: stmt -> abstract_value -> (stmt edge * abstract_value) list
    val mu: (abstract_value -> abstract_value) -> abstract_value -> abstract_value
    val join: abstract_value list -> abstract_value

  end):Node with type abstract_value = M.abstract_value
             and type node = Cil_types.stmt
