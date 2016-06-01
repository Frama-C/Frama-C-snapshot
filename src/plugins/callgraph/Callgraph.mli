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

(** Callgraph plugin. *)

module Options: sig
  module Filename: Parameter_sig.String
  module Init_func: Parameter_sig.Kernel_function_set
  module Uncalled: Parameter_sig.Bool
  module Uncalled_leaf: Parameter_sig.Bool
  module Services: Parameter_sig.Bool
end

module Cg: module type of Cg
(** The callgraph itself *)

module Services: module type of Services
(** The graph of services built on top of the callgraph *)

module Uses: module type of Uses
(** Several useful functions over the callgraph *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
