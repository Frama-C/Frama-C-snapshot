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

include Plugin.S

val name: string
module Filename: Parameter_sig.String
module Init_func: Parameter_sig.Kernel_function_set
module Uncalled: Parameter_sig.Bool
module Uncalled_leaf: Parameter_sig.Bool
module Services: Parameter_sig.Bool

val dump: (out_channel -> 'a -> unit) -> 'a -> unit
(** dump the given value into [Filename.get ()] by using [output] *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
