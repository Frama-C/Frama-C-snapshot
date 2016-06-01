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

(** Evaluation of functions using their specification *)

(** Evaluate [kf] in state [with_formals], first by reducing by the
    preconditions, then by evaluating the assigns, then by reducing
    by the post-conditions. This function should be used when there is
    a single behavior (but is always correct).  *)
val compute_using_specification_single_behavior:
  Kernel_function.t ->
  Cil_types.funspec ->
  call_kinstr:Cil_types.kinstr ->
  with_formals:Cvalue.Model.t ->
  Value_types.call_result

(** Evaluate [kf] in state [with_formals], first by reducing by the
  preconditions, then by evaluating the assigns, then by reducing
  by the post-conditions. This function should be used when there are multiple
  behaviors, as it will evaluate them separately (when useful) to gain
  precision *)
val compute_using_specification_multiple_behaviors:
  Kernel_function.t ->
  Cil_types.funspec ->
  call_kinstr:Cil_types.kinstr ->
  with_formals:Cvalue.Model.t ->
  Value_types.call_result
