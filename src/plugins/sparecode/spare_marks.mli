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

open Cil_types

type proj
type fct

val select_useful_things :
  select_annot:bool -> select_slice_pragma:bool -> kernel_function -> proj

val get_marks : proj -> kernel_function -> fct option

val key_visible : fct -> PdgIndex.Key.t -> bool

(** Useful mainly if there has been some Pdg.Top *)
val kf_visible : proj -> kernel_function -> bool

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
