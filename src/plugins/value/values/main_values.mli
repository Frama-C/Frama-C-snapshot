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

(** Main numeric values of EVA. *)

(** Abstract values built over Cvalue.V *)
module CVal : Abstract_value.Internal with type t = Cvalue.V.t

(** Key for cvalues. *)
val cvalue_key : CVal.t Abstract_value.key

(** Dummy interval: no forward nor backward propagations.
    [None] is top. *)
module Interval : Abstract_value.Internal with type t = Ival.t option

(** Key for intervals. *)
val interval_key : Interval.t Abstract_value.key

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
