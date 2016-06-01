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

(** Cartesian product of two value abstractions. *)

module Make
    (Left: Abstract_value.Internal)
    (Right: Abstract_value.Internal)
  : Abstract_value.Internal with type t = Left.t * Right.t


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
