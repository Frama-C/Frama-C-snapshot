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

(** Widening hints for the Value Analysis datastructures. *)

include Datatype.S


(** An empty set of hints *)
val empty : t

(** A default set of hints *)
val default : unit -> t

(** Add numeric hints for one or all variables ([None]),
    for a a certain stmt or for all statements ([None]).  *)
val add_num_hints:
  Cil_types.stmt option -> Base.t option -> Ival.Widen_Hints.t -> t -> t

(** Add a set of bases to widen in priority for a given statement. *)
val add_var_hints : Cil_types.stmt -> Base.Set.t -> t -> t

(** Widen hints for a given statement, suitable for function
    {!Cvalue.Model.widen}. *)
val hints_from_keys :
  Cil_types.stmt -> t ->
  Base.Set.t * (Base.t -> Locations.Location_Bytes.generic_widen_hint)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
