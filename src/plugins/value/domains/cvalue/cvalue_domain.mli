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

(** Main domain of the Value Analysis. *)

module State : Abstract_domain.Internal
  with type value = Main_values.CVal.t
   and type location = Main_locations.PLoc.location


val key : Cvalue.Model.t Abstract_domain.key

val extract :
  (Cvalue.Model.t Abstract_domain.key -> ('state -> Cvalue.Model.t) option) ->
  'state Eval.or_bottom -> Cvalue.Model.t

val inject : Cvalue.Model.t -> State.t
val project : State.t -> Cvalue.Model.t


(** Specific functions for partitioning optimizations.  *)

type prefix
module Subpart : Hashtbl.HashedType
val distinct_subpart :
  Cvalue.Model.t -> Cvalue.Model.t -> (prefix * Subpart.t * Subpart.t) option
val find_subpart : Cvalue.Model.t -> prefix -> Subpart.t option



(*
Local Variables:
compile-command: "make -C ../../../../.."
End:
*)
