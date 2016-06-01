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

(* -------------------------------------------------------------------------- *)
(* --- Iterator for Report                                                --- *)
(* -------------------------------------------------------------------------- *)

open Property_status

class type inspector =
object

  method empty : unit
  method started : unit
  method global_section : unit
  method function_section : Kernel_function.t -> unit
  method property : Property.t -> Consolidation.t -> unit
  method finished : unit
    
end

val dead_reasons : Consolidation.pending -> Property.Set.t
val partial_pending : Consolidation.pending -> Property.Set.t Emitter.Usable_emitter.Map.t
val iter : inspector -> unit

val report_ip: Property.t -> bool
(** Should this property be part of the final report according to the users
    filters. *)
