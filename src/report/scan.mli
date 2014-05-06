(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
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
