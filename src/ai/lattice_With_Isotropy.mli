(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

module type S = sig
  open Abstract_interp
  include Lattice
  module Top_Param : Lattice_Set
    
  (** Are the bits independent? *)
  val is_isotropic : t -> bool
  val hash: t -> int
  val cast : with_alarms:CilE.warn_mode -> size:Int.t -> signed:bool -> t -> t
  val extract_bits : with_alarms:CilE.warn_mode -> start:Int.t -> stop:Int.t -> t -> t
  val bitwise_or : size:int -> t -> t -> t
  val shift_left : with_alarms:CilE.warn_mode -> size:int -> t -> t -> t
  val little_endian_merge_bits :
    total_length:int -> value:t -> offset:Int.t -> t -> t
  val big_endian_merge_bits :
    total_length:int -> length:Int.t -> value:t -> offset:Int.t -> t -> t


  (* Make a top for integers or pointers *)
  val topify_merge_origin : t -> t
  val topify_arith_origin : t -> t
  val inject_top_origin : Origin.t -> Top_Param.O.t -> t
  val topify_misaligned_read_origin : t -> t
  val under_topify : t -> t
  val anisotropic_cast : size:Int.t -> t -> t

  val topify_with_origin : Origin.t -> t -> t

  val singleton_zero : t
  val of_char : char -> t

(* [is_included_actual_generic bases actual generic]
     returns [i] if the hidden variables of [generic] can
     be instanciated with an instanciation [i] so that [actual]
     is included in "[i(generic)]". Raises [Is_not_included]
     if the instanciation was not found. *)
  val is_included_actual_generic :
    BaseUtils.BaseSet.t ->
    BaseUtils.BaseSet.t ref -> 
    Locations.Location_Bytes.t BaseUtils.BaseMap.t ref -> t -> t -> unit

  val project : t -> Locations.Location_Bytes.t
  val id : string

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
