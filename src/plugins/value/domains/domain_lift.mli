(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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


module type Conversion = sig
  type extended_value
  type extended_location
  type internal_value
  type internal_location

  val extend_val : internal_value -> extended_value
  val restrict_val : extended_value -> internal_value

  val extend_loc : internal_location -> extended_location
  val restrict_loc : extended_location -> internal_location
end


module Make
    (Domain: Abstract_domain.Internal)
    (Convert : Conversion with type internal_value := Domain.value
                           and type internal_location := Domain.location)
  : Abstract_domain.Internal with type state = Domain.state
                              and type value = Convert.extended_value
                              and type location = Convert.extended_location
                              and type origin = Domain.origin


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
