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

(** Automatic builders to complete abstract domains from different
    simplified interfaces. *)

module type InputDomain = sig
  include Abstract_domain.S_with_Structure
  val storage: unit -> bool
end

module Complete
    (Domain: InputDomain)
  : Abstract_domain.Internal with type state = Domain.state
                              and type value = Domain.value
                              and type location = Domain.location

module Complete_Minimal
    (Value: Abstract_value.S)
    (Location: Abstract_location.S)
    (Domain: Simpler_domains.Minimal)
  : Abstract_domain.Internal with type value = Value.t
                              and type location = Location.location
                              and type state = Domain.t

module Complete_Minimal_with_datatype
    (Value: Abstract_value.S)
    (Location: Abstract_location.S)
    (Domain: Simpler_domains.Minimal_with_datatype)
  : Abstract_domain.Internal with type value = Value.t
                              and type location = Location.location
                              and type state = Domain.t

module Complete_Simple_Cvalue
    (Domain: Simpler_domains.Simple_Cvalue)
  : Abstract_domain.Internal with type value = Cvalue.V.t
                              and type location = Precise_locs.precise_location
                              and type state = Domain.t
