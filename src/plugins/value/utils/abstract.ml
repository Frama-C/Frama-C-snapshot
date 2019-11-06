(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** External interface of an abstraction, built by {!Structure.Open}. *)
module type Interface = sig
  type t
  type 'a key
  val mem : 'a key -> bool
  val get : 'a key -> (t -> 'a) option
  val set : 'a key -> 'a -> t -> t
end

module Value = struct

  module V = struct
    type 'a t = (module Abstract_value.S with type t = 'a)
  end

  include Structure.Shape (Structure.Key_Value) (V)

  module type Internal = sig
    include Abstract_value.S
    val structure: t structure
  end

  module type External = sig
    include Internal
    include Structure.External with type t := t
                                and type 'a key := 'a key
  end
end

module Location = struct

  module L = struct
    type 'a t = (module Abstract_location.S with type location = 'a)
  end

  include Structure.Shape (Structure.Key_Location) (L)

  module type Internal = sig
    include Abstract_location.S
    val structure: location structure
  end

  module type External = sig
    include Internal
    include Structure.External with type t := location
                                and type 'a key := 'a key
  end
end

module Domain = struct

  module D = struct
    type 'a t = (module Abstract_domain.Internal with type state = 'a)
  end

  include Structure.Shape (Structure.Key_Domain) (D)

  module type Internal = sig
    include Abstract_domain.Internal
    val structure: t structure
  end

  module type External = sig
    include Internal
    include Structure.External with type t := t
                                and type 'a key := 'a key

    val get_cvalue: (t -> Cvalue.Model.t) option
    val get_cvalue_or_top: t -> Cvalue.Model.t
    val get_cvalue_or_bottom: t Bottom.or_bottom -> Cvalue.Model.t
  end
end
