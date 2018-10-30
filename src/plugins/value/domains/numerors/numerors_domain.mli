(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

type value
type location = Precise_locs.precise_location
val value_key : value Structure.Key_Value.k

(** True if the numerors domain is available;
    False if the MPFR library has not been found. *)
val ok: bool

(** Functions used by the engine to build numerors abstractions. *)

(** Builds the product between a given value module and the numerors value
    module. If the given value module contains Cvalue, uses cvalues to reduce
    numerors values on casts from integer to floating-point values.
    Fails if numerors domain is not available.  *)
val add_numerors_value:
  (module Abstract_value.Internal) -> (module Abstract_value.Internal)

(* From a given abstract value product, creates the reduction function that
   reduces numerors values by using cvalues. Returns the identity if the given
   value product does not contain numerors and cvalue componants. *)
val reduce_error:
  (module Abstract_value.External with type t = 'v) -> ('v -> 'v)

(** Returns the numerors domain module, if available. Fails otherwise. *)
val numerors_domain:
  unit -> (module Abstract_domain.Internal with type value = value
                                            and type location = location)
