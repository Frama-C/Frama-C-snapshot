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

#24 "src/plugins/value/domains/numerors/numerors_domain.ko.ml"

type value
type location = Precise_locs.precise_location
let value_key = Structure.Key_Value.create_key "dummy_numerors_values"

let ok = false

let abort () =
  Value_parameters.abort
    "The numerors domain has been requested but is not available, as Frama-C \
     did not found the MPFR library. The analysis is aborted."

let add_numerors_value _ = abort ()
let numerors_domain = abort

let reduce_error _ = fun v -> v
