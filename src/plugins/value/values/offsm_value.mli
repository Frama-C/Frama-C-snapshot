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

type offsm_or_top = O of Cvalue.V_Offsetmap.t | Top

val offsm_key : offsm_or_top Structure.Key_Value.k

val cast :
  old_size: Integer.t -> new_size: Integer.t -> signed: bool ->
  Cvalue.V_Offsetmap.t -> Cvalue.V_Offsetmap.t


module Offsm : Abstract_value.Internal with type t = offsm_or_top

module CvalueOffsm : Abstract_value.Internal with type t = Cvalue.V.t * offsm_or_top
