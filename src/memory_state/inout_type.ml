(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

type t = 
    { over_inputs_if_termination :  Locations.Zone.t ;
      under_outputs_if_termination : Locations.Zone.t ;
      over_inputs : Locations.Zone.t }
type tt = t
open Locations

module Datatype =
  Project.Datatype.Register
    (struct
       type t = tt
       let rehash x =
	 { over_inputs_if_termination = 
	     Zone.Datatype.rehash x.over_inputs_if_termination;
	   under_outputs_if_termination =
	     Zone.Datatype.rehash x.under_outputs_if_termination;
	   over_inputs =
	     Zone.Datatype.rehash x.over_inputs }
       include Datatype.Nop
       let copy _ = assert false (* TODO *)
       let name = Project.Datatype.Name.make "Inout" 
       let dependencies = [ Zone.Datatype.self ] 
     end)
