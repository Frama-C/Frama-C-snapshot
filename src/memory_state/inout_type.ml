(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

module Datatype = struct
  include Project.Datatype.Register
    (struct
       type t = tt
       let rehash x =
         let a = Zone.Datatype.rehash x.over_inputs_if_termination in
         let b = Zone.Datatype.rehash x.under_outputs_if_termination in
         let c = Zone.Datatype.rehash x.over_inputs in
	 { over_inputs_if_termination = a;
	   under_outputs_if_termination = b; over_inputs = c}
       let descr = Unmarshal.Abstract (* TODO: use Data.descr *)
       let copy _ = assert false (* TODO *)
       let name = "inout" 
     end)
  let hash 
      { over_inputs_if_termination = a; 
        under_outputs_if_termination = b; 
        over_inputs = c} = 
    Zone.tag a + 17 * Zone.tag b + 587 * Zone.tag c
  let equal 
      { over_inputs_if_termination = a; 
        under_outputs_if_termination = b; 
        over_inputs = c} 
      { over_inputs_if_termination = a'; 
        under_outputs_if_termination = b'; 
        over_inputs = c'} = 
    Zone.equal a a' && Zone.equal b b' && Zone.equal c c'
  let () = register_comparable ~hash ~equal ()
end
