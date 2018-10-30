(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open Wp.Tactical

(* ---------------------------------------------------------------------- *)
(* --- NOP Tactical                                                   --- *)
(* ---------------------------------------------------------------------- *)

class nop =
  object
    inherit Wp.Tactical.make
        ~id:"Wp.Test.NOP"
        ~title:"NOP"
        ~descr:"Does nothing."
        ~params:[]

    method select feedback (s : Wp.Tactical.selection) =
      match s with
      | Empty -> Not_applicable
      | Compose _
      | Inside _
      | Clause _ ->
          feedback#set_title "NOP" ;
          feedback#set_descr "Does nothing; just for testing." ;
          Applicable (fun s -> ["Nop", s])

  end

let tactical = Wp.Tactical.export (new nop)
let _ = Wp.Strategy.make tactical ~arguments:[]

(* -------------------------------------------------------------------------- *)
