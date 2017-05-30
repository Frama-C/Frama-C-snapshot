(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Tactical

(* -------------------------------------------------------------------------- *)
(* --- Filter Tactical                                                    --- *)
(* -------------------------------------------------------------------------- *)

let vanti,panti =
  Tactical.checkbox ~id:"anti"
    ~title:"Absurd"
    ~descr:"Find Contradiction in Side Hypotheses"
    ~default:false ()

class filter =
  object(self)
    inherit Tactical.make ~id:"Wp.filter"
        ~title:"Filter"
        ~descr:"Erase Hypotheses"
        ~params:[panti]

    method select feedback _sel =
      let anti = self#get_field vanti in
      let process seq = ["Filter",Filtering.compute ~anti seq] in
      feedback#set_title (if anti then "Filter (absurd)" else "Filter") ;
      Applicable process

  end

let tactical = Tactical.export (new filter)

let strategy ?(priority=1.0) ?(anti=false) () =
  Strategy.{
    priority ; tactical ;
    selection = Empty ;
    arguments = [arg vanti anti] ;
  }
