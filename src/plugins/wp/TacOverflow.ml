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

open Lang
open Tactical

class overflow =
  object
    inherit Tactical.make
        ~id:"Wp.overflow"
        ~title:"Overflow"
        ~descr:"Consider no overflow nor downcast"
        ~params:[]

    method select _feedback selection =
      let e = Tactical.selected selection in
      let open Qed.Logic in
      match F.repr e with
      | Fun(f,[v]) ->
          let iota = Cint.to_cint f in
          let cond = Cint.range iota v in
          (*
          let a,b = Ctypes.bounds iota in
          let range = F.p_and
              (F.p_leq (F.e_zint a) v)
              (F.p_leq v (F.e_zint b)) in
          *)
          Applicable( fun (hs,g) -> [
                "In-Range", (hs , cond) ;
                "No-Overflow" ,
                Conditions.subst
                  (fun u -> if u == e then v else u)
                  (hs , F.p_imply cond g)
              ])
      | _ -> Not_applicable

  end

let overflow = Tactical.export (new overflow)
