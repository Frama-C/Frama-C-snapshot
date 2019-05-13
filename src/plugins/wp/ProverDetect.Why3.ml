(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

# 23 "src/plugins/wp/ProverDetect.Why3.ml"

(* -------------------------------------------------------------------------- *)
(* --- Why3 Prover Detection                                              --- *)
(* -------------------------------------------------------------------------- *)

open Why3
open Wstdlib
open Whyconf

let detect () =
  let config = Whyconf.read_config None in
  let provers = Whyconf.get_prover_shortcuts config in
  let index = ref Mprover.empty in
  Mstr.iter
    (fun key dp ->
       let keys = Mprover.find_def [] dp !index in
       index := Mprover.add dp (key::keys) !index)
    provers ;
  let dps =
    Mprover.fold
      (fun dp keys dps ->
         VCS.{
           dp_name = dp.prover_name ;
           dp_version = dp.prover_version ;
           dp_altern = dp.prover_altern ;
           dp_shortcuts = List.rev keys ;
         } :: dps
      ) !index []
  in List.rev dps

(**************************************************************************)
