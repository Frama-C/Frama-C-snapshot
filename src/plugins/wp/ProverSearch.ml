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
open Strategy

let configure (console : #Tactical.feedback) strategy =
  let { tactical ; selection ; arguments } = strategy in
  let verdict =
    try
      tactical#reset ;
      Strategy.set_args tactical arguments ;
      tactical#select console selection
    with Not_found | Exit -> Not_applicable
  in
  match verdict with
  | Applicable process when not console#has_error ->
      let title = tactical#title in
      let script = ProofScript.jtactic ~title tactical selection in
      Some (script , process)
  | _ -> None

let fork tree anchor strategy =
  let console = new ProofScript.console ~title:strategy.tactical#title in
  try
    let model = ProofEngine.model anchor in
    match Model.with_model model (configure console) strategy with
    | None -> None
    | Some (script,process) ->
        Some (ProofEngine.fork tree ~anchor script process)
  with
  | Not_found ->
      console#set_error "Can not configure strategy" ; None
  | e ->
      console#set_error "Exception <%s>" (Printexc.to_string e) ;
      raise e

let rec lookup tree anchor k hs =
  let n = Array.length hs in
  if n=0 then None,n,hs else
    match fork tree anchor hs.(k) with
    | Some fork -> Some fork,k,hs
    | None ->
        if k = 0 then
          lookup tree anchor 0 (Array.sub hs 1 (n-1))
        else
          let slice = Array.sub hs 0 (n-1) in
          if k < n-1 then
            ( Array.blit hs (succ k) slice k (n-k-1) ;
              lookup tree anchor k slice )
          else
            lookup tree anchor 0 hs

let first tree ~anchor hs =
  let fork,index,hs = lookup tree anchor 0 hs in
  ProofEngine.set_strategies anchor ~index hs ; fork

let backtrack tree ~anchor ~loop =
  let k,hs = ProofEngine.get_strategies anchor in
  let n = Array.length hs in
  if 1<n && (loop || succ k < n) then
    let k = if succ k < n then succ k else 0 in
    let fork,index,hs = lookup tree anchor k hs in
    ProofEngine.set_strategies anchor ~index hs ; fork
  else None

(* -------------------------------------------------------------------------- *)
