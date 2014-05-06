(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* ---------------------------------------------------------------------- *)
(* --- Patricia Sets By L. Correnson & P. Baudin                      --- *)
(* ---------------------------------------------------------------------- *)

type t = unit Intmap.t

let empty = Intmap.empty
let singleton x = Intmap.singleton x ()
let add x = Intmap.add x ()
let remove x = Intmap.remove x
let is_empty = Intmap.is_empty
let mem = Intmap.mem
let cardinal = Intmap.size
let compare = Intmap.compare (fun () () -> 0)
let equal = Intmap.equal (fun () () -> true)

let _keep _ _ _ = ()
let _keepq _ _ _ = Some ()
let _same _ () () = true
let union = Intmap.union _keep
let inter = Intmap.interq _keepq
let diff = Intmap.diffq _keepq
let subset = Intmap.subset _same
let intersect = Intmap.intersectf _same

let iter f = Intmap.iteri (fun i () -> f i)
let fold f = Intmap.foldi (fun i () e -> f i e)

let filter f = Intmap.filter (fun i () -> f i)
let partition f = Intmap.partition (fun i () -> f i)

let for_all f = Intmap.for_all (fun i () -> f i)
let exists f = Intmap.exists (fun i () -> f i)

let elements = Intmap.mapl (fun i () -> i)
let mapf f= Intmap.mapf (fun i () -> f i)
let map f = Intmap.map (fun i () -> Some (f i))
