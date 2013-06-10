(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(* -------------------------------------------------------------------------- *)
(* --- Patricia Sets By L. Correnson                                      --- *)
(* -------------------------------------------------------------------------- *)

type t = unit Intmap.t

let empty = Intmap.empty
let singleton x = Intmap.add x () Intmap.empty
let add x s = Intmap.add x () s
let is_empty = Intmap.is_empty
let mem = Intmap.mem
let cardinal = Intmap.size
let compare = Intmap.compare (fun () () -> 0)
let equal = Intmap.equal (fun () () -> true)

let _keep _ _ _ = ()
let _same _ () () = true
let union = Intmap.union _keep
let inter = Intmap.inter _keep
let subset = Intmap.subset _same
let diff = Intmap.merge (fun _i x y -> if y = None then x else None)

let iter f = Intmap.iteri (fun i () -> f i)
let fold f s e = Intmap.foldi (fun i () e -> f i e) s e

let elements s = fold (fun x s -> x::s) s []
  
