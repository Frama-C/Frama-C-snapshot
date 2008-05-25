(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007                                                    *)
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

module MemoryFootprint = 
  Computation.Ref
    (struct include Datatype.Int let default = 2 end)
    (struct 
       let name = Project.Computation.Name.make "Binary_cache.MemoryFootprint" 
       let dependencies = [] 
     end)

let create sentinel initial_size =
  Array.create (2 * initial_size) sentinel
  
let clear sentinel h =
  Array.fill h 0 (Array.length h) sentinel

let merge h has results f a1 a2 = 
  let b0 = 2 * has in
  let b1 = succ b0 in
  if h.(b0) == a1 && h.(b1) == a2
  then results.(has)
  else 
    let result = f () in
    h.(b0) <- a1;
    h.(b1) <- a2;
    results.(has) <- result;
    result

let get_size () = 
  match MemoryFootprint.get () with
    1 -> 1024
  | 2 -> 8192
  | _ -> 32 * 1024

module type BinCacheable =
sig
  type t
  type result
  val hash : t -> int
  val size : int
  val sentinel : t
  val sentinel_result : result
end

module Make_Symetric (H: BinCacheable) =
struct
  assert (H.size = 65536 || H.size = 2048 || H.size = 4096 || 
      H.size = 8192 || H.size = 16384);;

  let size = get_size ()
  let args = create H.sentinel size
  let results = Array.create size H.sentinel_result

  let mask = pred size

  let clear () = 
    clear H.sentinel args;
    clear H.sentinel_result results

  let hash = H.hash

  let merge f a1 a2 =
    let a1, a2, has = 
      let h1 = hash a1 in
      let h2 = hash a2 in
      if h1 < h2 
      then a1, a2, 599 * h1 + h2
      else a2, a1, 599 * h2 + h1
    in
    merge args (has land mask) results f a1 a2
end

module Make_Asymetric (H: BinCacheable) =
struct
  assert (H.size = 65536 || H.size = 2048 || H.size = 4096 || 
      H.size = 8192 || H.size = 16384);;

  let size = get_size ()
  let args = create H.sentinel size
  let results = Array.create size H.sentinel_result

  let mask = pred size

  let clear () = 
    clear H.sentinel args;
    clear H.sentinel_result results

  let hash = H.hash

  let merge f a1 a2 =
    let has = 599 * (hash a1) + hash a2 in
    merge args (has land mask) results f a1 a2
end

module type Cacheable =
sig
  type t
  val hash : t -> int
  val sentinel : t
end

module Make_Het (H1: Cacheable) (H2: Cacheable) (R: Cacheable) =
struct
  let size = get_size ()
  let args1 = Array.create size H1.sentinel
  let args2 = Array.create size H2.sentinel
  let results = Array.create size R.sentinel

  let mask = pred size

  let clear () = 
    clear H1.sentinel args1;
    clear H2.sentinel args2;
    clear R.sentinel results

  let merge f a1 a2 =
    let has = 599 * (H1.hash a1) + H2.hash a2 in
    if args1.(has) == a1 && args2.(has) == a2
    then results.(has)
    else 
      let result = f () in
      args1.(has) <- a1;
      args2.(has) <- a2;
      results.(has) <- result;
      result


end
