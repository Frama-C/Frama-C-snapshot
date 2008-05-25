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

module MemoryFootprint : Computation.REF_OUTPUT with type data = int

module type BinCacheable =
sig
  type t
  type result
  val hash : t -> int
  val size : int
  val sentinel : t
  val sentinel_result : result
end

module type Cacheable =
sig
  type t
  val hash : t -> int
  val sentinel : t
end

module Make_Symetric :
  functor (H : BinCacheable) ->
    sig
      val clear : unit -> unit
      val merge : (unit -> H.result) -> H.t -> H.t -> H.result
    end


module Make_Asymetric :
  functor (H : BinCacheable) ->
    sig
      val clear : unit -> unit
      val merge : (unit -> H.result) -> H.t -> H.t -> H.result
    end

module Make_Het :
functor (H1 : Cacheable) -> 
  functor (H2 : Cacheable) -> 
    functor (R : Cacheable) ->
  sig
    val clear : unit -> unit
    val merge : (unit -> R.t) -> H1.t -> H2.t -> R.t
  end
