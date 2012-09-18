(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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
(* --- Empty Memory Model                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Qed
open Lang.F
open Memory

let datatype = "MemEmpty"
let register ~name ?id ?descr ?tuning () =
  let model = Model.register ~name ?id ?descr ?tuning () in
  Model.set_parameter model Lang.pointer (fun _typ -> Logic.Int) "no pointer" ;
  Model.set_parameter model Cvalues.null (p_equal e_zero) "null is 0" ;
  model

module Chunk =
struct
  type t = unit
  let self = "empty"
  let hash () = 0
  let compare () () = 0
  let pretty _fmt () = ()
  let tau_of_chunk () = Logic.Int
  let basename_of_chunk () = "u"
end

module Heap = Qed.Collection.Make(Chunk)
module Sigma = Sigma.Make(Chunk)(Heap)

type loc = unit
type chunk = Chunk.t
type sigma = Sigma.t
type segment = loc rloc

let pretty _fmt () = ()
let vars _l = Vars.empty
let occurs _x _l = false

let null = ()
let cvar _x = ()
let pointer_loc _t = ()
let pointer_val () = e_zero

let field _l _f = ()
let shift _l _obj _k = ()
let base_addr _l = ()
let block_length _s _obj _l = e_zero

let cast _ _l = ()
let loc_of_int _ _ = ()
let int_of_loc _ () = e_zero

let domain _obj _l = Heap.Set.empty

let source = "Empty Model"

let load _sigma _obj () = Warning.error ~source "Can not load value"
let copied _s _obj () () = []
let stored _s _obj () _ = []
let assigned _s _obj _sloc = []

let no_pointer () = Warning.error ~source "Can not compare pointers"

let is_null _ = no_pointer ()
let loc_eq _ _ = no_pointer ()
let loc_lt _ _ = no_pointer ()
let loc_leq _ _ = no_pointer ()
let loc_neq _ _ = no_pointer ()
let loc_offset _ _ _ = no_pointer ()

let valid _sigma _l = Warning.error ~source "No validity"
let scope sigma _s _xs = sigma , []

let included _s1 _s2 = no_pointer ()
let separated _s1 _s2 = no_pointer ()

