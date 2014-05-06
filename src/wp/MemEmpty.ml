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

(* -------------------------------------------------------------------------- *)
(* --- Empty Memory Model                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Lang.F
open Memory

module Logic = Qed.Logic

let datatype = "MemEmpty"
let configure () =
  begin
    Context.set Lang.pointer (fun _typ -> Logic.Int) ;
    Context.set Cvalues.null (p_equal e_zero) ;
  end

let theories () = []

module Chunk =
struct
  type t = unit
  let self = "empty"
  let hash () = 0
  let equal () () = true
  let compare () () = 0
  let pretty _fmt () = ()
  let tau_of_chunk () = Logic.Int
  let basename_of_chunk () = "u"
  let is_framed () = true
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
let literal ~eid _ = ignore eid
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

let load _sigma _obj () = Warning.error ~source "Can not load value in Empty model"
let copied _s _obj () () = []
let stored _s _obj () _ = []
let assigned _s _obj _sloc = []

let no_pointer () = Warning.error ~source "Can not compare pointers in Empty model"

let is_null _ = no_pointer ()
let loc_eq _ _ = no_pointer ()
let loc_lt _ _ = no_pointer ()
let loc_leq _ _ = no_pointer ()
let loc_neq _ _ = no_pointer ()
let loc_diff _ _ _ = no_pointer ()

let valid _sigma _l = Warning.error ~source "No validity"
let scope sigma _s _xs = sigma , []

let included _s1 _s2 = no_pointer ()
let separated _s1 _s2 = no_pointer ()
