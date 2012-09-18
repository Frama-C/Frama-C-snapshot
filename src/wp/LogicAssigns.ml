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

open Ctypes
open Lang
open Lang.F
open Memory

module type Code =
sig
  type loc
  val equal_obj : c_object -> loc value -> loc value -> F.pred
end

module type Logic =
sig
  type loc
  val vars : loc Memory.sloc list -> Vars.t
  val pp_logic : Format.formatter -> loc Memory.logic -> unit
  val pp_sloc : Format.formatter -> loc Memory.sloc -> unit
  val pp_region : Format.formatter -> loc Memory.sloc list -> unit
end

module Make
  ( M : Memory.Model )
  ( C : Code with type loc = M.loc )
  ( L : Logic with type loc = M.loc ) =
struct

  open M
  module Hmap = Heap.Map
  module Dom = Heap.Set

  type region = (c_object * loc sloc list) list

  (* -------------------------------------------------------------------------- *)
  (* --- Domain                                                             --- *)
  (* -------------------------------------------------------------------------- *)

  let vars (r:region) =
    List.fold_left
      (fun xs (_,s) -> Vars.union xs (L.vars s))
      Vars.empty r

  let dsloc obj = function
    | Sloc l | Sdescr(_,l,_) -> M.domain obj l
    | Srange(l,obj,_,_) -> M.domain obj l

  let domain (r:region) =
    List.fold_left
      (fun d (obj,slocs) ->
	 List.fold_left
	   (fun d sloc -> Dom.union d (dsloc obj sloc)) d slocs
      ) Dom.empty r

  (* -------------------------------------------------------------------------- *)
  (* --- Assignation                                                        --- *)
  (* -------------------------------------------------------------------------- *)

  let is_havoc s1 s2 domain =
    let pool = ref [] in
    M.Sigma.iter2
      (fun chunk c1 c2 ->
	 match c1 , c2 with
	   | Some x , Some y when not (M.Heap.Set.mem chunk domain)
	       -> pool := F.p_equal (e_var x) (e_var y) :: !pool
	   | _ -> ()
      ) s1 s2 ;
    !pool

  let rec assigned_seq hs s = function
    | [] -> hs

    | [obj,sloc] -> 
	let hs_sloc = M.assigned s obj sloc in
	let hs_others = is_havoc s.pre s.post (dsloc obj sloc) in
	hs_sloc @ hs_others @ hs

    | (obj,sloc)::tail ->
	let sigma = M.Sigma.havoc s.post (dsloc obj sloc) in
	let hs_sloc = M.assigned { pre = sigma ; post = s.post } obj sloc in
	assigned_seq (hs_sloc @ hs) { pre = s.pre ; post = sigma } tail

  let assigned (s:sigma sequence) (r:region) = 
    assigned_seq [] s
      begin
	List.fold_left
	  (fun w (obj,slocs) ->
	     List.fold_left (fun w sloc -> (obj,sloc) :: w) w slocs
	  ) [] r
      end
      
end
