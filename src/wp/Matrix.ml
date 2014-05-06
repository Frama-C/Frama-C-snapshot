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
(* --- Array Dimensions                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Lang.F

type dim = int option
type matrix = c_object * dim list

let of_array = Ctypes.array_dimensions

module KEY(E : sig val compare : c_object -> c_object -> int end) =
struct
  type t = matrix

  let compare_dim d1 d2 = match d1 , d2 with
    | None,None -> 0
    | Some _,None -> (-1)
    | None,Some _ -> 1
    | Some _,Some _ -> 0
	
  let compare (e1,ds1) (e2,ds2) =
    let cmp = E.compare e1 e2 in
    if cmp = 0 then Qed.Hcons.compare_list compare_dim ds1 ds2 else cmp
      
  let pretty fmt (obj,ds) =
    Ctypes.pretty fmt obj ;
    List.iter
      (function
	 | None -> Format.pp_print_string fmt "[]"
	 | Some d -> Format.fprintf fmt "[%d]" d
      ) ds
end

module COBJ =
struct
  let compare e1 e2 = match e1 , e2 with
    | C_int _ , C_int _ -> 0
    | C_int _ , _ -> (-1)
    | _ , C_int _ -> 1
    | C_float _ , C_float _ -> 0
    | C_float _ , _ -> (-1)
    | _ , C_float _ -> 1
    | C_pointer _ , C_pointer _ -> 0
    | C_pointer _ , _ -> (-1)
    | _ , C_pointer _ -> 1
    | C_comp a , C_comp b -> Cil_datatype.Compinfo.compare a b
    | C_comp _ , _ -> (-1)
    | _ , C_comp _ -> 1
    | C_array _ , C_array _ -> assert false
end

module MACHINE = KEY(Ctypes)
module NATURAL = KEY(COBJ)

let natural_id = function
  | C_int _ -> "int"
  | C_float _ -> "float"
  | C_pointer _ -> "pointer"
  | C_array _ -> "array"
  | C_comp c -> Lang.comp_id c

let add_rank buffer k = if k > 0 then Buffer.add_string buffer (string_of_int k)
let add_dim buffer rank = function
  | None -> add_rank buffer rank ; Buffer.add_string buffer "w" ; 0
  | Some _ -> succ rank

let id ds =
  let buffer = Buffer.create 8 in
  add_rank buffer (List.fold_left (add_dim buffer) 0 ds) ;
  Buffer.contents buffer

type denv = {
  size_var : var list ; (* size variables *)
  size_val : term list ; (* size values *)
  index_var : var list ; (* index variables *)
  index_val : term list ; (* index values *)
  index_range : pred list ; (* indices are in range of size variables *)
  index_offset : term list ; (* polynomial of indices *)
  monotonic : bool ;
}

let rec collect rank = function
  | [] -> 
      {
	size_var = [] ;
	size_val = [] ;
	index_var = [] ;
	index_val = [] ;
	index_range = [] ;
	index_offset = [] ;
	monotonic = true ;
      }
  | d::ds ->
      let denv = collect (succ rank) ds in
      let k_base = match rank with 0 -> "i" | 1 -> "j" | _ -> "k" in
      let k_var = Lang.freshvar ~basename:k_base Qed.Logic.Int in
      let k_val = e_var k_var in
      let k_ofs = e_prod (k_val :: denv.size_val) in
      match d with
	| None ->
	    { denv with
		index_var = k_var :: denv.index_var ;
		index_val = k_val :: denv.index_val ;
		index_offset = k_ofs :: denv.index_offset ;
		monotonic = false ;
	    }
	| Some _ ->
	    let n_base = match rank with 0 -> "n" | 1 -> "m" | _ -> "d" in
	    let n_var = Lang.freshvar ~basename:n_base Qed.Logic.Int in
	    let n_val = e_var n_var in
	    let k_inf = p_leq e_zero k_val in
	    let k_sup = p_lt k_val n_val in
	    { 
	      size_var = n_var :: denv.size_var ;
	      size_val = n_val :: denv.size_val ;
	      index_var = k_var :: denv.index_var ;
	      index_val = k_val :: denv.index_val ;
	      index_offset = k_ofs :: denv.index_offset ;
	      index_range = k_inf :: k_sup :: denv.index_range ;
	      monotonic = denv.monotonic ;
	    }
	      
let denv = collect 0
let rec dval = function
  | [] -> []
  | None :: ds -> dval ds
  | Some n :: ds -> e_int n :: dval ds
let size (_,ds) = dval ds
let rec tau obj = function
  | [] -> Lang.tau_of_object obj
  | _ :: ds -> Qed.Logic.Array( Qed.Logic.Int , tau obj ds )

let rec do_merge ds1 ds2 =
  match ds1 , ds2 with
    | [] , [] -> []
    | [] , _ | _ , [] -> raise Exit
    | d1::ds1 , d2::ds2 ->
	let d = match d1 , d2 with
	  | None , _ | _ , None -> None
	  | Some n1 , Some n2 -> if n1=n2 then d1 else raise Exit
	in d :: do_merge ds1 ds2

let merge ds1 ds2 = 
  try Some(do_merge ds1 ds2)
  with Exit -> None
