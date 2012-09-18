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
(** Memory Model for Runtime:
* more information about it in {{:../../wp/Notes/m3.html}this document}
* *)
(* -------------------------------------------------------------------------- *)

let _dkey = "heap" (* debugging key *)

module Create
  (F:Formula.S)
  (A:Mint.S   with module F = F)
  (R:Mfloat.S with module F = F)
  =
struct

  module Model =
  struct

    module F = F
    module A = A
    module R = R

    type loc =
      | Null
      | Pointer of F.abstract (* pointer *)

    let tau_of_loc = Formula.ADT("pointer",[])

    let loc_of_term _obj ptr = Pointer ptr
    let term_of_loc = function
      | Null -> F.e_app2 "addr" F.i_zero F.i_zero
      | Pointer ptr -> ptr

    let cast_loc_to_int _typ loc c_int : F.integer =
      let iword = Ctypes.c_ptr () in
      F.i_convert iword c_int (F.e_app1 "int_of_pointer" (term_of_loc loc))

    let cast_int_to_loc c_int (k:F.integer) _typ : loc =
      let iword = Ctypes.c_ptr () in
      Pointer (F.e_app1 "pointer_of_int" (F.i_convert c_int iword k))

    let null = Null

    let base = function
      | Null -> F.i_zero
      | Pointer ptr -> F.e_app1 "base" ptr

    let offset = function
      | Null -> F.i_zero
      | Pointer ptr -> F.e_app1 "offset" ptr

    let is_null = function
      | Null -> F.e_true
      | p -> F.e_and
	  (F.e_icmp Formula.Ceq (base p) F.i_zero)
	  (F.e_icmp Formula.Ceq (offset p) F.i_zero)
	  
    let e_loc2 f p q = F.e_app2 f (term_of_loc p) (term_of_loc q)
    let p_loc2 f p q = F.p_app2 f (term_of_loc p) (term_of_loc q)

    let minus_loc = e_loc2 "pointer_sub"
    let le_loc = p_loc2 "pointer_le"
    let lt_loc = p_loc2 "pointer_lt"
    let le_loc_bool = e_loc2 "pointer_le_bool"
    let lt_loc_bool = e_loc2 "pointer_lt_bool"
    let equal_loc = p_loc2 "pointer_eq"
    let equal_loc_bool = e_loc2 "pointer_eq_bool"

    let pp_loc fmt = function
      | Null -> Format.fprintf fmt "null"
      | Pointer ptr -> F.pp_term fmt ptr

  end

  module V =  Datalib.Cvalues(Model)
  module L = Datalib.Create(V)

end

