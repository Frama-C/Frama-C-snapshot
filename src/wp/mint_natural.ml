(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
(* --- Basic Implementation (Arithmetics is mapped to Z)                  --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Formula

module Create (F:Formula.S) : (Mint.S with module F = F) =
struct

  module F = F

  (* integer cst *)
  let  i_int _i k = F.e_icst k

  (* arithmetic operations on Z in why *)

  let i_neg i e =
    let k = F.e_ineg e in
    if (Ctypes.signed i) then  k else F.modulo i k
  let i_op _i iop e1 e2 = F.e_iop iop e1 e2
  let i_cmp _i cmp e1 e2 = (F.e_icmp cmp e1 e2)

  (* bitwise operations on Z in why *)

  let sizeof i = F.e_int (Ctypes.i_sizeof i)
  let signess i = if (Ctypes.signed i) then F.e_true else F.e_false

  let bits_not  _ = F.e_app1 "int_not"
  let bits_and  _ = F.e_app2 "int_and"
  let bits_or   _ = F.e_app2 "int_or"
  let bits_xor  _ = F.e_app2 "int_xor"
  let bits_lshift  _ = F.e_app2 "int_lsh"
  let bits_rshift  i =
    F.e_app2 (if (Ctypes.signed i) then "int_rshs" else "int_rshu")

  (* integers conversions *)

  let i_convert t1 t2 e =
    if Ctypes.sub_c_int t1 t2 then e else F.modulo t2 e

  let integer_of_int _ e = e
    (* Z-representatives live in the correct range. *)

  let int_of_integer _ e = e
    (* Conversions are never necessary for this model, since no overflow are allowed. *)

end
