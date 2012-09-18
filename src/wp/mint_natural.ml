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
(* --- Basic Implementation (Arithmetics is mapped to Z)                  --- *)
(* -------------------------------------------------------------------------- *)

module Create (F:Formula.S) : (Mint.S with module F = F) =
struct

  module F = F

  (* arithmetic operations on Z in why *)

  let i_neg i e =
    let k = F.e_ineg e in
    if (Ctypes.signed i) then  k else F.i_modulo i k
  let i_op _i iop e1 e2 = F.e_iop iop e1 e2
  let i_cmp _i cmp e1 e2 = (F.e_icmp cmp e1 e2)

  (* bitwise operations on Z in why *)

  let iop1 op1 tr x = F.i_modulo tr (op1 x)
  let iop2 op2 tr x y = F.i_modulo tr (op2 x y)

  let bits_not = iop1 F.e_bnot
  let bits_and = iop2 F.e_band
  let bits_or  = iop2 F.e_bor
  let bits_xor = iop2 F.e_bxor
  let bits_lshift = iop2 F.e_lshift
  let bits_rshift = iop2 F.e_rshift

end
