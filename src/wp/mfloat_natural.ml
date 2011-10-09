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
(* --- Basic Real Model                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Formula

module Create (F:Formula.S) : (Mfloat.S with module F = F) =
struct

  module F = F

  let tau_of_cfloat _ = Real

  let format_of_cfloat _ =  F.unwrap (F.e_call "real_format" [])

  (*Float cst *)

  let f_float _f k = F.e_rcst k

  (* arithmetic operations on R in why *)

  let f_neg _i = F.e_rneg
  let f_op  _i = F.e_rop
  let f_cmp _i = F.e_rcmp

  (* conversion on R in why *)

  let f_convert _t1 _t2 e = e
  let real_of_float _te e = e
  let float_of_real _t1 e = e

end
