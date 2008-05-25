(*
 * The Why certification tool
 * Copyright (C) 2002 Jean-Christophe FILLIATRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 *)

(* $Id: WhyInt.v,v 1.7 2006/11/02 09:18:20 hubert Exp $ *)

Require Export ZArith.
Require Export ZArith_dec.
Require Export Zdiv.

Theorem Znotzero : forall x:Z, {x <> 0%Z} + {x = 0%Z}.
Proof.
intro x.
 elim (Z_eq_dec x 0); auto.
Qed.

