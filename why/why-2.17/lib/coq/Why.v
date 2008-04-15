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

(* $Id: Why.v,v 1.21 2006/11/02 09:18:20 hubert Exp $ *)

Require Export WhyCoqCompat.

Require Export WhyTuples.
Require Export WhyInt.
Require Export WhyBool.
Require Export WhyArrays.
Require Export WhyPermut.
Require Export WhySorted.
Require Export WhyTactics.
Require Export WhyExn.
Require Export WhyLemmas.
Require Export WhyPrelude.

Implicit Arguments well_founded [A].
Implicit Arguments well_founded_induction [ ].
Hint Unfold Zwf .

Open Scope Z_scope.
