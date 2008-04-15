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

(* $Id: WhyBool.v,v 1.2 2006/11/02 09:18:20 hubert Exp $ *)

Require ZArith.
Require Sumbool.
Require WhyCoqCompat.

Definition annot_bool :
  (b:bool) { b':bool | if b' then b=true else b=false }.
Proof.
Intro b.
Exists b. Case b; Trivial.
Save.

Definition if_then_else [A:Set; a:bool; b,c:A] := if a then b else c.
Implicits if_then_else.

(* Logical connectives *)

Definition spec_and := [A,B,C,D:Prop][b:bool]if b then A /\ C else B \/ D.

Definition prog_bool_and :
  (Q1,Q2:bool->Prop) (sig bool Q1) -> (sig bool Q2)
  -> { b:bool | if b then (Q1 true) /\ (Q2 true)
                     else (Q1 false) \/ (Q2 false) }.
Proof.
Intros Q1 Q2 H1 H2.
Elim H1. Intro b1. Elim H2. Intro b2. 
Case b1; Case b2; Intros.
Exists true; Auto.
Exists false; Auto. Exists false; Auto. Exists false; Auto.
Save.

Definition spec_or := [A,B,C,D:Prop][b:bool]if b then A \/ C else B /\ D.

Definition prog_bool_or :
  (Q1,Q2:bool->Prop) (sig bool Q1) -> (sig bool Q2)
  -> { b:bool | if b then (Q1 true) \/ (Q2 true)
                     else (Q1 false) /\ (Q2 false) }.
Proof.
Intros Q1 Q2 H1 H2.
Elim H1. Intro b1. Elim H2. Intro b2. 
Case b1; Case b2; Intros.
Exists true; Auto. Exists true; Auto. Exists true; Auto.
Exists false; Auto.
Save.

Definition spec_not:= [A,B:Prop][b:bool]if b then B else A.

Definition prog_bool_not :
  (Q:bool->Prop) (sig bool Q)
  -> { b:bool | if b then (Q false) else (Q true) }.
Proof.
Intros Q H.
Elim H. Intro b.
Case b; Intro.
Exists false; Auto. Exists true; Auto.
Save.

(** Conversely, we build a [sumbool] out of a boolean, 
    for the need of validations *)

Definition btest
  [q:bool->Prop][b:bool][p:(q b)] : {(q true)}+{(q false)} :=
  (<[b:bool](q b)->{(q true)}+{(q false)}>
   Cases b of true => [p](left (q true) (q false) p) 
      | false => [p](right (q true) (q false) p) 
   end 
   p).


(** Equality over booleans *)

Definition B_eq_dec : (x,y:bool){x=y}+{~x=y}.
Proof. Decide Equality. Qed.

Definition B_eq_bool := 
 [x,y:bool](bool_of_sumbool (B_eq_dec x y)).

Definition B_noteq_bool := 
 [x,y:bool](bool_of_sumbool (sumbool_not ? ? (B_eq_dec x y))).

(** Equality over type unit *)

Definition U_eq_dec : (x,y:unit){x=y}+{~x=y}.
Proof. Decide Equality. Qed.

Definition U_eq_bool := 
 [x,y:unit](bool_of_sumbool (U_eq_dec x y)).

Definition U_noteq_bool := 
 [x,y:unit](bool_of_sumbool (sumbool_not ? ? (U_eq_dec x y))).

