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

(* This file is a contribution by Christine Paulin *)

(* $Id: WhyExn.v,v 1.2 2006/11/02 09:18:21 hubert Exp $ *)

Implicit Arguments On.

Section Generic.

Variables U,T : Set.

(* Exception monad *)
Inductive EM : Set := Val : T -> EM | Exn : U -> EM.

Variables Q : U -> Prop.
Variables P : T -> Prop.

Definition qcomb : EM -> Prop 
      := [v]if v then P else Q.

Inductive QEM [A:Set] : Set 
      := Qval : A -> (QEM A) | Qexn : (u:U)(Q u) -> (QEM A).

Definition decomp : (v:EM)(qcomb v)->(QEM {t:T|(P t)}).
Intros [t|u]; Simpl; Intro p.
Apply Qval; Exists t; Trivial.
Apply Qexn with u; Trivial.
Defined.

Definition uncurry : (A:Set;P:A->Prop;C:Set)((x:A)(P x)->C)->{x:A|(P x)}->C.
Destruct 2; Intros.
Apply H with x; Trivial.
Defined.

Definition QEM_mon : (A,B:Set)(A->B)->(QEM A)->(QEM B).
Destruct 2; Intros.
Apply Qval; Auto.
Apply Qexn with u; Trivial.
Defined.

End Generic.

Definition decomp1 [T:Set][P:T->Prop]
                   [E1:Set][Q1:E1->Prop]
                   [v:(EM E1 T); pv: (qcomb Q1 P v)]
 : (QEM Q1 {t:T|(P t)})
 := (decomp pv).

Definition decomp2 [T:Set][P:T->Prop]
                   [E1:Set][Q1:E1->Prop][E2:Set][Q2:E2->Prop]
                   [v:(EM E1 (EM E2 T)); pv:(qcomb Q1 (qcomb Q2 P) v)]
 : (QEM Q1 (QEM Q2 {t:T|(P t)}))
 := (QEM_mon 2!Q1 (uncurry (!decomp1 T P E2 Q2)) (decomp pv)).

Definition decomp3 [T:Set][P:T->Prop][E1:Set]
                   [Q1:E1->Prop][E2:Set][Q2:E2->Prop][E3:Set][Q3:E3->Prop]
                   [v:(EM E1 (EM E2 (EM E3 T)));
                    pv:(qcomb Q1 (qcomb Q2 (qcomb Q3 P)) v)]
  : (QEM Q1 (QEM Q2 (QEM Q3 {t:T|(P t)}))) 
  := (QEM_mon 2!Q1 (uncurry (!decomp2 T P E2 Q2 E3 Q3)) (decomp pv)).

Definition decomp4 [T:Set][P:T->Prop][E1:Set]
                   [Q1:E1->Prop][E2:Set][Q2:E2->Prop]
		   [E3:Set][Q3:E3->Prop][E4:Set][Q4:E4->Prop]
                   [v:(EM E1 (EM E2 (EM E3 (EM E4 T))));
                    pv:(qcomb Q1 (qcomb Q2 (qcomb Q3 (qcomb Q4 P))) v)]
  : (QEM Q1 (QEM Q2 (QEM Q3 (QEM Q4 {t:T|(P t)}))) )
  := (QEM_mon 2!Q1 (uncurry (!decomp3 T P E2 Q2 E3 Q3 E4 Q4)) (decomp pv)).

