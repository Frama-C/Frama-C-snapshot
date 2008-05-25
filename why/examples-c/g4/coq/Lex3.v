Require Export Relation_Operators.
Require Import Lexicographic_Product.

Set Implicit Arguments.
Unset Strict Implicit.

Section lexico.

Variables A B : Set.

Variable Ra : A -> A -> Prop.
Variable Rb : B -> B -> Prop.

Definition lex := lexprod A (fun _:A => B) Ra (fun _:A => Rb).

Lemma lex_well_founded :
 well_founded Ra -> well_founded Rb -> well_founded lex.
Proof.
intros wfa wfb.
exact
 (wf_lexprod A (fun _:A => B) Ra (fun _:A => Rb) wfa (fun _ => wfb)).
Qed.

End lexico.

Require Import ZArith.
Require Import Zwf.

Definition int3 := {x1 : Z &  {x2:Z & Z }}.

Definition prod3 (x1 x2 x3:Z) : int3 := 
   existS (fun _:Z => {x2:Z & Z}) x1 (existS (fun _:Z => Z) x2 x3).

Definition lex3 := lex (Zwf 0) (lex (Zwf 0) (Zwf 0)).

Definition lex3_well_founded :=
  lex_well_founded (Zwf_well_founded 0) 
    (lex_well_founded (Zwf_well_founded 0) (Zwf_well_founded 0)).

