(* Load Programs. *)(**************************************************************************)
(*                                                                        *)
(* Proof of the Knuth-Morris-Pratt Algorithm.                             *)
(*                                                                        *)
(* Jean-Christophe Filliâtre (LRI, Université Paris Sud)                  *)
(* November 1998                                                          *)
(*                                                                        *)
(**************************************************************************)

(* The terminations of both initnext and kmp involve a lexicographic
 * order relation. *)

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

(* Instantiation on Z x Z *)

Require Import ZArith.
Require Import Zwf.

Definition prodZZ := {x_ : Z &  Z}.

Definition pairZ (x y:Z) : prodZZ := existS (fun _:Z => Z) x y.

Definition lexZ := lex (Zwf 0) (Zwf 0).

Definition lexZ_well_founded :=
  lex_well_founded (Zwf_well_founded 0) (Zwf_well_founded 0).
