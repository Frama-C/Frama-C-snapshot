
Require Export caduceus_why.

Notation " p # f " := (acc f p) (at level 30, f at level 0).

(*
Hint Rewrite shift_zero : caduceus.
Hint Rewrite shift_shift : caduceus.
*)

(* tactic to prove the equality of two pointers *)
Ltac eq_pointer := 
  repeat rewrite shift_shift;
  progress auto with * ||
  match goal with
  | |- (shift ?P1 ?O1) = (shift ?P2 ?O2) => solve [ring O1; ring O2; auto with *]
  | |- (shift ?P1 ?O1) = ?P2 => solve [ring O1; auto with *]
  | |- ?P1 = (shift ?P2 ?O2) => solve [ring O2; auto with *]
  end.

(* tactic to prove the disequality of two pointers *)
Ltac neq_pointer := 
  repeat rewrite shift_shift; 
  progress auto with * ||
  match goal with
  | h: base_addr ?P1 = base_addr ?P2 -> False |- ?P1 <> ?P2 =>
       red; intro; subst P1; auto
  | h: base_addr ?P2 = base_addr ?P1 -> False |- ?P1 <> ?P2 =>
       red; intro; subst P1; auto
  | h: base_addr ?P1 <> base_addr ?P2 |- ?P1 <> ?P2 =>
       red; intro; subst P1; auto
  | h: base_addr ?P2 <> base_addr ?P1 |- ?P1 <> ?P2 =>
       red; intro; subst P1; auto
  | |- (shift ?P1 ?O1) <> (shift ?P2 ?O2) =>
       solve [ apply neq_offset_neq_shift; auto with * ]
  end.

Hint Extern 0 (eq pointer _ _) => eq_pointer.
Hint Extern 0 (not (eq pointer _ _)) => neq_pointer.

(**************************************
  handling (acc (upd ...)) patterns
**************************************)
Ltac Acc_upd :=
  rewrite acc_upd ||
(*  (rewrite acc_upd_eq; [ idtac | eq_pointer ]) || *)
  (rewrite acc_upd_neq; [ idtac | neq_pointer ]); 
  auto with *.

Ltac caduceus := repeat Acc_upd.

(*******************************************
  handling valid, valid_index, valid_range
*********************************************)

Hint Resolve neq_base_addr_neq_shift.
Hint Resolve neq_offset_neq_shift.
Hint Resolve eq_offset_eq_shift.

Ltac valid := 
  repeat rewrite shift_shift; 
  repeat rewrite shift_zero;
  match goal with
  | id:(valid_range ?X1 ?X2 ?X3 ?X4) |-  (valid ?X1 (shift ?X2 ?X5))
  => solve [apply valid_range_valid_shift with X3 X4; auto with *]
  | id:(valid_range ?X1 ?X2 ?X3 ?X4) |-  (valid ?X1 ?X5)
  => solve [apply valid_range_valid with X3 X4; auto with *]
  | id:(valid_index ?X1 ?X2 ?X3) |- (valid ?X1 (shift ?X2 ?X3))
    => solve [apply valid_index_valid_shift; auto with *]
end.

Hint Extern 0 (valid _ _) => valid.

Ltac valid_index := match goal with
  | id:(valid_range ?X1 ?X2 ?X3 ?X4) |- (valid_index ?X1 ?X2 ?X5)
    => apply valid_range_valid_index with X3 X4; auto with *
end.

Hint Extern 0 (valid_index _ _ _) => valid_index.

(***************************************
  tactics for proving 'assigns' clauses 
****************************************)

Hint Resolve not_assigns_refl.

Ltac CleanAssigns :=
  match goal with
(*
  | id:(store_extends ?X1 ?X1) |- _ =>
      clear id; CleanAssigns
*)
  | id:(not_assigns ?X2 ?X1 ?X1 ?X3) |- _ =>
      clear id; CleanAssigns
  | _ => idtac
  end.


Ltac AssignsRec :=
  match goal with
  | id:(not_assigns ?X1 ?X2 ?X3 ?X4) |- (not_assigns ?X1 ?X2 ?X3 ?X4) =>
      exact id
(*
  |  |- (assigns ?X1 ?X2 ?X2 ?X4) =>
      exact (modifiable_refl (h:=X1) X2 (unchanged:=X4))
*)
(*
  | id:(fresh ?X1 ?X3) |- (modifiable ?X1 ?X7 (update ?X2 ?X3 ?X4) ?X6) =>
      apply
       (modifiable_fresh_update (h:=X1) (m1:=X7) (m2:=X2) (unch:=X6) (v:=X3)
          X4 id); ModRec
*)
(*
*)
(*
  |  |- (modifiable ?X1 ?X7 (update ?X2 ?X3 ?X4) ?X6) =>
      apply
       (modifiable_trans (h:=X1) (m1:=X7) (m2:=X2) (m3:=(
          update X2 X3 X4)) (unch:=X6)); ModRec
*)
  | id:(?X2 = ?X3) |- (not_assigns ?X1 ?X4 ?X2 ?X5) =>
      rewrite id; AssignsRec
(*
  | id:(modifiable ?X1 ?X2 ?X3 ?X4) |- (modifiable ?X5 ?X6 ?X3 ?X7) =>
      apply modifiable_trans_sub with (4:=id);
       [ subst; krakatoa; unfold inter_loc; intuition; fail 
       | Store | ModRec ]

  |  |- (not_assigns ?X1 ?X2 ?X3 (pset_singleton ?X4)) =>
       unfold not_assigns; intros tmpp validcond unchangedcond;
        generalize (pset_singleton_elim tmpp X4 unchangedcond);
        intro neq_pointer_cond;
        caduceus; progress auto

  |  |- (assigns ?X1 ?X2 ?X3 (union_loc ?X4 ?X5)) =>
*) 
  end.


Ltac Assigns := CleanAssigns; AssignsRec.
(*
Lemma not_not_in_pset_singleton : 
  forall z : Set,
  forall p : pointer z, ~ (not_in_pset p (pset_singleton p)).
Proof.
  intros z p; red; intro H.
  elim (pset_singleton_elim H); auto.
Save.
Hint Resolve not_not_in_pset_singleton.
*)
Ltac Unchanged :=
  match goal with
  | |- (not_in_pset ?X1 pset_empty) =>
      apply pset_empty_intro
  | |- (not_in_pset ?X1 (pset_singleton ?X2)) =>
      apply pset_singleton_intro;auto with *
  | |- (not_in_pset ?X1 (pset_union ?X2 ?X3)) =>
      apply pset_union_intro;auto with *
 end.


Hint Extern 0 (not_assigns _ _ _ _) => Assigns.
Hint Extern 0 (not_in_pset _ _) => Unchanged.
