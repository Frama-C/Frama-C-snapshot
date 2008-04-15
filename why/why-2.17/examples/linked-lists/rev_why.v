
Require Import Why.

Require Export LinkedLists.

(* Why obligation from file , characters 1173-1631 *)
Lemma rev_po_1 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (r:pointer) (Post7:r = p) (p2:pointer)
   (Post1:p2 = null), well_founded ll_order.
Proof.
auto.
Qed.

(* should go in PolyList *)
Lemma app_rev_cons :
 forall (A:Set) (l1 l2:list A) (x:A),
   app (rev l1) (cons x l2) = app (rev (cons x l1)) l2.
Proof.
intros; simpl.
rewrite app_ass; auto.
Qed.


(* Why obligation from file , characters 1534-1622 *)
Lemma rev_po_2 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (r:pointer) (Post7:r = p) (p2:pointer)
   (Post1:p2 = null) (Variant1:StorePointerPair) (Ltl0:pointer_store)
   (p3 r1:pointer) (Pre3:Variant1 = store_pointer_pair Ltl0 r1)
   (Pre2: EX lp : plist
         | ( EX lr : plist
            | llist Ltl0 p3 lp /\
              llist Ltl0 r1 lr /\
              disjoint lp lr /\
              (forall l:plist,
                 llist Ltl p0 l -> app (rev lr) lp = rev l)))
   (Test2:r1 <> null) (q:pointer) (Post5:q = r1) (r2:pointer)
   (Post2:r2 = pget Ltl0 r1) (Ltl1:pointer_store)
   (Post3:Ltl1 = pset Ltl0 q p3) (p4:pointer) (Post4:p4 = q),
   ( EX lp : plist
    | ( EX lr : plist
       | llist Ltl1 p4 lp /\
         llist Ltl1 r2 lr /\
         disjoint lp lr /\
         (forall l:plist, llist Ltl p0 l -> app (rev lr) lp = rev l))) /\
   ll_order (store_pointer_pair Ltl1 r2) (store_pointer_pair Ltl0 r1).
Proof.
intuition.
elim Pre2; clear Pre2; intuition.
elim H; clear H; intuition.
subst.
inversion H.
absurd (r1 = null); intuition.
exists (cons r1 x); exists l; subst; intuition.
unfold llist; apply Path_cons; intuition.
 rewrite PointerStore.get_set_same; auto.
apply llist_pset_same; auto.
unfold disjoint in H1; intuition.
apply (H7 r1); auto.
rewrite <- H6; auto with *.
apply llist_pset_same; auto.
apply llist_not_starting with Ltl0; auto.
apply disjoint_cons.
rewrite H6; auto.
apply llist_not_starting with Ltl0; auto.
rewrite app_rev_cons.
rewrite H6; apply H3; auto.
unfold ll_order, store_pointer_pair.
elim Pre2; clear Pre2; intuition.
elim H; clear H; intuition.
subst.
inversion H; intuition.
exists l; exists x0; intuition.
apply llist_pset_same; auto.
apply llist_not_starting with Ltl0; auto.
rewrite <- H6; simpl; omega.
Qed.

(* Why obligation from file , characters 1173-1631 *)
Lemma rev_po_3 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (r:pointer) (Post7:r = p) (p2:pointer)
   (Post1:p2 = null) (Variant1:StorePointerPair) (Ltl0:pointer_store)
   (p3 r1:pointer) (Pre3:Variant1 = store_pointer_pair Ltl0 r1)
   (Pre2: EX lp : plist
         | ( EX lr : plist
            | llist Ltl0 p3 lp /\
              llist Ltl0 r1 lr /\
              disjoint lp lr /\
              (forall l:plist,
                 llist Ltl p0 l -> app (rev lr) lp = rev l)))
   (Test2:r1 <> null) (Ltl1:pointer_store) (p4 r2:pointer)
   (Post9:( EX lp : plist
           | ( EX lr : plist
              | llist Ltl1 p4 lp /\
                llist Ltl1 r2 lr /\
                disjoint lp lr /\
                (forall l:plist,
                   llist Ltl p0 l -> app (rev lr) lp = rev l))) /\
          ll_order (store_pointer_pair Ltl1 r2)
            (store_pointer_pair Ltl0 r1)),
   ll_order (store_pointer_pair Ltl1 r2) Variant1.
Proof.
intros; subst Variant1; intuition.
Qed.

(* Why obligation from file , characters 1222-1445 *)
Lemma rev_po_4 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (r:pointer) (Post7:r = p) (p2:pointer)
   (Post1:p2 = null),
    EX lp : plist
   | ( EX lr : plist
      | llist Ltl p2 lp /\
        llist Ltl r lr /\
        disjoint lp lr /\
        (forall l:plist, llist Ltl p0 l -> app (rev lr) lp = rev l)).
Proof.
intros; subst.
exists (nil (A:=pointer)).
elim (is_list_llist Ltl p0 Pre4); intros l Hl; exists l.
intuition.
rewrite (llist_function _ _ _ _ Hl H).
intuition.
Qed.

(* Why obligation from file , characters 1637-1639 *)
Lemma rev_po_5 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (r:pointer) (Post7:r = p) (p2:pointer)
   (Post1:p2 = null) (Ltl0:pointer_store) (p3 r1:pointer)
   (Post6:( EX lp : plist
           | ( EX lr : plist
              | llist Ltl0 p3 lp /\
                llist Ltl0 r1 lr /\
                disjoint lp lr /\
                (forall l:plist,
                   llist Ltl p0 l -> app (rev lr) lp = rev l))) /\
          r1 = null),
    EX l : plist
   | llist Ltl0 p3 l /\
     (forall l0:plist, llist Ltl p0 l0 -> l = rev l0).
Proof.
intuition.
elim H; clear H; intros lp H; elim H; clear H; intro lr; intuition.
exists lp; intuition.
subst r1.
inversion H.
rewrite <- (H4 l0); auto.
rewrite <- H5; trivial.
subst.
inversion H0.
Qed.


