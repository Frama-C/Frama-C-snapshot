
Require Import Why.

Require Import LinkedLists.

(* Why obligation from file , characters 1039-1200 *)
Lemma length_po_1 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (n:Z) (Post7:n = 0%Z),
   well_founded ll_order.
Proof.
auto.
Qed.

(* Why obligation from file , characters 1061-1061 *)
Lemma length_po_2 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (n:Z) (Post7:n = 0%Z)
   (Variant1:StorePointerPair) (p2:pointer)
   (Pre3:Variant1 = store_pointer_pair Ltl p2) (Pre2:is_list Ltl p2)
   (Test2:p2 = null) (result0:bool) (Post3:result0 = false),
   if result0
   then p2 = null /\ true = false \/ p2 <> null /\ true = true
   else p2 = null /\ false = false \/ p2 <> null /\ false = true.
Proof.
simple_destruct result0; intuition.
Qed.

(* Why obligation from file , characters 1061-1061 *)
Lemma length_po_3 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (n:Z) (Post7:n = 0%Z)
   (Variant1:StorePointerPair) (p2:pointer)
   (Pre3:Variant1 = store_pointer_pair Ltl p2) (Pre2:is_list Ltl p2)
   (Test1:p2 <> null) (result0:bool) (Post4:result0 = true),
   if result0
   then p2 = null /\ true = false \/ p2 <> null /\ true = true
   else p2 = null /\ false = false \/ p2 <> null /\ false = true.
Proof.
simple_destruct result0; intuition.
Qed.

(* Why obligation from file , characters 1157-1193 *)
Lemma length_po_4 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (n:Z) (Post7:n = 0%Z)
   (Variant1:StorePointerPair) (n1:Z) (p2:pointer)
   (Pre3:Variant1 = store_pointer_pair Ltl p2) (Pre2:is_list Ltl p2)
   (Test4:p2 = null /\ true = false \/ p2 <> null /\ true = true)
   (n2:Z) (Post1:n2 = (n1 + 1)%Z) (p3:pointer)
   (Post2:p3 = pget Ltl p2),
   is_list Ltl p3 /\
   ll_order (store_pointer_pair Ltl p3) (store_pointer_pair Ltl p2).
Proof.
intuition.
discriminate H1.
discriminate H1.
inversion Pre2.
rewrite H in H0; intuition.
case (eq_null_dec p3); intro.
clear Post2; subst p3; auto.
subst p3; auto.
unfold store_pointer_pair, ll_order.
generalize (is_list_llist _ _ Pre2).
intros [l Hl].
inversion Hl; intuition.
exists l0; exists l; subst; intuition.
rewrite <- H4; simpl; omega.
Qed.

(* Why obligation from file , characters 1039-1200 *)
Lemma length_po_5 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (n:Z) (Post7:n = 0%Z)
   (Variant1:StorePointerPair) (p2:pointer)
   (Pre3:Variant1 = store_pointer_pair Ltl p2) (Pre2:is_list Ltl p2)
   (Test4:p2 = null /\ true = false \/ p2 <> null /\ true = true)
   (p3:pointer)
   (Post10:is_list Ltl p3 /\
           ll_order (store_pointer_pair Ltl p3)
             (store_pointer_pair Ltl p2)),
   ll_order (store_pointer_pair Ltl p3) Variant1.
Proof.
intros; rewrite Pre3; intuition.
Qed.

(* Why obligation from file , characters 1081-1096 *)
Lemma length_po_6 :
 forall (p0:pointer) (Ltl:pointer_store) (Pre4:is_list Ltl p0)
   (p:pointer) (Post8:p = p0) (n:Z) (Post7:n = 0%Z), is_list Ltl p.
Proof.
intros; subst p; intuition.
Qed.

