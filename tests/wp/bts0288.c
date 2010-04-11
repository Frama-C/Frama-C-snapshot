int cpt, loc, *p ;
/*@ assigns p[..], cpt ;
    ensures \forall int * q ; q != &cpt ==> *q == \old(*q);
    ensures \forall int * q ; q == &loc ==> *q == \old(*q);
*/
void f1(int x) {
  p[1] = x ;
  cpt = 1;
}
/*
Proof.
intros.
subst mem_379.
subst q_370.
rewrite acc_upd_disj; 
  [ | apply disj_base; rewrite base_id_cpt; rewrite base_id_loc; auto with zarith].
subst mem_383.

rename mem_386 into m.
rename addr_x_381 into ad_x.

1 subgoal
m : memory
ad_x : pointer Z
H : int_base_addr ad_x = 368
______________________________________(1/1)
acc (upd m (shift_pointer (acc m addr_p_382) 1) (acc m ad_x)) addr_loc_374 =
acc m addr_loc_374
*/

/*~~~ autres essais ~~~*/

/* This property is nor true because we don't know if (&loc # p) */
/*@ ensures \forall int * q ; q == &loc ==> *q == \old(*q);
*/
void f2 (void) {
  p[1] = 0;
}

int main (void) {return 0;}
