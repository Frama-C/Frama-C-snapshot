/* run.config_pruntime
   OPT: -wp -wp-model Runtime -wp-no-logicvar -journal-disable -wp-proof simplify -wp-print -wp-verbose 2
*/


/* ---------------------------------------------------------------------------*/
/* TODO: many funny examples at 
 *       http://graphics.stanford.edu/~seander/bithacks.html.
 */
/* ---------------------------------------------------------------------------*/
int X,Y;
char T[10];

struct str { char c; int i; };
struct str1 { int a; int b; int c; int d; } S1;
struct str2 { int x; int y; int z; } S2;

/* ---------------------------------------------------------------------------*/

#define NULL ((void*)0)


//@ ensures \result == 0;
int null_is_zero (void) {
  void * p = NULL;
  return (int) p;
}

//@ ensures c == 0 ==> \result == \null;
int * null (int c) {
  int * p = c ? &X : NULL;
  return p;
}
/* ---------------------------------------------------------------------------*/

/* For numeric address as pointeur, see [tests/wp/numeric_addr.c] */

/*@ behavior bigendian : ensures \result == 1;
 */
char ch_interp (void) {
  int x = 0x01020304;
  char * p = &x;
  char c = *p;
  return c;
}
/*
Lemma wp_ch_interp_bigendian_post_1 : 

intros M2 M3 M4 M5.
subst M5.
rewrite bits_of_sint8_of_bits.
subst M4.
repeat rewrite load_store_same.
rewrite uint32_of_bits_of_uint32.
subst M3.
rewrite load_store_disj;
  [ | apply base_sep; rewrite x_0_name; rewrite p_0_name; 
      rewrite uniq_name; auto with zarith ].
rewrite load_store_incl_part; unfold rt_incl; auto with zarith.
rewrite Zminus_diag.

Remains :
  sint8_of_bits (bits_part (bits_of_sint32 16909060) 0 8) = 1
*/

//@ assigns T[1..3];
void range (void) {
  T[1] = 1;
  T[2] = 2;
  T[3] = 3;
}

/*@ behavior bigendian : ensures \result == 3;
 */
int cast4 (void) {
  int * p = T;
  T[0] = 0; T[1] = 0; T[2] = 0; T[3] = 3;
  return *p;
}
/*

   
intros.
subst __retres mb3 mb2 mb1 mb0 p.
rewrite addr_of_pointer_of_addr.
unfold rt_shift.
repeat (rewrite simpl_as_int; [ | rewrite is_in_format_sint8; omega]).

replace (rt_vaddr ma X_T + 0) with (rt_vaddr ma X_T) by omega.

Hint Rewrite bits_concat_size rt_to_bits_size sint8_format_size : smp_rt_size.

erewrite (store_concat mb); eauto.
3: erewrite (store_concat mb); eauto.
4: erewrite (store_concat mb); eauto.

4: autorewrite with smp_rt_size; auto with zarith.
3: autorewrite with smp_rt_size; auto with zarith.
2: autorewrite with smp_rt_size; auto with zarith.

rewrite load_store_same; autorewrite with smp_rt_size; auto with zarith.

rewrite rt_int_from_bits.
rewrite big_b32_to_bbits; 
  [ |autorewrite with smp_rt_size; omega | ].

rewrite bits_concat_nth_byte_left; [ | autorewrite with smp_rt_size; omega].
rewrite bits_concat_nth_byte_left; [ | autorewrite with smp_rt_size; omega].
rewrite bits_concat_nth_byte_left; [ | autorewrite with smp_rt_size; omega].

rewrite bits_concat_nth_byte_left; [ | autorewrite with smp_rt_size; omega].
rewrite bits_concat_nth_byte_left; [ | autorewrite with smp_rt_size; omega].
erewrite (bits_concat_nth_byte_right _ _ 1); eauto; autorewrite with smp_rt_size; try omega.

rewrite bits_concat_nth_byte_left; [ | autorewrite with smp_rt_size; omega].
erewrite (bits_concat_nth_byte_right _ _ 2); eauto; autorewrite with smp_rt_size; try omega.

erewrite (bits_concat_nth_byte_right _ _ 3); eauto; autorewrite with smp_rt_size; try omega.

repeat rewrite nth_byte_0; autorewrite with smp_rt_size; auto.

unfold cint_of_bits.
rewrite sint32_format_sign.
rewrite ite_true.
rewrite sint32_format_size.
rewrite sint_of_bits_def; [ | omega].
repeat (rewrite concat_bytes_left; [ | omega]).
rewrite mbyte_to_bbits_def; autorewrite with smp_rt_size; try omega.
erewrite rt_to_bits_zero; eauto; autorewrite with smp_rt_size; try omega.
rewrite bit_of_bool_false; ring_simplify.

---------------------
Another one :




*/

/* ---------------------------------------------------------------------------*/

/*@ requires \valid (p1);
    ensures p1->b == 0;
*/
void struct_cast (struct str1 * p1) {
  struct str2 * p2 = (struct str2 *) p1;
  //@ assert \valid (p2);
  p2->y = 0;
}
//@ requires \valid (p2);
void invalid_struct_cast (struct str2 * p2) {
  struct str1 * p1 = (struct str1 *) p2;
  //@ assert ko : \valid(p1);
}

/* === About memcpy ----------------------------------------------------------*/

/*@
  requires \valid (src + (0..n-1));
  requires \valid (dest+(0..n-1));
  requires \separated (src + (0..n-1), dest+(0..n-1));

  ensures \forall integer i; 0 <= i < n ==> dest[i] == \old(src[i]);
  assigns dest[0..n-1] \from src[0..n-1] ;
*/
void memcpy(char *dest, char *src, unsigned long n);

void use_memcpy_on_int (int * p) {
  int x;
  memcpy (&x, p, sizeof (x));
  //@ assert x == \at(*p,Pre);
}
/*
Proof.

intros.
apply same_bits_same_val.
subst mb_3.
unfold rt_shift in *; simpl; rewrite Zplus_0_r.
unfold rt_vzone.
erewrite rt_valloc_size; [ | subst ma0; eauto].
rewrite load_store_same.


(* 1st byte 8 *)
eapply (eq_bits_split _ _ 32 _ _ 8 24); auto with zarith.

eapply same_int_val_same_bits in H1; 
  try instantiate (1 := 0); 
  try rewrite sint8_format_size;
  try rewrite bits_part_size;
  try rewrite rt_load_size;
  try rewrite size_zone;
  auto with zarith.

rewrite load_store_incl_part in H1; [ |
  unfold rt_incl; repeat rewrite addr_zone; repeat rewrite size_zone; auto with zarith].
repeat rewrite addr_zone in H1; rewrite size_zone in H1.
unfold rt_shift in *. 
  simpl in H1; repeat rewrite Zplus_0_r in H1;
  rewrite Zminus_diag in H1.

rewrite H1.
eapply bits_part_rt_load; eauto; [ |
  unfold rt_incl; repeat rewrite addr_zone; repeat rewrite size_zone; auto with zarith].
rewrite addr_zone; rewrite Zplus_0_r; auto.

auto with zarith.

(* Same thing for 2d 3rd 4th byte ... *)

eapply (eq_bits_split _ _ 24 _ _ 8 16); auto with zarith;
  repeat rewrite bits_part_of_bits_part; simpl.

Save.

 */

void use_memcpy_on_struct (struct str * p) {
  struct str x;
  memcpy (&x, p, sizeof (x));
  //@ assert x == \at(*p, Pre);
}
/*

intros.

rewrite EqrecDef_str.
split; erewrite <- bits_part_vs_access; eauto;
  [ rewrite Loaded_str_c;
    destruct Finfo_str_c as [Hf1 [Hf2 Hf3]];
    rewrite Hf1; rewrite Hf2; rewrite Hf3
  | rewrite Loaded_str_i;
    destruct Finfo_str_i as [Hf1 [Hf2 Hf3]];
    rewrite Hf1; rewrite Hf2; rewrite Hf3
]; clear Hf1 Hf2 Hf3 .

(* 1st field is a [char] *)

replace 0 with (8*0) by auto with zarith .
rewrite <- (H2 0); auto with zarith; clear H2.
apply same_bits_same_val. 
symmetry. eapply bits_part_rt_load; eauto;
  unfold rt_vzone, rt_incl, rt_shift; 
  repeat rewrite addr_zone; repeat rewrite size_zone; split; auto with zarith.

erewrite rt_valloc_size; subst ma0; eauto; auto with zarith.

(* 2d field *)

see. use_memcpy_on_int

 */
/* ---------------------------------------------------------------------------*/
