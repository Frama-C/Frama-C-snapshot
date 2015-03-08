/* run.config 
   DONTRUN: test under construction
*/

/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-prop qed_ok
*/

struct S {
  int i;
  int a[];
};

/*@ requires s.i >= 0 && \valid(s.a+(0..(s.i-1)));
  @ */
void f(struct S s) {
  int j;
  /*@ loop invariant 0 <= j <= s.i && \forall int k; 0 <= k < j ==> s.a[k] == 0;
    @ loop variant s.i - j;
    @ */
  for (j = 0; j < s.i; j++) s.a[j] = 0;
  /*@ assert \forall int k; 0 <= k < s.i ==> s.a[k] == 0; */
}

struct S0 {
  int i0;
  int *a0;
};

/*@ requires s.i0 >= 0 && \valid(s.a0+(0..(s.i0-1)));
  @ */
void f0(struct S0 s) {
  int j;
  /*@ loop invariant qed_ok: 0 <= j <= s.i0 && \forall int k; 0 <= k < j ==> s.a0[k] == 0;
    @ loop variant qed_ok: s.i0 - j;
    @ */
  for (j = 0; j < s.i0; j++) s.a0[j] = 0;
  /*@ assert qed_ok: \forall int k; 0 <= k < s.i0 ==> s.a0[k] == 0; */
}

struct S1 {
  int i1;
  int a1[1];
};

/*@ requires s.i1 >= 0 && \valid_range(s.a1,0,s.i1-1);
  @ */
void f1(struct S1 s) {
  int j;
  /*@ loop invariant qed_ok: 0 <= j <= s.i1 && \forall int k; 0 <= k < j ==> s.a1[k] == 0;
    @ loop variant qed_ok: s.i1 - j;
    @ */
  for (j = 0; j < s.i1; j++) s.a1[j] = 0;
  /*@ assert qed_ok: \forall int k; 0 <= k < s.i1 ==> s.a1[k] == 0; */
}

