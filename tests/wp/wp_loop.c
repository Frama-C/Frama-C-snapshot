
/* run.config_phoare
  OPT:  -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

//@ assigns \nothing;
void infinite (int c) {
  int s = 0;
  if (c) {
    //@ loop assigns s;
    while (1)
      s++;
    //@ assert 2 > 1;
  }
  //@ assert c == 0;
}

//@ assigns \nothing;
void loops (int c) {
  int x;

  if (c) x=0;
    x=3;


  //@ loop assigns x;
  while (c) { x=0;}
  x=1;


  if (c) 
    //@ loop assigns x;
    while(c) x=0;
  x=1;


  if (c) 
    //@ loop assigns x;
    do {x=0; } while(c);
  x=1;
  //@ assert x == 1;

}

/*@  assigns \nothing;
  ensures \result == 9 ; */
int classical_loop () {
  int i;
  int s = -1;

  /*@ loop invariant s + 1 == i && i <= 10; 
    @ loop assigns i, s;
   */
  for (i=0; i< 10; i++) s = i ;

  return s;
}

/*@ assigns \nothing;
  ensures \result == 5; */
int simple_loop (int c) {
  int x;
  int c = 0;

  /*@ loop invariant 0 <= c <= 6 && ((c==0 || x == c-1));
    @ loop assigns c, x;
    */
  for(c=0;c<=5;) {
    //   CEA_DUMP();
    x = c;
    c++;
    //   CEA_DUMP();
  }
// CEA_DUMP();
//@ assert c == 6;
  return x;
}

/*@ assigns \nothing;
  ensures \result == 6; */
int goto_natural_loop (int c) {
  int c = 0;
L : if (c > 5) goto R;
    c++;
    goto L;
R : return c;
}

int T[10];

//@ ensures \result == T + 5;
int * ptr_on_array () {
  int * p = T;
  int i;
  /*@ loop invariant i <= 5 && p == T+i;
    @ loop assigns i, p;
    */
  for (i = 0; i < 5; i++)
    p++;
  return p;
}

void assigns_loop_tab (void) {
  //@ loop assigns i, T[0..i-1];
  for (int i = 0; i < 10; i++) {
    T[i] = 0;
  }
}
//@ requires \valid (&(t[0..9]));
void assigns_loop_ptr (int * t) {
  /*@ loop assigns i, t[0..i-1];
      loop invariant (10 >= i >= 0); 
  */
  for (int i = 0; i < 10; i++) {
    t[i] = 0;
  }
}

/*@ ensures p_ok : T[9] == \old(T[9]);
    ensures p_ko : T[3] == \old(T[3]);
*/
void post_using_loop_assigns (void) {
  /*@ loop invariant i <= 7;
      loop assigns i, T[0..i-1];
    */
  for (int i = 0; i < 7; i++) {
    T[i] = 0;
  }
}

/*@ ensures p_ok : T[9] == \old(T[9]);
    ensures p_ko : T[3] == \old(T[3]);
*/
void post_using_loop_assigns_no_inv (void) {
  /*@ loop assigns i, T[0..6]; */
  for (int i = 0; i < 7; i++) {
    T[i] = 0;
  }
}


void loop_assert () {
  int i = 0;
  /*@ loop invariant i >= 0;
    @ loop assigns i; */
  while (i < 10) {
    ++i;
    //@ assert 0 < i <= 10;
  }
}

int loop_assigns () {
  int i = 0;
  int s = 0;
  /*@ loop assigns i, s; 
   */
  while (i < 10) {
    s += i;
    i++;
  }
  return s;
}
int loop_assigns_ko (void) {
  int s = 0;
  int i;
  //@ loop assigns s;
  for (i = 0; i < 10; i++) {
    s++;
  }
  return s;
}

int loop_var (int n) {
  int i, s = 0;
  /*@ loop assigns i, s;
    @ loop variant (n - i);
  */
  for (i = 0; i < n; i++) {
    s++;
  }
  return s;
}

int loop_inv_only (int n) {
  int i, s = 0;
  /*@ loop invariant 0 <= i && s == i;
      loop assigns i,s ; */
  for (i = 0; i < n; i++) {
    s++;
  }
  return s;
}

//@ ensures \result == -1 || T[\result] == a;
int find (int a) {
  int i;
  //@ loop assigns i;
  for (i = 0; i < 10; i++) {
    if (T[i] == a) return i;
  }
  return -1;
}
int inv_need_init () {
  int k = 4;
  int i = 0;
   /*@ loop invariant i <= 4; 
      loop assigns i;
    */
  while (i < k)
    i++;
  return i;
}

/*@ requires 0 <= m && m < M && M <= 10;
  @ ensures \forall int k; m <= k && k < M ==> T[k] == 0;
*/
void raz (int m, int M) {
  /*@ loop invariant m <= i && i <= M;
      loop invariant \forall int k; m <= k && k < i ==> T[k] == 0;
      loop assigns i, T[m..(i-1)];
  */
  for (int i = m; i < M; i++) {
    T[i] = 0;
  }
}
void loop_assigns_limit (void) {
  /*@ loop invariant T[i] == \at(T, Pre)[i];
      loop invariant 0 <= i;
      loop assigns i, T[0..(i-1)];
   */
  for (int i = 0; i < 10; i++)
    T[i] = 0;
}
/*----------------------------------------------------------------------------*/

//@ ensures \result >= n;
int bts494a (int n) {
  int i = 0 ;
  //@ loop invariant 0 <= i && (0 <= n ==> i <= n) && n == \at(n, Pre);
  while (i < n)
    i++ ;
  return i;
}

//@ ensures \result >= n;
int bts494b (int n) {
  int i = 0 ;
  /*@ loop invariant 0 <= i && (0 <= n ==> i <= n);
      loop assigns i;
      */
  while (i < n)
    i++ ;
  return i;
}

//@ ensures \result >= n;
int bts494c (int n) {
  int i = 0 ;
  while (i < n)
    //@ invariant 0 <= i < n && n == \at(n, Pre);
    i++ ;
  return i;
}

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/* Examples from DivEx1.c but adapted to substitute pointer by array
 * to be able to test is with Hoare. */

typedef struct {int ch11[10]; int ch12;} T1;
typedef struct {int ch21;      T1 ch22;} T2;

T2 t[20];

void loops_simple_assigns (void) {
  int i, j ;

  /*@ loop assigns i, j, t[0..19].ch21, t[0..19].ch22.ch11[..];
      loop invariant I0: oracle_ok: 0 <= i && i <= 20;
      loop invariant I1 : oracle_ok:
        \forall int k; 0 <= k && k < i ==> t[k].ch21 == 0;
  */
  for (i = 0 ; i < 20 ; i++) {
    t[i].ch21 = 0 ;
  /*@ loop assigns j, t[i].ch22.ch11[0..9];
      loop invariant J0: oracle_ok: 0 <= j && j <= 10;
  */
    for (j = 0 ; j < 10 ; j++)
      t[i].ch22.ch11[j] = 0 ;
    }
}

void loops_var_assigns (void) {
  int i, j ;

  /*@ loop assigns i, j, t[0..i-1].ch21, t[0..i-1].ch22.ch11[..];
      loop invariant I0: oracle_ok: 0 <= i && i <= 20;
      loop invariant I1 : oracle_ok:
        \forall int k; 0 <= k && k < i ==> t[k].ch21 == 0;
  */
  for (i = 0 ; i < 20 ; i++) {
    t[i].ch21 = 0 ;
  /*@ loop assigns j, t[i].ch22.ch11[0..j-1];
      loop invariant J0: oracle_ok: 0 <= j && j <= 10;
  */
    for (j = 0 ; j < 10 ; j++)
      t[i].ch22.ch11[j] = 0 ;
    }
}

/*----------------------------------------------------------------------------*/
/* This is the example of loop variant given in ACSL document.
 * Notive that this loop variant is ok even for \at(Pre, x < 0).
 * Notice also that the variant can be negative at the last iteration. */
void doc_acsl_variant (int x) {
  //@ loop variant x;
  while (x >= 0) {
    x -= 2;
  }
}
/*----------------------------------------------------------------------------*/
/* The ensures of [incr_tab] is proved, 
 * and it should be proved in [incr_tab] too,
 * but it seems that we don't manage to use the loop assigns... */

//@ ensures \forall integer k; 0 <= k < 10 ==> T[k] == \old(T[k]) + 1;
void incr_tab (void) {
  int x = 1;
  /*@ loop invariant @   0 <= i <= 10
    @   && (\forall integer k; 0 <= k < i ==> T[k] == \at(T[k],Pre) + x) 
    @   && x == 1 
    @   && (\forall integer k; i <= k  ==> T[k] == \at(T[k],Pre)) 
    @ ;
    */
  for (int i = 0; i < 10; i++) {
    T[i] = T[i] + x;
  }
}

//@ ensures \forall integer k; 0 <= k < 10 ==> T[k] == \old(T[k]) + 1;
void incr_tab_assigns (void) {
  int x = 1;
  /*@ loop invariant @   0 <= i <= 10
    @   && (\forall integer k; 0 <= k < i ==> T[k] == \at(T[k],Pre) + x) 
    // @   && x == 1 
    // @   && (\forall integer k; i <= k  ==> T[k] == \at(T[k],Pre)) 
    @ ;
    @ loop assigns i, T[0..i-1];
    */
  for (int i = 0; i < 10; i++) {
    T[i] = T[i] + x;
  }
}

/*@ requires \valid(t) 
          && \forall integer k; 0 <= k < 10 ==> \separated (&t, t+k);
  @ ensures \forall integer k; 0 <= k < 10 ==> t[k] == \old(t[k]) + 1;
*/
void incr_ptab (int * t) {
  int x = 1;
  /*@ loop invariant @   0 <= i <= 10
    @   && (\forall integer k; 0 <= k < i ==> t[k] == \at(t[k],Pre) + x) 
    @   && t == \at(t, Pre) 
    @   && \forall integer k; 0 <= k < 10 ==> \separated (&t, t+k)
    @   && x == 1 
    @   && (\forall integer k; i <= k  ==> t[k] == \at(t[k],Pre)) 
    @ ;
    */
  for (int i = 0; i < 10; i++) {
    t[i] = t[i] + x;
  }
}

/*@ requires \valid(t);
  @ ensures \forall integer k; 0 <= k < 10 ==> t[k] == \old(t[k]) + 1;
*/
void incr_ptab_assigns (int * t) {
  int x = 1;
  /*@ loop invariant @   0 <= i <= 10
    @   && (\forall integer k; 0 <= k < i ==> t[k] == \at(t[k],Pre) + x) 
    // @   && x == 1 
    // @   && (\forall integer k; i <= k  ==> t[k] == \at(t[k],Pre)) 
    @ ;
    @ loop assigns i, t[0..i-1];
    */
  for (int i = 0; i < 10; i++) {
    t[i] = t[i] + x;
  }
}
/*----------------------------------------------------------------------------*/
//@ requires \valid (&t[0..n-1]);
void valid_range_inv (int *t, int n) {
  //@ loop invariant n == \at(n,Pre);
  for (int i = 0; i < n; i++) {
    //@ assert \valid (t+i);
    t[i] = 0;
  }
}
/*----------------------------------------------------------------------------*/
