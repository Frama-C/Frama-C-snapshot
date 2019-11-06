/* run.config_qualif
   EXECNOW: rm -rf @PTEST_DIR@/oracle@PTEST_CONFIG@/@PTEST_NAME@.1.session/script
   OPT: -wp-prop=-lack,-tactic
   OPT: -wp-prop=tactic -wp-auto=wp:split
   OPT: -wp-prop=lack -wp-steps 300
 */
/*@ requires \valid(a+(0..n-1)) ;
  @ requires n >= 0 ;
  @ assigns a[0..n-1] ;
  @ ensures \forall int k ; 0 <= k < n ==> a[k] == v ;
  @ exits   \false;
*/
void init( int * a , int n , int v )
{
  /*@ loop assigns   Zone:  i,a[0..n-1] ;
    @ loop invariant Range: 0 <= i <= n ;
    @ loop invariant Partial: \forall int k ; 0 <= k < i ==> a[k] == v ;
    @ loop variant   Decr_i: n - i ;
  */
  for (int i = 0 ; i < n ; i++) a[i] = v ;
}
//-------------------------
int t1[10];
/*@ ensures \forall integer k; 0 <= k < 10 ==> t1[k] == v ;
  @ exits \false;
  @ assigns t1[0..9] ;
*/
void init_t1(int v) {
  unsigned i;
  /*@ loop assigns   Zone:  i,t1[0..9] ;
    @ loop invariant Range: 0 <= i <= 10 ;
    @ loop invariant Partial: \forall integer k ; 0 <= k < i ==> t1[k] ≡ v ;
    @ loop variant   Decr: 10 - i ;
  */
  for (i = 0 ; i < 10 ; i++) t1[i] = v ;
}
//-------------------------
int t2[10][20];
/*@ ensures \forall integer k, l; 0 <= k < 10 && 0 <= l < 20  ==> t2[k][l] == v;
  @ exits \false;
  @ assigns lack: t2[0..9][0..19];
  */
void init_t2_v1(int v) {

  unsigned i,j;
  /*@ loop assigns   lack: Zone_i:  i, j, t2[0..9][0..19];
    @ loop invariant Range_i: 0 <= i <= 10 ;
    @ loop invariant Partial_i: \forall integer k,l; 0 <= k < i && 0 <= l < 20 ==> t2[k][l] == v;
    @ loop variant   Decr_i: 10 - i ;
  */
  for(i = 0; i <= 9; i++) {
    /*@ loop assigns   lack: Zone_j:  j, t2[0..9][0..19];
      @ loop invariant Range_j: 0 <= j <= 20 ;
      @ loop invariant Partial_j: \forall integer l; 0 <= l < j ==> t2[i][l] == v;
      @ loop invariant Previous_i: \forall integer k,l; 0 <= k < i && 0 <= l < 20 ==> t2[k][l] == \at(t2[k][l], LoopEntry);
      @ loop variant   Decr_j: 20 - j ;
    */
    for(j = 0; j <= 19; j++) {
      t2[i][j] = v;
    }
    //@ assert Last_j: j==20;
    ;
  }
  //@ assert Last_i: i==10;
  ;
}
//-------------------------
/*@ ensures \forall integer k, l; 0 <= k < 10 && 0 <= l < 20  ==> t2[k][l] == v;
  @ exits \false;
  @ assigns tactic: t2[..][..];
  */
void init_t2_v2(int v) {

  unsigned i,j;
  /*@ loop assigns   tactic: Zone_i:  i, j, t2[..][..];
    @ loop invariant Range_i: 0 <= i <= 10 ;
    @ loop invariant Partial_i: \forall integer k,l; 0 <= k < i && 0 <= l < 20 ==> t2[k][l] == v;
    @ loop variant   Decr_i: 10 - i ;
   */
  for(i = 0; i <= 9; i++) {
    /*@ loop assigns   tactic: Zone_j:  j, t2[..][..];
      @ loop invariant Range_j: 0 <= j <= 20 ;
      @ loop invariant Partial_j: \forall integer l; 0 <= l < j ==> t2[i][l] == v;
      @ loop invariant Previous_i: \forall integer k,l; 0 <= k < i && 0 <= l < 20 ==> t2[k][l] == \at(t2[k][l], LoopEntry);
      @ loop variant   Decr_j: 20 - j ;
    */
    for(j = 0; j <= 19; j++) {
      t2[i][j] = v;
    }
    //@ assert Last_j: j==20;
    ;
  }
  //@ assert Last_i: i==10;
  ;
}
//-------------------------
//@ predicate MemSet20(int t2[20], integer n, integer v) = n <= 20 && \forall integer k ; 0 <= k < n ==> t2[k] == v;

/*@ ensures \forall integer k; 0 <= k < 10 ==> MemSet20(t2[k], 20, v);
  @ exits \false;
  @ assigns tactic: t2[..][..];
  */
void init_t2_v3(int v) {

  unsigned i,j;
  /*@ loop assigns   tactic: Zone_i:  i, j, t2[..][..];
    @ loop invariant Range_i: 0 <= i <= 10 ;
    @ loop invariant Partial_i: \forall integer k; 0 <= k < i ==> MemSet20(t2[k], 20, v);
    @ loop variant   V_i: 10 - i ;
   */
  for(i = 0; i <= 9; i++) {
    /*@ loop assigns   tactic: Zone_j:  j, t2[i][..];
      @ loop invariant Range_j: 0 <= j <= 20 ;
      @ loop invariant Partial_j: MemSet20(t2[i], j, v);
      @ loop variant   Decr_j: 20 - j ;
    */
    for(j = 0; j <= 19; j++) {
      t2[i][j] = v;
    }
    //@ assert Last_j: j==20;
    ;
  }
  //@ assert Last_i: i==10;
  ;
}
//-------------------------
/*@ ensures \forall integer k, l; 0 <= k < 10 && 0 <= l < 20  ==> t2[k][l] == v;
  @ assigns lack: t2[0..9][0..19];
  @ exits \false;
  */
void init_t2_bis_v1(int v) {

  unsigned i;
  /*@ loop assigns   lack: Zone:  i, t2[0..9][0..19];
    @ loop invariant Range: 0 <= i <= 10 ;
    @ loop invariant Partial: \forall integer k,l; 0 <= k < i && 0 <= l < 20 ==> t2[k][l] == v;
    @ loop variant   Decr: 10 - i ;
   */
  for(i = 0; i <= 9; i++) {
    init(&t2[i][0], 20, v);
    //@ assert Offset: &t2[i][0] == &t2[0][0] + 20*i;
  }
}
//-------------------------
/*@ ensures \forall integer k, l; 0 <= k < 10 && 0 <= l < 20  ==> t2[k][l] == v;
  @ assigns tactic: t2[..][..];
  @ exits \false;
  */
void init_t2_bis_v2(int v) {

  unsigned i;
  /*@ loop assigns   tactic: Zone:  i, t2[..][..];
    @ loop invariant Range: 0 <= i <= 10 ;
    @ loop invariant Partial: \forall integer k,l; 0 <= k < i && 0 <= l < 20 ==> t2[k][l] == v;
    @ loop variant   Decr: 10 - i ;
   */
  for(i = 0; i <= 9; i++) {
    init(&t2[i][0], 20, v);
    //@ assert Offset_i: &t2[i][0] == &t2[0][0] + 20*i;
    ;
  }
}
