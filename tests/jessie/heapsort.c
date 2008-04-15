/************/
/* Heapsort */
/************/


/*** logic type: int_array *************************************************/

/*@ axiomatic IntArray {
  @  type int_array;
  @  logic int access(int_array a, integer i);
  @  logic int_array update(int_array a, integer i, int v);
  @  axiom access_update_eq : 
  @    \forall int_array a, integer i, int v; access(update(a, i, v), i) == v;
  @  axiom access_update_neq : 
  @    \forall int_array a, integer i, integer j, int v; 
  @       i != j ==> access(update(a, i, v), j) == access(a, j);
  @ }
  @*/

/*** permutation ************************************************************/

/*@ predicate Swap(int_array a1, int_array a2, integer i, integer j) =
  @   access(a1, i) == access(a2, j) &&
  @   access(a1, j) == access(a2, i) &&
  @   \forall integer k; k != i ==> k != j ==> access(a1, k) == access(a2, k) ;
  @*/

/*@ axiomatic Permut {
  @  predicate Permut(int_array a1, int_array a2, integer l, integer h);
  @  axiom Permut_refl: 
  @    \forall int_array a; \forall integer l, integer h; Permut(a, a, l, h);
  @  axiom Permut_sym: 
  @   \forall int_array a1, int_array a2, integer l, integer h; 
  @     Permut(a1, a2, l, h) ==> Permut(a2, a1, l, h);
  @  axiom Permut_trans: 
  @    \forall int_array a1, int_array a2, int_array a3, integer l, integer h; 
  @      Permut(a1, a2, l, h) ==> Permut(a2, a3, l, h) ==> Permut(a1, a3, l, h);
  @  axiom Permut_swap: 
  @    \forall int_array a1, int_array a2, integer l, integer h, integer i, integer j; 
  @   l <= i <= h ==> l <= j <= h ==> Swap(a1, a2, i, j) ==> Permut(a1, a2, l, h);
  @ axiom Permut_extend: 
  @   \forall int_array a1, int_array a2, integer l, integer h, integer ll, integer hh; 
  @     Permut(a1, a2, l, h) ==> ll <= l ==> h <= hh ==> Permut(a1, a2, ll, hh);
  @ }
  @ */

/*** contents function *******************************************************/

/*@ axiomatic ArrayContents {
  @   logic int_array contents{L}(int* a) ;
  @     // reads a[..];
  @  axiom access_contents{L} : 
  @   \forall int* a; \forall integer i; 
  @        access(contents(a), i) == a[i];
  @ }
  @*/

/*** sorted property *********************************************************/

/*@ predicate Sorted{L}(int* a, integer l, integer h) =
  @   \forall integer i; l <= i < h ==> a[i] <= a[i+1] ;
  @*/

/*** heap property ***********************************************************/

/*@ predicate Hnode{L}(int* a, integer i, integer h) =
  @   (2*i+1 <= h ==> a[i] >= a[2*i+1]) &&
  @   (2*i+2 <= h ==> a[i] >= a[2*i+2]) ;
  @*/

/*@ predicate H{L}(int* a, integer l, integer h) =
  @   \forall integer i; l <= i <= h ==> Hnode{L}(a, i, h) ;
  @*/

//@ lemma H_init{L}: \forall int* a, integer l, integer h; l <= h < 2*l+1 ==> H{L}(a, l, h);

//@ lemma H_reduce{L}: \forall int* a, integer h; 0 < h ==> H{L}(a, 0, h) ==> H{L}(a, 1, h-1);

/*@ lemma H_max{L}: 
  @   \forall int* a, integer h; 
  @     H{L}(a, 0, h) ==> \forall integer i; 0 <= i <= h ==> a[0] >= a[i];
  @*/

/*** arith *******************************************************************/

//@ lemma div2_1 : \forall int i; 0 <= i ==> 0 <= i/2 <= i;

//@ lemma div2_2 : \forall int i; 0 < i ==> 0 <= i/2 < i;

//@ lemma div2_3 : \forall int i; 0 <= i ==> i-1 < 2*(i/2)+1;

/*@ lemma div2_4 : \forall int i, int k; 
  @   0 <= i ==> 0 <= k ==> k != (i-1)/2 ==> 2*k+1 != i;
  @*/

/*@ lemma div2_5 : \forall int i, int k; 
  @   0 <= i ==> 0 <= k ==> k != (i-1)/2 ==> 2*k+2 != i;
  @*/

/*****************************************************************************/

/*@ requires
  @   0 <= low <= high && \valid_range(a, low, high) &&
  @   H(a, low+1, high);
  @ assigns
  @   a[low..high];
  @ ensures
  @   Permut(contents(a), \old(update(contents(a), low, v)), low, high) &&
  @   H(a, low, high);
  @*/
void sift_down(int* a, unsigned int low, unsigned int high, int v) {
  unsigned int i = low, child;
  /*@ loop invariant
    @   low <= i <= high &&
    @   Permut(contents(a), \at(contents(a), Pre), low, high) &&
    @   (\forall int k; low < k <= high ==> Hnode(a, k, high)) &&
    @   (low < i ==> Hnode(a, low, high)) &&
    @   (low <= (i-1)/2 ==> a[(i-1)/2] >= v);
    @ loop assigns
    @   a[low..high];
    @ loop variant
    @   high - i;
    @*/
  while ((child = 2*i+1) <= high) {
    if (child+1 <= high && a[child+1] >= a[child]) child++;
    if (v >= a[child]) break;
    a[i] = a[child];
    //@ assert Hnode(a, i, high);
    i = child;
  }
  a[i] = v;
}

/*@ requires
  @   0 <= n && \valid_range(a, 0, n-1);
  @ ensures
  @   Permut(contents(a), \old(contents(a)), 0, n-1) &&
  @   Sorted(a, 0, n-1);
  @*/
void heapsort(int* a, unsigned int n) {
  unsigned int i;
  if (n <= 1) return;
  /*@ loop invariant
    @   0 <= i < n &&
    @   Permut(contents(a), \at(contents(a), Pre), 0, n-1) &&
    @   H(a, i, n-1);
    @ loop variant i;
    @*/
  for (i = n/2; i >= 1; i--) sift_down(a, i-1, n-1, a[i-1]);
  /*@ loop invariant
    @   0 <= i < n &&
    @   Permut(contents(a), \at(contents(a), Pre), 0, n-1) &&
    @   H(a, 0, i) && Sorted(a, i+1, n-1) &&
    @   \forall int k1, int k2; 0 <= k1 <= i ==> i < k2 < n ==> a[k1] <= a[k2];
    @ loop variant i;
    @*/
  for (i = n-1; i >= 1; i--) {
    int tmp = a[i];
    a[i] = a[0];
    sift_down(a, 0, i-1, tmp);
  }
}

/* test 
int main() {
  int i;
  int t[10] = { 3,5,1,0,6,8,4,2,9,7 };
  heapsort(t, 10);
  for (i = 0; i < 10; i++) printf("%d ", t[i]);
}
*/

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make heapsort"
End:
*/
