
/*** permutation ************************************************************/

/*@ predicate Swap{L1,L2}(int a[], integer i, integer j) =
  @   \at(a[i],L1) == \at(a[j],L2) &&
  @   \at(a[j],L1) == \at(a[i],L2) &&
  @   \forall integer k; k != i && k != j ==> \at(a[k],L1) == \at(a[k],L2);
  @*/

/*@ predicate Permut{L1,L2}(int a[], integer l, integer h) 
  @   reads \at(a[..],L1),\at(a[..],L2) ;
  @*/

/*@ axiom Permut_refl{L}: 
  @   \forall int a[]; \forall integer l, integer h; Permut{L,L}(a, l, h) ;
  @*/

/*@ axiom Permut_sym{L1,L2}: 
  @   \forall int a[], integer l, integer h; 
  @     Permut{L1,L2}(a, l, h) ==> Permut{L2,L1}(a, l, h) ;
  @*/

/*@ axiom Permut_trans{L1,L2,L3}: 
  @   \forall int a[], integer l, integer h; 
  @     Permut{L1,L2}(a, l, h) && Permut{L2,L3}(a, l, h) ==> 
  @       Permut{L1,L3}(a, l, h) ;
  @*/

/*@ axiom Permut_swap{L1,L2}: 
  @   \forall int a[]; \forall integer l, integer h, integer i, integer j; 
  @   l <= i <= h && l <= j <= h && Swap{L1,L2}(a, i, j) ==> 
  @     Permut{L1,L2}(a, l, h) ;
  @*/

/*** sorted property *********************************************************/

/*@ predicate Sorted{L}(int a[], integer l, integer h) =
  @   \forall integer i; l <= i < h ==> a[i] <= a[i+1] ;
  @*/

