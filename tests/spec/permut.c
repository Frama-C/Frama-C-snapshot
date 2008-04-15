

/*@ axiomatic Permut {
  @   // permut{L1,L2}(t1,t2,n) is true whenever t1[0..n-1] in state L1 
  @   // is a permutation of  t2[0..n-1] in state L2 
  @  predicate permut{L1,L2}(double t1[], double t2[], integer n);
  @    // reads \at(t1[..],L1), \at(t2[..],L2);
  @  axiom permut_refl{L} :
  @    \forall double t[], integer n; permut{L,L}(t,t,n);
  @  axiom permut_sym{L1,L2} :
  @    \forall double t1[], double t2[], integer n; 
  @      permut{L1,L2}(t1,t2,n) ==> permut{L2,L1}(t2,t1,n) ;
  @  axiom permut_trans{L1,L2,L3} :
  @    \forall double t1[], double t2[], double t3[], integer n;
  @      permut{L1,L2}(t1,t2,n) && permut{L2,L3}(t2,t3,n) 
  @      ==> permut{L1,L3}(t1,t3,n) ;
  @  axiom permut_exchange{L1,L2} :
  @    \forall double t1[], double t2[], integer i, integer j, integer n;
  @        \at(t1[i],L1) == \at(t2[j],L2) &&
  @        \at(t1[j],L1) == \at(t2[i],L2) &&
  @        (\forall integer k; 0 <= k < n && k != i && k != j ==>
  @             \at(t1[k],L1) == \at(t2[k],L2)) 
  @        ==> permut{L1,L2}(t1,t2,n); 
  @ }
  @*/


