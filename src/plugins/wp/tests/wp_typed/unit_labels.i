/*@ predicate is_duplet{L}(int *a, integer i, integer j) = a[i] == a[j];
  @*/

/*@ requires \separated(a+(*pi),pi,pj);
  @ ensures is_duplet(a,*pi,*pj) ; 
  @*/
void duplet(int *a, int *pi, int *pj) { 
  a[*pi] = a[*pj] ;
  //@ assert PI: \at(*pi,Pre) == *pi ;
  //@ assert PJ: \at(*pj,Pre) == *pj ;
}
