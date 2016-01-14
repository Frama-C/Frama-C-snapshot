/*@ logic integer fact(integer n) = n <= 1 ? 1 : n * fact (n-1) ; */
/*@ lemma fact_ge1: \forall integer n ; fact(n) >= 1 ; */
/*@ lemma fact_next: \forall integer n,m ; 
    0 < n <= m ==> (n*(fact(m)/fact(n))) == (fact(m)/fact(n-1)) ; */

/*@ ensures \result == fact(n) ; */
int F1(int n)
{
  if (n<=1) return 1;
  int p=1,i=2 ;
  /*@ 
    loop invariant 2 <= i <= (n+1) ;
    loop invariant p == fact(i-1) ;
    loop assigns p,i; 
  */
  while (i <= n) { p *= i ; i++; }
  return p;
}

/*@ ensures \result == fact(n) ; */
int F2(int n)
{
  int p=1,i=2 ;
  /*@
    loop invariant RANGE: n<=1 ? i==2 : 2 <= i <= (n+1) ;
    loop invariant PART:  n<=1 ? p==1 : p == fact(i-1) ;
    loop assigns p,i;
  */
  while (i <= n) { p *= i ; i++; }
  return p;
}

/*@ ensures \result == fact(n) ; */
int F4(int n)
{
  int p=1 ;
  /*@
    loop invariant RANGE: \at(n,Pre) <= 1 ? n == \at(n,Pre) : 1 <= n <= \at(n,Pre) ;
    loop invariant NEVER: \at(n,Pre) <= 1 ? p == 1 : p == fact(\at(n,Pre)) / fact(n) ;
    loop assigns p,n ;
  */
  while (n > 1) { 
    p *= n ; n--; 
  }
  return p;
}
