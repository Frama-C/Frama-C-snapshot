

/*@  predicate my_killing_prop(double uc, double up, int n) */



/*@ requires 2 <= N <= 2^^26 && 
  @      \exact(u0)==u0 && \exact(u1)==u1 &&
  @      \forall int k; 0 <= k <= N =>  |u0+k*(u1-u0)| <= 1
  @ ensures  \exact(\result)==u0+N*(u1-u0) &&
  @          \round_error(\result) <= (N)*(N+1)/2.*2^^(-53)
  @*/


double comput_seq(double u0, double u1, int N) {
  int i;
  double uprev, ucur,tmp;
  uprev=u0;
  ucur=u1;

  /*@ invariant 2 <= i && i <= N+1 && 
    @   \exact(ucur) ==u0+(i-1)*(u1-u0) &&
    @   \exact(uprev)==u0+(i-2)*(u1-u0) &&
    @   my_killing_prop(ucur,uprev,i-2) 
    @ variant N-i*/ 
  for (i=2; i<=N; i++) {
    tmp=2*ucur;
    /*@ assert tmp==2*ucur */
    tmp-=uprev;
    uprev=ucur;
    ucur=tmp;
  }
  return ucur;
}
