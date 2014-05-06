/* run.config
   OPT: -val -cpp-command "gcc -C -E -DPTEST"  -journal-disable -then -machdep x86_64 -then -machdep x86_16
*/

#ifndef PTEST
#include <stdio.h>
#endif


int test1 () {
  unsigned long long u, w, *q ;
  u = (unsigned long long) -1LL ;
  q = (unsigned long long *) u;
  w = (unsigned long long) q;
  int c1 = (sizeof (q) == sizeof (u)) ;
  int c2 = (w == u) ;
#ifndef PTEST
  printf("%d==1  => %d==1\n", c1, c2);
#endif
  return c1!=1||c2==1 ;
}

int main() {

  return test1() ;

}
