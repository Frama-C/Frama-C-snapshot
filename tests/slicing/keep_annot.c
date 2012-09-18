/* run.config
   OPT: -check -context-valid-pointers -lib-entry -main f -slice-assert f  -then-on 'Slicing export' -print
   OPT: -check -context-valid-pointers -lib-entry -main f -slice-assert f -slicing-keep-annotations -then-on 'Slicing export' -print
   OPT: -check -context-valid-pointers -lib-entry -main L -slice-pragma L -slicing-keep-annotations -then-on 'Slicing export' -print
   OPT: -check -context-valid-pointers -lib-entry -main L -slice-pragma L -then-on 'Slicing export' -print
   OPT: -slice-return bts1110 -main bts1110 -then-on 'Slicing export' -print


*/

typedef struct { int a; double b; } las;

void g (las * p) {
  int i=0;
  while (i<5) {
    p->b  = (double)i / (double)(i+1);
    p->a = 1 + i;
    i++;
  }
  //@ assert 1<=p->a<=6;
  //@ assert 0.0<=p->b<=1.0;
}

//@assigns *p;
void f (las * p, int n, int m) { 
  g(p);
  //@ assert 0.0<=p->b<=1.0;
  //@ assert (\forall integer k; k < n ==> k < m);
} 


/* from BTS#448 from Dillon : the loop invariant is not in the slice.
 * After bug fix, it is kept when using option -slicing-keep-annotations
 * TODO: maybe it should also be kept without the option.
 */

void L (float u,int nn, float dabs[], float *y) {
  int ii;
  /*@ loop invariant (\forall integer k; u<=dabs[k]); */
  for (ii = nn-2; ii >= 0; ii--) { 
    *y = u - dabs[ii+1] * 2.0;
    //@ assert (\forall integer k; u<=dabs[k]);
  }
  //@slice pragma expr *y;
}

int bts1110(int x) {
  int y = 3;
  int z;
  //@ assert y == 3;
  y = 2;
  //@ assert x == 5;
  z = 5;
  x = x+1;
  return x;
}
