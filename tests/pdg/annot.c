/* run.config
  STDOPT: +"-fct-pdg f1 -main f1"
  STDOPT: +"-fct-pdg loop -main loop"
*/

int G;

int f1 (int x) {
  int a = 10;
    if (x < 10)
      x = 10;
L : x++;
    //@ assert x > G+a ;
    x = 3;
    // @ assert x < \at(x,L) ; TODO : \at not implemented yet
    return x;
}

int loop (int n) {
  int i, s = 0;

  /*@ loop invariant 0 <= i <= n ;
    @ loop variant n-i;
  */
  for (i = 0; i < n; i++)
    s += 2;
  return s;
}
