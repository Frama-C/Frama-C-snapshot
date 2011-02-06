
/*@ ensures *q == \old(*p) && *p == \old(*q) ;
  assigns *p,*q;
 */
void f (int *p,int*q) {
  int t = *p;
  *p = *q;
  *q = t;
  return;
}

