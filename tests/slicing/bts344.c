/* run.config
   OPT: -check -slice-return main -journal-disable -then-on 'Slicing export' -print
   OPT: -check -slice-return main_bis -main main_bis -journal-disable -then-on 'Slicing export' -print
*/

int X, Y ;
void h(int x);

/*@ ensures X == \old(X) + x; */
void k(int x) {
  X += x ;
  Y ++ ;
}
void h(int x) {
  X += x ;
  Y ++ ;
}
void f(int x, ...) {
  void (*q) (int) = &h;
  void (*p) (int) = &k;
  h(x);
}
int main (int x) {
  f (1) ;
  h(2) ;
  k(3);
 return X ;
}

int main_bis (void) {
  void (*p) (int) = &k;
  (*p)(1) ;
  return Y;
}
