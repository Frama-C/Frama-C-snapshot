/* run.config
   OPT: -check -calldeps -slice-return main -journal-disable -then-on 'Slicing export' -print
*/
/* Problem : f(1) should be sliced out. See BTS#326 */
int t[2] ;
int r;
void f (int i) {
  t[i] = i;
}

void g (void) {
  f(0) ;
  f(1) ;
}

int main (void) {
  g () ;
  r = t[0] ;
  return r;
}
