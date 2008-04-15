/* run.config
   OPT: -slice-print -slice-return call_top -main call_top -journal-disable
   OPT: -slice-print -slice-return top      -main top -journal-disable
   OPT: -slice-print -slice-return top      -main call_top -journal-disable
   OPT: -slice-return called_by_top -main top -journal-disable
   OPT: -slice-return called_by_top -main call_top -journal-disable
*/

int called_indirectly_by_top (int x) {
  x++ ;
  return x ;
}

int called_by_top (int x) {
  x++ ;
  int z = called_indirectly_by_top (x) ;
  return z ;
}

int top (int x, ...) {
  x++ ;
  int z = called_by_top (x) ;
  return z;
}

int call_top (int y) {
  y++;
  int z = top (y) ;
  return z ;
}
