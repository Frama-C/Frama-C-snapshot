
//#pragma AnnotationPolicy(WeakPre)
//#pragma AbstractDomain(Box)
#pragma CheckArithOverflow(yes)

int f(int i) {
  //@ assert (i > 0);
  return i;
}

int ff(int i) {
  int j = 1;
  //@ assert (i+j > 0);
  return i;
}

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make annot_assert"
End:
*/
