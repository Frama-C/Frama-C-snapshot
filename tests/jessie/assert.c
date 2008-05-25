
//#pragma AnnotationPolicy(WeakPre)
//#pragma AbstractDomain(Box)

int f(int i) {
  assert (i > 0);
  return i;
}

int ff(int i) {
  int j = 1;
  assert (i+j > 0);
  return i;
}

void g() {
  assert (f(1) > 0);
}

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make assert"
End:
*/
