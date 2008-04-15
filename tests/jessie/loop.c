
//#pragma AnnotationPolicy(Invariants)
//#pragma AbstractDomain(Box)

/*@ requires n >= 0;
  @*/
void f(int n) {
  int i;
  for (i = 0; i < n; ++i) {
    //@ assert 0 <= i < n;
  }
}

/* 
Local Variables:
compile-command: "LC_ALL=C make -j loop"
End:
*/
