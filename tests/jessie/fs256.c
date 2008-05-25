
void f() {
  int i = 0;
  while (i < 10) { //@ invariant 0 <= i < 10;
    ++i;
    //@ assert 0 < i <= 10;
  }
}

/* 
Local Variables:
compile-command: "LC_ALL=C make fs256"
End:
*/
