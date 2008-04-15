
int g;

//@ predicate gzero{L} = g == 0;

//@ ensures gzero && g == 0;
void f() {
  g = 0;
}

//@ ensures gzero && g == 0;
void h() {
  f();
}

/* 
Local Variables:
compile-command: "LC_ALL=C make glob"
End:
*/
