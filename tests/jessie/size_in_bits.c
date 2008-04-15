
struct S {
  int i;
  char a;
  int j;
  char c;
  char d;
};

void f() {
  struct S s;
  s.i = 0;
}

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make size_in_bits"
End:
*/
