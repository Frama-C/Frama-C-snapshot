
struct S {
  int i: 1;
  int j: 1;
};

int f(struct S s) {
  return s.i + s.j;
}

/*
Local Variables:
compile-command: "LC_ALL=C make bitvector"
End:
*/
