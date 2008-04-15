
//@ ensures \result != 0 <==> i && j;
int and(int i, int j) {
  int ret;
  if (i && j) ret = 1; else ret = 0;
  return ret;
}

//@ ensures \result != 0 <==> i || j;
int or(int i, int j) {
  if (i || j) return 1;
  return 0;
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j boolean"
End:
*/
