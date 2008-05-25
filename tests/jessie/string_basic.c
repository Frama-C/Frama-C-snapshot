
/*@ requires \valid_index(p,i);
  @ behavior ok:
  @   assumes p[i] == 0;
  @   ensures \result == 1;
  @ behavior ko:
  @   assumes p[i] != 0;
  @   ensures \result == 0;
  @ */
int endsat(char* p, int i) {
  if (p[i] == '\0') return 1;
  return 0;
}

char gtoto[] = "toto";

void test1() {
  int r1 = endsat(gtoto, 4);
  //@ assert r1 == 1;
}

void test2() {
  char ltoto[] = "toto";
  int r1 = endsat(ltoto, 4);
  //@ assert r1 == 1;
}

char* gptoto = "toto";

void test1ptr() {
  int r1 = endsat(gptoto, 4);
  //@ assert r1 == 1;
}

void test2ptr() {
  char* lptoto = "toto";
  int r1 = endsat(lptoto, 4);
  //@ assert r1 == 1;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make string_basic"
End:
*/
