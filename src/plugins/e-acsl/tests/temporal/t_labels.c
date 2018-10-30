/* run.config
   COMMENT: Check that statements generated via temporal analysis are handled
   COMMENT: properly, i.e., if a statement has a label attached then all
   COMMENT: the generated statements are inserted after that label
*/

void foo(int *a, int *b) {
  int t = *a;
  *a = *b;
  *b = t;
}

int *Q;
int* bar() {
RET:
  return Q;
}

int main(int argc, const char **argv) {
  int a = 11,
      b = 12;

  int *p = &a,
      *q = &b;

LAB:
  foo(p, q);
  /*@assert \valid(p) && \valid(q); */

LAB2:
  q = p;
  /*@assert \valid(p); */
  return 0;
}
