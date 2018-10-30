/* run.config
   COMMENT: Check simple case of calling functions via pointer derefernce
*/

int* foo(int *p) {
  int *q = p;
  return q;
}

int main(int argc, const char **argv) {
  int *p = &argc,
      *q;

  int* (*fp)(int*) = &foo;

  fp = &foo;
  /*@assert \valid_function(fp); */

  q = (*fp)(p);
  /*@assert \valid(q); */
  return 0;
}
