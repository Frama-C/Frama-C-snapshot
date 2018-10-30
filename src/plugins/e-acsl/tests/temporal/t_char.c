/* run.config
   COMMENT: Check that when small blocks (such as char) are used the
   COMMENT: instrumentation adds alignment sufficient for tracking
   COMMENT: block origin number via shadowing
*/

int main(int argc, const char **argv) {
  char a = '1',
       b = '2';
  /* Should fail here in debug mode due to overlapping shadow spaces
     of [a] and [b]*/

  char *p = &a,
       *q = &b;

  p = q;
  /*@assert \valid(p); */
  /*@assert \valid(q); */
  return 0;
}
