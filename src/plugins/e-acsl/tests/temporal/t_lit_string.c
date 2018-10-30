/* run.config
   COMMENT: Check handling of literal strings. Because literal strings are
   COMMENT: replaced by variables we need to make sure that we take block
   COMMENT: numbers and not referent numbers in assignments
*/

int main(void) {
  char *f = "f";
  /*@assert \valid_read(f) && !\valid(f); */
  char *g;
  g = "g";
  /*@assert \valid_read(g) && !\valid(g); */

  char *p = f;
  /*@assert \valid_read(p) && !\valid(p); */
  char *q;
  q = f;
  /*@assert \valid_read(q) && !\valid(q); */
  return 0;
}
