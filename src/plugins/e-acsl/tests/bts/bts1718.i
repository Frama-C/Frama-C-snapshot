/* run.config
   COMMENT: bts #1718, issue regarding incorrect initialization of literal strings in global arrays with compound initializers
   COMMENT: no diff
   COMMENT: no diff
   COMMENT: no diff
*/

int main(void) {
  int a = 10, *p;
  goto lbl_1;

 lbl_2:
  /*@ assert \valid(p); */
  return 0;

 lbl_1:
  p = &a;
  goto lbl_2;
}
