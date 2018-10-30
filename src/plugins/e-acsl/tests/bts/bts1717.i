/* run.config
   COMMENT: bts #1717, issue with labels on memory-related statements
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
