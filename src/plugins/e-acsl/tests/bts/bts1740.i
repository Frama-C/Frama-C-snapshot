/* run.config
   COMMENT: bts #1740, about failure to correctly track local variables
            in presence of goto
*/

int main(void) {
  int *p;
  {
    int a = 0;
    p = &a;
    /*@ assert \valid(p); */
    goto L;
  }

 L:
  /*@ assert ! \valid(p); */
  return 0;
}
