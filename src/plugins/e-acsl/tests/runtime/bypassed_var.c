/* run.config
   COMMENT: Variable, which declaration is bypassed by a goto jump
*/

int main(int argc, char const **argv) {
  goto L;
  {
    int *p;
    L:
      p = &argc; /* Important to keep this statement here to make sure
                   initialize is ran after store_block */

    /*@ assert \valid(&p); */
  }
  return 0;
}
