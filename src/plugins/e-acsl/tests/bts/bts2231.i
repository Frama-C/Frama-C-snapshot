/* run.config
   COMMENT: bts #2231, issue with typing of casts
*/

long A = 0;

int main(void) {
  /*@ assert A + (long)((long)(3 * A) - 1) == -1; */
  return 0;
}
