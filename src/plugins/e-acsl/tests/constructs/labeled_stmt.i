/* run.config
   COMMENT: labeled stmt and gotos
*/

int X = 0;

/*@ ensures X == 3; */
int main(void) {
  goto L1;
 L1: /*@ assert X == 0; */ X = 1;
  goto L2;
 L2: /*@ requires X == 1; ensures X == 2; */ X = 2;
  if (X) { X = 3; return 0; }
  return 0;
}
