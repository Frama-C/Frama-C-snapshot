/* run.config
   OPT: -val-show-progress -val -slice-value a -then-on "Slicing export" -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check
 */
/*@ requires \valid(dest); */
extern void cpy(int *dest, const int *src);

void cpy(int* region1, const int* region2) {
  *(region1) = *region2;
}

int a=1, b=2;

void main() {
  cpy(&a,&b);
}
