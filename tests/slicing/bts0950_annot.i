/* run.config
   OPT: -val -slice-value a -then-on "Slicing export" -print
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
