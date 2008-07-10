/* run.config
   GCC:
   OPT: -impact-pragma f -lib-entry -main f -impact-print
   */

int P,c;

/*@ requires \valid(x); */
int f(int *x) {
  /*@ impact pragma stmt; */
  int *y = x;
  *y = 4;
  int a = *x + 2;
  *y = 2;
  if (c) 
    return *x;
  else {
    y = P;
    return *y;
  }
}
