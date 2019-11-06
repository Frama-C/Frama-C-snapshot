/* run.config
   COMMENT: pointers and pointer arithmetic
*/

int main(void) {

  int x = 1;
  int t[3] = { 2, 3, 4 };
  int *p = &x;

  /*@ assert *p == 1; */
  /*@ assert *t == 2; */
  /*@ assert *(t+2) == 4; */
  /*@ assert *(t+2*sizeof(int)/sizeof((int)0x0)) == 4; */

  for(int i = 0; i < 2; i++) {
    /*@ assert (*(t+i) == i+2); */ ;
    /*@ assert (*(t+(2-i)) == 4-i); */ ;
    /*@ assert (*(t+2-i) == 4-i); */ ;
    ;
  }

  p = t+2;
  t[2] = 5;
  /*@ assert *p == 5; */
  int k = -1;
  /*@ assert *(p+k) == 3; */ // bts #2252

  return 0;
}
