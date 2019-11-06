/* run.config
   COMMENT: cast
*/

int main(void) {
  long x = 0;
  int y = 0;

  /*@ assert (int)x == y; */ ;
  /*@ assert x == (long)y; */ ;

  /*@ assert y == (int)0; */ ; // cast from integer to int
  /*@ assert (unsigned int) y == (unsigned int)0; */ ; /* cast from integer
                                                          to unsigned int */

  /*@ assert y != (int)0xfffffffffffffff; */ ; // cast from integer to int
  /*@ assert (unsigned int) y != (unsigned int)0xfffffffffffffff; */ ;
  /* cast from integer to unsigned int */

  /* heterogeneous casts from/to integers */
  int t[2] = { 0, 1 };
  /*@ assert (float)x == t[(int)0.1]; */

  return 0;
}
