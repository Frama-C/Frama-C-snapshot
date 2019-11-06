/* run.config
   COMMENT: structured stmt with several code annotations inside
*/

int main(void) {
  int x = 0, y = 1;
  /*@ assert x < y; */
  /*@ requires x == 0;
    @ ensures x >= 1; */
  {
    if (x) /*@ assert \false; */ ;
    else {
      /*@ requires x == 0;
	@ ensures x == 1; */
      x++;
      if (x) {
	/*@ requires x == 1;
	  @ ensures x == 2; */
	x++;
      }
      else /*@ assert \false; */ ;
    }
  }
  return 0;
}
