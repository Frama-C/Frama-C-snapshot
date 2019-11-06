/* run.config
   COMMENT: integer constant + a stmt after the assertion
*/
int main(void) {
  int x;
  /*@ assert 0 == 0; */ x = 0;
  x++; /* prevent GCC's warning */
  /*@ assert 0 != 1; */
  /*@ assert 1152921504606846975 == 0xfffffffffffffff; */

  /*@ assert 0xffffffffffffffffffffffffffffffff == 0xffffffffffffffffffffffffffffffff; */

  return 0;
}
