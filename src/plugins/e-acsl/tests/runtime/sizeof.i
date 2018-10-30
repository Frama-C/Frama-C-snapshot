/* run.config
   COMMENT: sizeof
*/

int main(void) {
  int x = 0;
  x++; /* prevent GCC's warning */
  /*@ assert sizeof(int) == sizeof(x); */ ;
  return 0;
}
