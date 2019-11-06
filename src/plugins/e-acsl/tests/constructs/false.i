/* run.config
   COMMENT: assert \false
*/
int main(void) {
  int x = 0;
  if (x) /*@ assert \false; */ ;
  return 0;
}
