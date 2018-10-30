/* run.config
   COMMENT: assert \true
*/
int main(void) {
  int x = 0;
  x++; /* prevent GCC's warning */
  ///*@ assert \true == 0; */ // \true as a term: not yet implemented
  /*@ assert \true; */
  return 0;
}
