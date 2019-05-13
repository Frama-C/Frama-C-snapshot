/*run.config
  OPT: -eva -print
*/
int main (void) {
  int x = 42;
  /*@ check (boolean)x == 17; */
  /*@ check (integer)(boolean)x == 17; */
}
