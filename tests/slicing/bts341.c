/* run.config
   OPT: -slice-print -slice-assert main -journal-disable
*/
int main (int c) {
  if (c)
    while (1) { ; }
  //@ assert c == 0;
  return c;
}
