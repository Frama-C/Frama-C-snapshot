/* run.config
   OPT: -check -slice-assert main -journal-disable -then-on 'Slicing export' -print
*/
int main (int c) {
  if (c)
    while (1) { ; }
  //@ assert c == 0;
  return c;
}
