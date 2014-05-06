/* run.config
   OPT: -check -slice-return main -journal-disable -then-on 'Slicing export' -print -check
   OPT: -sparecode
*/
int main() {
  int t[10] = {0, 1, 2};
  return t[5]+t[2];
}
