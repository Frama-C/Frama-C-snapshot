/* run.config_qualif
   DONTRUN:
*/
/*@ assigns \nothing; */
int main() {
  int foo = 1;
  1 & (foo & 0x80000000000001LL) << 1;
  return 0;
}
