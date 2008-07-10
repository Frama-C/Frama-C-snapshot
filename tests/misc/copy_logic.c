/* run.config
   OPT: -files-debug "-check -copy" -val -print
 */

/*@ predicate p(int x) reads x; */
/*@ predicate q(int x) = x == 42; */
/*@ logic int f (int y) reads y; */
/*@ logic integer g (int x) = x + 42; */

int main (int x) {
  int y = 42;
  /*@ assert q(y) && p(x); */
  y+=x;
  /*@ assert g(x) == f(y); */
  return 0;
}
