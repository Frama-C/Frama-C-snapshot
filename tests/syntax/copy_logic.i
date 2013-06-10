/* run.config
   STDOPT: +"-copy" +"-val"
 */

/*@ predicate p(int x); */
/*@ predicate q(int x) = x == 42; */
/*@ logic int f (int y); */
/*@ logic integer g (int x) = x + 42; */


/*@ frees x; */
void f(int *x);

int main (int x) {
  int y = 42;
  /*@ assert q(y) && p(x); */
  y+=x;
  /*@ assert g(x) == f(y); */
  return 0;
}
