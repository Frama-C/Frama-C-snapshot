/* run.config*
   DONTRUN: small example related to U3CAT's WP2
*/
int x;

void f(void) { x = 2; }

void g(void) { x++; }

int main() {
  if (!x) { f(); }
  g();
  return 0;
}
