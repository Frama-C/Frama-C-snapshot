/* run.config
  OPT: -rte -warn-signed-overflow  -warn-signed-downcast -rte-precond -print
  OPT: -rte -warn-signed-overflow  -warn-signed-downcast -rte-precond -no-collapse-call-cast -print
*/

/*@ 
  ensures (\result == a) || (\result == b);
  assigns \result \from a,b;
 */
int nondet(int a, int b);

/*@ 
  ensures (\result == a) || (\result == b);
  assigns \result \from a,b;
 */
void *nondet_ptr(void *a, void *b) {
  return (void*) nondet((int)a, (int)b);
}

//@ ensures \result == 1; assigns \nothing;
int f();

void g() {
  char c = f();
  return;
}
