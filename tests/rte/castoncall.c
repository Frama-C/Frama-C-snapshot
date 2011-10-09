/* run.config
  OPT: -rte -rte-precond -rte-print -journal-disable 
  OPT: -rte -rte-precond -no-collapse-call-cast -rte-print -journal-disable 
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

//@ ensures \result == 1;
int f();

void g() {
  char c = f();
  return;
}
