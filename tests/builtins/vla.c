/* run.config*
   STDOPT: #"-slevel 10 -eva-builtins-auto"
*/

void f(int i) {
  int t[i+1];
  Frama_C_show_each(t);
  t[i] = i;
  if (t[i] != i) { // test that we can always perform a strong update on the
    // base corresponding  to the vla. This works with the current allocation
    // builtin (Frama_C_malloc_by_stack) because bases that are allocated
    // multiple times, but that are never allocated twice _in the same state_
    // are never changed into weak bases.
    //@ assert \false;
  }
}

void main() {
  for (int i=1; i<10; i++)
    f(i);
}
