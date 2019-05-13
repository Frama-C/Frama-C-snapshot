volatile int nondet;

int main () {
  int i = 42;
  if (nondet) goto end; // Invalid goto, as it skips the initialization of vla.
  char vla[i];
  if (nondet) return 1;
 end:
  return 0;
}
