#include "time.c"

volatile int v;

int main() {
  time_t t;
  if (v) t = 42;
  char *s = ctime(&t); // warn about initialization
  //@ assert valid_read_string(s);

  return 0;
}
