#include <stdio.h>
#include "__fc_builtin.h"

volatile int nondet;
int main() {
  FILE *f = fopen("/dev/urandom", "r");
  if (!f) return 1;
  int r = fseek(f, 0L, SEEK_SET);
  if (nondet) {
    fseek(NULL, 0L, SEEK_CUR); // must fail
    //@ assert \false;
  }
  if (nondet) {
    // to obtain an invalid value for whence, any interval containing at
    // least 4 elements must contain an invalid value
    int invalid_whence = Frama_C_interval(0, 3);
    if (invalid_whence != SEEK_SET && invalid_whence != SEEK_CUR &&
        invalid_whence != SEEK_END) {
      fseek(f, 42, invalid_whence); // must fail
      //@ assert \false;
    }
  }
  return 0;
}
