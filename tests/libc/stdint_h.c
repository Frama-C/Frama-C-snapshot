#include <stdint.h>

int main() {
  intmax_t min = INTMAX_MIN;
  intmax_t max = INTMAX_MAX;
  uintmax_t umax = UINTMAX_MAX;
  //@ assert min < max && max <= umax;
  return 0;
}
