/* run.config
	 COMMENT: Tests for function-based implementation of ctype.h features
*/

/* ctype.h tests (e.g., `isalpha`, `isnumber` etc) in GLIBC are implemented as
  macro-definitions featuring `__ctype_b_loc` function which returns an address
  of an array with locale-specific data. Because of Frama-C normalization below
  snippet:
    char c = isupper(argc);
    char *d = &c;
  is approximately as follows:
    char c;
    unsigned short const **tmp;
    char *d;
    tmp = __ctype_b_loc();
    d = &c;
  Since no implementation of `__ctype_b_loc` is provided, its return address
  is not recorded (the bounds of the array are also implementation specific).
  Then, `d` points to some internal array on stack and the assertion below
  does not hold (while it should).

  This test checks that E-ACSL uses function-based implementations of ctype
  tests (by defining __NO_CTYPE macro during preprocessing). Thus, the
  normalized code should resemble the below snippet:
    char c;
    int tmp;
    char *d;
    tmp = isupper(argc);
    c = (char)tmp;
    d = & c;
  Notably, since isupper returns an int, `d` points to `c` (on stack) and
  therefore the assertion holds. */

#include <ctype.h>

int main(int argc, const char **argv) {
  char c = isupper(argc);
  char *d = &c;
  /*@ assert \valid(d); */
  return 0;
}
