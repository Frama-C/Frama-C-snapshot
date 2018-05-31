/* Tests for the equality domain. */

volatile int rand;

/* Tests the replacement of an lvalue x by an equal term when x also appears
   in another term t equal to x. The precision gain is useless in these cases,
   but the domain nust not crash or be unsound: x cannot be replaced by t. */
void main () {
  int x = rand;
  int y = x;
  int z = 0;
  /* Tests if x is even in a way that the backward propagation fails to
     reduce x. */
  if (x == x/2 + x/2) {
    /* Replaces x by y (and not by x/2 + x/2) in the equality domain.  */
    x = 0;
    /* After the test, the equality could further reduce y to [-8..8]. */
    if (-10 < y && y < 10) {
      /* A temporary variable is needed to avoid a cycle in the evaluations:
         when evaluating y, the oracle for y/2+y/2 is top (as y has not been
         evaluated yet). */
      int tmp = y;
      z = tmp;
    }
  }
}
