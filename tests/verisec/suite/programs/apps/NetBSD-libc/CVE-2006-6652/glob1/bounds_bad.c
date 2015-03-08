#include "../glob.h"

int main ()
{
  Char pathbuf[MAXPATHLEN+1];

  Char *bound = pathbuf + sizeof(pathbuf) - 1;

  /* Force SatAbs to check that bound is in bounds.
   *
   * This test is meant to mimic checking that "bound" is computed 
   * correctly *before* passing it to glob2().
   */
  /* BAD */
  *bound = 10;
  return 0;
}
