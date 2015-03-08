#include "../glob.h"

int glob2 (Char *pathbuf, Char *pathlim)
{
  Char *p;

  for (p = pathbuf; p <= pathlim; p++) {
    /* BAD */
    *p = 1;
  }

  return 0;
}

int main ()
{
  Char pathbuf[MAXPATHLEN+1];

  Char *bound = pathbuf + sizeof(pathbuf) - 1;

  glob2 (pathbuf, bound);

  return 0;
}
