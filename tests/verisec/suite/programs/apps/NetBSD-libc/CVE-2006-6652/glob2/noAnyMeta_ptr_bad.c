#include "../glob.h"

int glob2 (Char *pathbuf, Char *pathend, Char *pathlim, Char *pattern)
{
  Char *p, *q;
  Char tmp;

  for (;;) {
    q = pathend;
    p = pattern;
    while (*p != EOS && *p != SEP) {
      if (q >= pathlim)
        return 1;
      tmp = *p;
      /* BAD */
      *q = tmp;
      q++;
      p++;
    }

    if (nondet_int ())
      return 0;
  }

  /* NOT REACHED */
}

int main ()
{
  Char *buf;
  Char *pattern;
  Char *bound;

  Char A [MAXPATHLEN+1];
  Char B [PATTERNLEN];

  buf = A;
  pattern = B;

  bound = A + sizeof(A) - 1;

  glob2 (buf, buf, bound, pattern);

  return 0;
}
