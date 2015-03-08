#include "../glob.h"

int glob2 (Char *pathbuf, Char *pathend, Char *pathlim, Char *pattern)
{
  int i;
  Char tmp;

  i = 0;
  for (;;) {
    while (pattern[i] != EOS && pattern[i] != SEP) {
      if (pathend + i >= pathlim)
        return 1;
      tmp = pattern[i];
      /* OK */
      pathend[i] = tmp;
      i++;
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

  bound = A + sizeof(A)/sizeof(*A) - 1;

  glob2 (buf, buf, bound, pattern);

  return 0;
}
