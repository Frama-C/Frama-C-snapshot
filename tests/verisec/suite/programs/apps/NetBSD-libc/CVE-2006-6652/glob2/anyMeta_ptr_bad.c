#include "../glob.h"

int glob2 (Char *pathbuf, Char *pathend, Char *pathlim, Char *pattern)
{
  Char *p, *q;
  int anymeta;
  Char tmp;

  for (anymeta = 0;;) {

    /* Copies a single string from pattern into pathend, checking for 
     * the presence of meta-characters.
     */
    q = pathend;
    p = pattern;
    while (*p != EOS && *p != SEP) {
      if (ismeta(*p))
        anymeta = 1;
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
