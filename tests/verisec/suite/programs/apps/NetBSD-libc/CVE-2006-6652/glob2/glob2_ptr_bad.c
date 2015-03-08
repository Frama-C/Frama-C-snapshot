#include "../glob.h"

int glob2 (Char *pathbuf, Char *pathend, Char *pathlim, Char *pattern)
{
  Char *p, *q;
  int anymeta;
  Char tmp;

  for (anymeta = 0;;) {

    /* End of the pattern. Recursion stops, and we extend the glob 
     * structure.
     */
    if (*pattern == EOS) {
      *pathend = EOS;
      if (NONDET()) {
        if (pathend >= pathlim)
          return 1;
        *pathend = SEP;
        pathend++;
        /* BAD */
        *pathend = EOS;
      }
      // replaces globextend
      return 0;
    }

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

    /* If there was no metacharacter, we take whatever came 
     * after the string we previously copied, copy it into 
     * pathend, and continue.
     *
     * If we did encounter a meta-character, we recurse 
     * by calling glob3 () -- we elide glob3 () in 
     * this example.
     */
    if (!anymeta) {
      pathend = q;
      pattern = p;
      while (*pattern == SEP) {
        // bounds check
        if (pathend >= pathlim)
          return 1;
        tmp = *pattern;
        /* BAD */
        *pathend = tmp;
        pathend++;
        pattern++;
      }
    } else {
      // stand-in for glob3 (which is recursive)
      return 0;
    }
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
