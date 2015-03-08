#include "../glob.h"

int glob2 (Char *pathbuf, Char *pathend, Char *pathlim, Char *pattern)
{
  int i;
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
        /* OK */
        *pathend = SEP;
        pathend++;
        /* OK */
        *pathend = EOS;
      }
      // replaces globextend
      return 0;
    }

    /* Copies a single string from pattern into pathend, checking for 
     * the presence of meta-characters.
     */
    i = 0;
    while (pattern[i] != EOS && pattern[i] != SEP) {
      if (ismeta(pattern[i]))
        anymeta = 1;
      if (pathend + i >= pathlim)
        return 1;
      tmp = pattern[i];
      /* OK */
      pathend[i] = tmp;
      i++;
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
      pathend = pathend + i;
      pattern = pattern + i;
      while (*pattern == SEP) {
        // bounds check
        if (pathend >= pathlim)
          return 1;
        tmp = *pattern;
        /* OK */
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

  bound = A + sizeof(A)/sizeof(*A) - 1;

  glob2 (buf, buf, bound, pattern);

  return 0;
}
