/* run.config_ci,run.config_dev
   COMMENT: Check detection of format-string vulnerabilities via printf
   DONTRUN:
*/

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <stdint.h>
#include <limits.h>
#include <wchar.h>
#include "signalled.h"

#define ABRT_AT(code,at) SIGNALLED_AT(code, 1, at)
#define OK_AT(code,at) SIGNALLED_AT(code, 0, at)

/* All valid format specifiers */
const char *valid_specifiers = "diouxfFeEgGaAcspn";

/* Given a format string with a specifier at the last position
   run printf with this format picking the right type */
void apply_specifier(char *format, int spec) {
  int n;
  void *p = NULL;
  if (strchr("fFeEgGaA", spec) != NULL)
    printf(format, 1.0);
  else if (strchr("uoxX", spec) != NULL)
    printf(format, 1U);
  else if (strchr("dic", spec) != NULL)
    printf(format, 97);
  else if (spec == 's')
    printf(format, "foo");
  else if (spec == 'n')
    printf(format, &n);
  else if (spec == 'p')
    printf(format, p);
  else
    abort();
}

/* Given a string consisting of format specifiers (`allowed`) and a mutable
   format string `fmt` with a specifier located at the last position
   run positive tests for all specifiers from `allowed` and negative ones
   for the remaining one. If `only_negative` is true then only negative tests
   are run */
void test_specifier_application(const char *allowed, const char *fmt, int only_negative, char *at) {
  int len = strlen(fmt);
  char format[len + 1];
  strcpy(format, fmt);
  int i;
  for (int i = 0; i < strlen(valid_specifiers); i++) {
    int c = valid_specifiers[i];
    format[len - 1] = c;
    if (strchr(allowed, c)) {
      if (!only_negative) {
        OK_AT(apply_specifier(format,c),at);
      }
    } else {
      ABRT_AT(apply_specifier(format,c),at);
    }
  }
}

int main(int argc, const char **argv) {
  // pointers
  char *pstr = "Hello world!";
  char astr[] = "Hello world!";
  signed char *sastr = astr;
  void *vptr = (void*)&argc;
  // char
  char chr = 'T';
  unsigned char uchr = 'U';
  // short
  short shrt = 4569;
  unsigned short ushrt = 4567;
  // int
  int i = 268;
  unsigned ui = 516;
  wint_t wi = 123;
  // long
  long li = 3134;
  unsigned long lu = 7845216;
  long long lli = 123LL;
  unsigned long long llu = 123LL;
  // double
  float flt = 0.2;
  double dbl = 0.3;
  long double ldbl = 0.3;
  // typedefs
  intmax_t imax = 10;
  uintmax_t uimax = 10;
  size_t szt = 10;
  ptrdiff_t ptrdf = 147;

  // An undefined behaviour occurs if:
  //    1. a format directive has no corresponding argument
  //    2. a format string is not NUL-terminated
  // +Argument Number
  //    3. numbered and non-numbered arguments cannot be mixed
  // +Precision
  //    4. precision is specified for a CS other than [diouxXaAeEfFgG]
  //       (csn%)
  // +Flags:
  //    5. a flag is not one of [-+ #0']
  //    6. '#' flag is used with a CS other than [oxXaAeEfFgG]
  //    7. '0' flag is used with a CS other than [diouxXaAeEfFgG]
  // +Length modifiers (LM):
  //    8. a LM is not one of [hljztL] or [ll] or [hh]
  //    9. there are more than one LM per one CS
  //    10. 'hh' used with a CS other than [diouxXn]
  //    11. 'h'  used with a CS other than [diouxXn]
  //    12. 'l'  used with a CS other than [diouxXncsaAeEfFgG]
  //    13. 'll' used with a CS other than [diouxXn]
  //    14. 'j'  used with a CS other than [diouxXn]
  //    15. 'z'  used with a CS other than [diouxXn]
  //    16. 't'  used with a CS other than [diouxXn]
  //    17. 'L'  used with a CS other than [aAeEfFgG]
  // +Conversion specifiers (CS):
  //    18. Not one of [diouxfFeEgGaAcspnCS%]
  //    - [di]
  //      19. no LM is present and the argument corresponding to
  //          the above specifier is not of type 'int'
  //      20. LM is present and the argument corresponding to the above
  //          CS is not of signed integral type given by the LM
  //    - [ouxX]
  //      21. no LM is present and the argument corresponding to
  //          the above specifier is not of type 'unsigned int'
  //      22. LM is present and the argument corresponding to the above
  //          CS is not of unsigned integral type given by the LM
  //    - [aAeEgGfF]
  //      23. no LM is present and the argument corresponding to the above CS
  //          is not of type 'double'
  //      24. LM is present (only 'L' is possible) and the argument
  //          corresponding to the above CS is not of type 'long double'
  //    - [c]
  //      25. no LM is present and the argument corresponding to the above CS
  //          is not of type 'int'
  //      26. LM is present (only 'l') and the argument corresponding to
  //          the above CS is not of type 'wint_t'
  //    - [s]
  //      27. no LM is present and the argument corresponding to the above CS
  //          is not a valid pointer of any character type.
  //      28. no LM is present and no precision is specified and the argument
  //          corresponding to the above CS is not NUL-terminated array of
  //          characters. An undefined behaviour also occurs if the precision
  //          is given but it is greater then the size of array and the array
  //          does not contain a NUL character
  //      29. LM is present and the argument corresponding to the above CS is
  //          not a valid pointer of wchar_t type.
  //      30. LM is present and no precision is specified and the argument
  //          corresponding to the above CS is not wide NUL-terminated (L'\0')
  //          array of wchar_t. An undefined behaviour also occurs if the
  //          precision is given but it is greater then the size of array
  //          and the array does not contain a NUL character
  //     - [p]
  //      31. the argument corresponding to the above CS is not a valid pointer
  //          of void type
  //     - [n]
  //      32. the argument corresponding to the above CS is not a valid pointer
  //          of signed int type
  //      33. directive involving the above CS contains flags or field width or
  //          or precision
  //     - [%]
  //      34. The complete specification for the above CS is other than '%%'
  //
  //  Additional undefined behaviours for functions other than printf
  //    35. fprintf: stream that fprintf writes to is not a valid open filehandle
  //    36. dprintf: file descriptor dprintf writes to is not a file descriptor
  //      for a file opened for writing
  //    37. sprintf/snprintf: buffer that sprintf or snprintf write to is not
  //      a writeable allocated block of whose size if equal to or larger than
  //      written bytes
  //    38. sprintf/snprintf: memory spaces given by the buffer and the rest of
  //      the arguments are not disjoint

  // Simplest case
  OK(printf("Hello world\n"));

  // A few conversion specifiers
  OK(printf("%s - %s! Say it %d or %u times \n", astr, pstr, i, ui));

  // Undef 1: insifficient arguments
  ABRT(printf("%s - %s and say it %d or %u more times \n", astr, pstr, i));

  // Excessive arguments are fine. They are discarded
  OK(printf("%s - %s. Say it %d or %u times \n", astr, pstr, i, ui, ui));

  // Undef 2: unterminated format string
  char fmt[7] = "fmt:%s";
  fmt[6] = 'a';
  ABRT(printf(fmt, pstr));

  // Support for numbered arguments
  OK(printf("%3$s Say it %2$d or %1$u times \n", ui, i, astr));
  // Excessive arguments lead to undefined behaviors
  ABRT(printf("%4$s Say it %2$d or %1$u times \n", ui, i, astr));
  // There is no argument 0
  ABRT(printf("%0$s Say it %2$d or %1$u times \n", ui, i, astr));

  // Undef 3: numbered and non-numbered arguments cannot be mixed ...
  ABRT(printf("%s Say it %2$d or %3$u times \n", astr, i, ui));
  // ... except for a complete specification '%%'
  OK(printf("%1$d - %% - %2$u times \n", i, ui));

  // Undef 4. precision is specified for a CS other than [diouxXaAeEfFgGs]
  test_specifier_application("diouxXaAeEfFgGs", "%.3X", 0, AT);

  // Undef 5. a flag is not one of [-+ #0']
  // Guarded by internal assertion

  // Undef 6. '#' flag is used with a CS other than [oxXaAeEfFgG] (i.e.,udicsn)
  test_specifier_application("oxXaAeEfFgG", "%#X", 0, AT);

  // Undef 7. '0' flag is used with a CS other than [diouxXaAeEfFgG] (i.e., csn)
  test_specifier_application("diouxXaAeEfFgG", "%0X", 0, AT);

  // Undef 8. a LM is not one of [hljztL] or [ll] or [hh]
  // Guarded by internal assertion

  // Undef 9. there are more than one LM per one CS
  OK(printf("%ld\n", 1L));
  OK(printf("%lld\n",1LL));
  ABRT(printf("%llld\n", 1LL));

  // FIXME: an issue with positive tests here. This is because length modifiers
  // change expected types and types used in the `apply_specifier` no longer apply.

  // Undef 10. 'hh' used with a CS other than [diouxXn]
  test_specifier_application("diouxXn", "%hhX", 1, AT);
  OK(printf("%hhd", 1)); OK(printf("%hhi", 1));
  OK(printf("%hhu", 1)); OK(printf("%hho", 1));
  OK(printf("%hhx", 1)); OK(printf("%hhX", 1)); OK(printf("%hhn", &chr));

  // Undef 11. 'h'  used with a CS other than [diouxXn]
  test_specifier_application("diouxXn", "%hX", 1, AT);
  OK(printf("%hd", 1)); OK(printf("%hi", 1));
  OK(printf("%hu", 1)); OK(printf("%ho", 1));
  OK(printf("%hx", 1)); OK(printf("%hX", 1)); OK(printf("%hn", &shrt));

  // Undef 12. 'l'  used with a CS other than [diouxXncsaAeEfFgG]
  test_specifier_application("diouxXncsaAeEfFgG", "%lX", 1, AT);
  OK(printf("%ld", 1L));  OK(printf("%li", 1L));
  OK(printf("%lu", 1UL)); OK(printf("%lo", 1UL));
  OK(printf("%lx", 1UL)); OK(printf("%lX", 1UL));
  // No effect on [aAeEfFgG]
  OK(printf("%f", dbl)); OK(printf("%F", dbl));
  OK(printf("%e", dbl)); OK(printf("%E", dbl));
  OK(printf("%a", dbl)); OK(printf("%A", dbl));
  OK(printf("%g", dbl)); OK(printf("%G", dbl));
  // Pointer to long int for [n]
  OK(printf("%ln", &li));
  // wint_t for [c], since wint_t is essentially short then it is the subject
  // to promotions and "%lc" expects an int
  OK(printf("%lc", wi));

  // Undef 13. 'll' used with a CS other than [diouxXn]
  //test_specifier_application("diouxXn", "%llX", 1, AT);
  OK(printf("%lld", 1LL)); OK(printf("%lli", 1LL));
  OK(printf("%llu", 1ULL)); OK(printf("%llo", 1ULL));
  OK(printf("%llx", 1ULL)); OK(printf("%llX", 1ULL)); OK(printf("%lln", &lli));

  // Undef 14. 'j'  used with a CS other than [diouxXn]
  test_specifier_application("diouxXn", "%jX", 1, AT);
  OK(printf("%jd", imax));  OK(printf("%ji", imax));
  OK(printf("%ju", uimax)); OK(printf("%jo", uimax));
  OK(printf("%jx", uimax)); OK(printf("%jX", uimax)); OK(printf("%jn", &imax));

  // Undef 15. 'z'  used with a CS other than [diouxXn]
  test_specifier_application("diouxXn", "%zX", 1, AT);
  // For 'zi' and 'zd' modifiers we need type of `size_t` size but signed.
  // For simplicity the below assumes that in a 32-bit system it is `int` and
  // `long` in 64 bit. This may fail though, so use with caution.
#if __WORDSIZE == 64
  OK(printf("%zd", li));  OK(printf("%zi", li));
#elif  __WORDSIZE == 32
  OK(printf("%zd", i));  OK(printf("%zi", i));
#endif
  OK(printf("%zu", szt)); OK(printf("%zo", szt));
  OK(printf("%zx", szt)); OK(printf("%zX", szt)); OK(printf("%zn", &szt));

  // Undef 16. 't'  used with a CS other than [diouxXn]
  // Same as above but since ptrdiff_t is sizned we have to tweak types for
  // [uoxX]
  test_specifier_application("diouxXn", "%tX", 1, AT);
#if __WORDSIZE == 64
  OK(printf("%tu", lu)); OK(printf("%to", lu));
  OK(printf("%tx", lu)); OK(printf("%tX", lu));
#elif  __WORDSIZE == 32
  OK(printf("%tu", u)); OK(printf("%to", u));
  OK(printf("%tx", u)); OK(printf("%tX", u));
#endif
  OK(printf("%td", ptrdf));  OK(printf("%ti", ptrdf));
  OK(printf("%tn", &ptrdf));

  // Undef 17. 'L'  used with a CS other than [aAeEfFgG]
  test_specifier_application("aAeEfFgG", "%LX", 1, AT);
  OK(printf("%Lf", ldbl)); OK(printf("%LF", ldbl));
  OK(printf("%Le", ldbl)); OK(printf("%LE", ldbl));
  OK(printf("%La", ldbl)); OK(printf("%LA", ldbl));
  OK(printf("%Lg", ldbl)); OK(printf("%LG", ldbl));

  // Undef 18. CS is not one of [diouxfFeEgGaAcspnCS%]
  // Try some of specifiers supported by GLIBC printf but not in C99
  ABRT(printf("%C\n",1));
  ABRT(printf("%S\n",1));
  ABRT(printf("%m\n",1));

  // Undef 19. [di]: no LM is present and the argument is not of type 'int'
  OK(printf("%i\n", i));        OK(printf("%d\n", i));
  OK(printf("%i\n", chr));      OK(printf("%d\n", chr)); // promoted to int
  OK(printf("%i\n", shrt));     OK(printf("%d\n", shrt)); // promoted to int
  ABRT(printf("%i\n", li));    ABRT(printf("%d\n", li));
  ABRT(printf("%i\n", ui));    ABRT(printf("%d\n", ui));
  ABRT(printf("%i\n", vptr));  ABRT(printf("%d\n", vptr));
  ABRT(printf("%i\n", flt));   ABRT(printf("%d\n", flt));

  // Undef 20. [di]: LM is present and the argument is not of type given by LM
  OK(printf("%li\n",  li));       OK(printf("%ld\n",  li));
  OK(printf("%lli\n", lli));      OK(printf("%lld\n", lli));
  OK(printf("%hi\n",  shrt));     OK(printf("%hd\n",  shrt));
  OK(printf("%hhi\n", chr));      OK(printf("%hhd\n", chr));
#if __WORDSIZE == 64
  OK(printf("%ji\n",  li));       OK(printf("%jd\n",  li));
#elif __WORDSIZE == 32
  OK(printf("%zi\n",  i));        OK(printf("%zd\n",  i));
#endif
  OK(printf("%ti\n",  ptrdf));    OK(printf("%td\n",  ptrdf));

  // Undef 21. [ouxX]: no LM is present and the argument is not 'unsigned int'
  OK(printf("%u\n", ui));       OK(printf("%o\n", ui));       OK(printf("%x\n", ui));       OK(printf("%X\n", ui));
  ABRT(printf("%u\n", li));    ABRT(printf("%o\n", li));    ABRT(printf("%x\n", li));    ABRT(printf("%X\n", li));
  ABRT(printf("%u\n", lu));    ABRT(printf("%o\n", lu));    ABRT(printf("%x\n", lu));    ABRT(printf("%X\n", lu));
  ABRT(printf("%u\n", flt));   ABRT(printf("%o\n", flt));   ABRT(printf("%x\n", flt));   ABRT(printf("%X\n", flt));
  ABRT(printf("%u\n", vptr));  ABRT(printf("%o\n", vptr));  ABRT(printf("%x\n", vptr));  ABRT(printf("%X\n", vptr));
  ABRT(printf("%u\n", astr));  ABRT(printf("%o\n", astr));  ABRT(printf("%x\n", astr));  ABRT(printf("%X\n", astr));

  // Undef 22. [ouxX]: LM is present and the argument is not of type given by the LM
  OK(printf("%lu\n", lu));    OK(printf("%lo\n", lu));    OK(printf("%lx\n", lu));    OK(printf("%lX\n", lu));
  OK(printf("%llu\n", llu));  OK(printf("%llo\n", llu));  OK(printf("%llx\n", llu));  OK(printf("%llX\n", llu));
  // subject to promotion so expects int
  OK(printf("%hu\n", i));     OK(printf("%ho\n", i));     OK(printf("%hx\n", i));     OK(printf("%hX\n", i));
  // subject to promotion so expects int
  OK(printf("%hhu\n", i));    OK(printf("%hho\n", i));    OK(printf("%hhx\n", i));    OK(printf("%hhX\n", i));
  OK(printf("%ju\n", uimax)); OK(printf("%jo\n", uimax)); OK(printf("%jx\n", uimax)); OK(printf("%jX\n", uimax));
  OK(printf("%zu\n", szt));   OK(printf("%zo\n", szt));   OK(printf("%zx\n", szt));   OK(printf("%zX\n", szt));
#if __WORDSIZE == 64
  OK(printf("%tu\n", lu));    OK(printf("%to\n", lu));    OK(printf("%tx\n", lu));    OK(printf("%tX\n", lu));
#endif

  // Undef 23. [aAgGfFeE]: no LM is present and the argument is not of type 'double'
     OK(printf("%f\n",dbl));       OK(printf("%F\n",dbl));
  ABRT(printf("%f\n",ldbl));   ABRT(printf("%F\n",ldbl));
  ABRT(printf("%f\n",i));      ABRT(printf("%F\n",i));
  ABRT(printf("%f\n",lu));     ABRT(printf("%F\n",lu));
     OK(printf("%a\n",dbl));       OK(printf("%A\n",dbl));
  ABRT(printf("%a\n",ldbl));   ABRT(printf("%A\n",ldbl));
  ABRT(printf("%a\n",i));      ABRT(printf("%A\n",i));
  ABRT(printf("%a\n",lu));     ABRT(printf("%A\n",lu));
     OK(printf("%e\n",dbl));       OK(printf("%E\n",dbl));
  ABRT(printf("%e\n",ldbl));   ABRT(printf("%E\n",ldbl));
  ABRT(printf("%e\n",i));      ABRT(printf("%E\n",i));
  ABRT(printf("%e\n",lu));     ABRT(printf("%E\n",lu));
     OK(printf("%g\n",dbl));       OK(printf("%G\n",dbl));
  ABRT(printf("%g\n",ldbl));   ABRT(printf("%G\n",ldbl));
  ABRT(printf("%g\n",i));      ABRT(printf("%G\n",i));
  ABRT(printf("%g\n",lu));     ABRT(printf("%G\n",lu));

  // 24. [aAgGfFeE]: 'L' LM is present and the argument is not of type 'long double'
  ABRT(printf("%Lf\n",dbl));    ABRT(printf("%LF\n",dbl));
     OK(printf("%Lf\n",ldbl));      OK(printf("%LF\n",ldbl));
  ABRT(printf("%Lf\n",i));      ABRT(printf("%LF\n",i));
  ABRT(printf("%Lf\n",lu));     ABRT(printf("%LF\n",lu));
  ABRT(printf("%La\n",dbl));    ABRT(printf("%LA\n",dbl));
     OK(printf("%La\n",ldbl));      OK(printf("%LA\n",ldbl));
  ABRT(printf("%La\n",i));      ABRT(printf("%LA\n",i));
  ABRT(printf("%La\n",lu));     ABRT(printf("%LA\n",lu));
  ABRT(printf("%Le\n",dbl));    ABRT(printf("%LE\n",dbl));
     OK(printf("%Le\n",ldbl));      OK(printf("%LE\n",ldbl));
  ABRT(printf("%Le\n",i));      ABRT(printf("%LE\n",i));
  ABRT(printf("%Le\n",lu));     ABRT(printf("%LE\n",lu));
  ABRT(printf("%Lg\n",dbl));    ABRT(printf("%LG\n",dbl));
     OK(printf("%Lg\n",ldbl));      OK(printf("%LG\n",ldbl));
  ABRT(printf("%Lg\n",i));      ABRT(printf("%LG\n",i));
  ABRT(printf("%Lg\n",lu));     ABRT(printf("%LG\n",lu));

  // Undef 25. [c] no LM is present and the argument is not of type 'int'
  OK(printf("%c\n", i));
  OK(printf("%c\n", chr));
  OK(printf("%c\n", shrt));
  ABRT(printf("%c\n", ui));
  ABRT(printf("%c\n", li));
  ABRT(printf("%c\n", flt));
  ABRT(printf("%c\n", astr));

  // Undef 26. [c]: 'l' LM is present and the argument is not of type 'wint_t'
  OK(printf("%lc\n", wi)); // Both are fine because size comparison is performed
  ABRT(printf("%lc\n", li));

  // Undef 27/28. no LM is present and
  //    - the argument is not a valid pointer of any character type
  //    - no precision is specified the argument is not NUL-terminated
  //    - the precision is given but it is greater then the size of array
  //      and the array does not contain a NUL character
  OK(printf("%s\n", astr));
  OK(printf("%s\n", pstr));
  ABRT(printf("%s\n", i));
  ABRT(printf("%s\n", vptr));

  char *s1 = NULL;
  char *s2;

  ABRT(printf("%s\n", s1)); // Unallocated
  ABRT(printf("%s\n", s2)); // Unallocated

  char s4[4] = "cat";
  OK(printf("%s\n", s4));
  s4[3] = 's';
  ABRT(printf("%s\n", s4)); // Not NUL-terminated

  // Precision
  OK(printf("%.s\n", s1));  // The precision is zero, so print nothing
  OK(printf("%.0s\n", s1)); // The precision is zero, so print nothing
  OK(printf("%.3s\n", s4)); // Within allocated limits even though no NUL
  OK(printf("%.4s\n", s4)); // Within allocated limits even though no NUL
  ABRT(printf("%.5s\n", s4)); // Precision goes over limits

  // Undef 29/30. Same as 27/28 but for '%ls' and wchar_t*
#ifdef WIDE_STRING
  ABRT(printf("%ls\n", astr));
  ABRT(printf("%ls\n", pstr));
  ABRT(printf("%ls\n", NULL));

  wchar_t *ls1;
  wchar_t *ls2 = NULL;
  wchar_t ls4[4] = L"cat";

  ABRT(printf("%ls\n", ls1));
  ABRT(printf("%ls\n", ls2));
  OK(printf("%ls\n", ls4));

  // Precision
  OK(printf("%.ls\n", ls1));  // The precision is zero, so print nothing
  OK(printf("%.0ls\n", ls1)); // The precision is zero, so print nothing
  OK(printf("%.3ls\n", ls4)); // Within allocated limits even though no NUL
  OK(printf("%.4ls\n", ls4)); // Within allocated limits even though no NUL
  ABRT(printf("%.5ls\n", ls4)); // Precision goes over limits
#endif

  // Undef 31. [p]: the argument is not a valid pointer of void type
  OK(printf("%p", vptr));
  ABRT(printf("%p", astr));
  ABRT(printf("%p", NULL));

  // Undef 32. [n]: the argument is not a valid pointer of signed int type
  OK(printf("%n", &i));
  ABRT(printf("%n", &ui)); // pointer to unsigned type
  ABRT(printf("%n", NULL)); // NULL
  ABRT(printf("%n", (int*)pstr)); // cast of read-only string

  // Undef 33. [n]: directive using [n] contains flags or field width or precision
  ABRT(printf("%'n", &i));
  ABRT(printf("%0n", &i));
  ABRT(printf("%#n", &i));
  ABRT(printf("% n", &i));
  ABRT(printf("%+n", &i));
  ABRT(printf("%-n", &i));
  ABRT(printf("%.n", &i));
  ABRT(printf("%.2n", &i));
  ABRT(printf("%.2n", &i));
  ABRT(printf("%10n", &i));

  // Undef 34. [%]: the complete specification is other than '%%'
  ABRT(printf("%d - %'% - %u times \n", i, ui));
  return 0;
}
