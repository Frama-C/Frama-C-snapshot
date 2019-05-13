#include <strings.h>

volatile int nondet;
void main() {
  const char *s1 = "AbC";
  const char *s2 = "De";
  const char *s3 = "ABc";
  char s[3] = "ABc";
  int r1 = strcasecmp(s1, s2);
  int r2 = strcasecmp(s1, s3);
  int r3 = strcasecmp(s2, s3);
  if (nondet) strcasecmp(s1, s);
  int r4 = strncasecmp(s1, s, 3);
  if (nondet) strncasecmp(s1, s, 4);
  int r5 = strncasecmp(s1, s2, 0);

  char s4[10];
  bzero(s4, 10);
  //@ assert s4[9] == s4[8] == 0;
}
