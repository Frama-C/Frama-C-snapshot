#include <stdlib.h>

int main() {
  char *s = "12 34 56";
  char *p, *q;
  long l = strtol(s, &p, 0);
  l = strtol(p, &q, 0);
  l = strtol(q, NULL, 0);
  l = strtol(s+8, NULL, 0);
  return l;
}
