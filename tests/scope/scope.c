/* run.config
   OPT: -val -main f share/builtin.c -journal-disable
   OPT: -val -main f2 -journal-disable
   OPT: -val -main loop -journal-disable
*/
/*
 * bin/viewer.byte -main f tests/scope/scope.c -val share/builtin.c
 * bin/viewer.byte -main f2 tests/scope/scope.c -val
 * bin/viewer.byte -main loop tests/scope/scope.c -val
 */

#include "share/builtin.h"

typedef struct {int a; int b; } Tstr;
Tstr S1, S2;
int T[100];

int f (int x, int y, Tstr s) {
  int a, b;
  int * p;
  int i;
  if (x > 0) {
    p =  &x;
    a = 0;
    s.a = 3;
    i =  Frama_C_interval (5, 15);
    T[i] = 1;
  }
  else {
    p =  &y;
    b = 0;
    i =  Frama_C_interval (10, 20);
    T[i] = 2;
  }
  i = 0;
  x = 5;
  y = 10;
  /* It can be interesting to see that selecting T[i]
   * is not the same than selecting T[0] even if i=0 */
  *p = i;
  x = 4;
  return *p;
}

void f2 (int c) {
  int x, y;
  y = 0;
  x = 1;
  y++;
  if (c) {
    y++;
  }
  else {
    y++;
    x = 2;
    y++;
  }
  y++;
}

int loop (int n) {
  int a, b, i, s;
  i = 0;
  s = 0;
  a = 0;
  b = 0;
  while (i < n) {
    a++;
    b++;
    s++;
    /* selecting i here select also stmts before the loop */
    /* selecting s here select also stmts after the loop */
    i++;
  }
  a++;
  b++;
  i++;
  return s;
}
