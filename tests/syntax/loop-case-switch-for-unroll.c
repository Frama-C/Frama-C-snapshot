/* run.config
   STDOPT: +"-slevel 100 -val"
   STDOPT: +"-ulevel 1 -slevel 100 -val"
   STDOPT: +"-ulevel 2 -slevel 100 -val"
   COMMENT: compile and run with GCC, save output to a file, and compare it to
   the result of Frama-C piped to:
     "| grep Frama_C_show_each | sed 's/^.*Frama_C_show_each_//'"
*/

#ifdef __FRAMAC__
#define print(line, s, a) Frama_C_show_each_ ## s ## _(a)
#else
#include <stdio.h>
#define STR(a) _STR(a)
#define _STR(a) #a
#define print(line, s, a) printf("%s_: {%d}\n", STR(s), a)
#endif

int gen_nondet(int line) {
  static int vals[] =
    { 1,  // goto L1
      42, // j
      5,  // >10?
      1,  // goto L
      43, // j
      11, // >10?
      0,  // no jump
      1,  // goto L0
      0,  // no jump
      44, // j
      12, // >10?
      0,  // no jump
      0,  // no jump
      1,  // goto L3
      1,  // goto L1
      45, // j
      11, // >10?
      0, 0, 0, // no jump
      0,  // no jump
      46, // j
      13, // >10?
      0, 0, 0, // no jump
      0,  // no jump
      47, // j
      12, // >10?
      0, 0, 0, // no jump
      48, // j
      15, // >10?
      0, 0, 0, // no jump
    };
  static int i = -1;
  i = (i+1)%(sizeof(vals)/sizeof(int));
  print(line, nondet, vals[i]);
  return vals[i];
}

#define nondet() gen_nondet(__LINE__)

void main() {
  int y = 32;
  int x;
  int n = 3;
 L0: switch(1) {
  case 0:
  L3:
    print(__LINE__, n, n);
    if (nondet()) goto L;
    if (nondet()) goto L1;
    do {
      case 1:
        if (nondet()) goto L1;
    L: x = y;
        case 2:
          for (int i = 0, j; i < 4; i++) {
          L1:
            j = nondet();
            if (nondet() > 10) i = 10; else i = 0;
            print(__LINE__, i, i);
            if (nondet()) goto L;
            if (nondet()) goto L0;
            if (nondet()) goto L3;
          }
    } while(--n > 0);
    print(__LINE__, y, y);
    print(__LINE__, x, x);
  }
}
