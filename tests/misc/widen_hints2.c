/* run.config
   STDOPT: #"-value-msg-key widen-hints"
   OPT: -print
 */

#include <stdlib.h>

#define N 2

int t[100];
const int x = 9;

int glob;

void f() {
  int tf[100];
  int m = 10;
  int n = 33+m;
  /*@
    widen_hints "all", 88; // necessary, apply on all variables
  */
  for (int a = 0; a < n*2+1; a++) { // uses @all hint
    for (int b = 0; b < a; b++) {
      tf[b] = 1;
    }
  }
}

void g() {
  int tg[100];
  int m = 10;
  int n = 33+m;
  // no hints from other functions should apply to these variables
  for (int ll = 0; ll < n*2+1; ll++) {
    for (int kk = 0; kk < ll; kk++) {
      tg[kk] = 1;
    }
  }
}

int y; // used to force renaming of local variable

int main() {
  int y;
  int m = 10;
  /*@ widen_hints global:m, N; // useless, just syntactic test
      widen_hints y, 5; // useless, just syntactic test */
  int n = 33+m;
  /*@ loop widen_hints a, 2; // useless, just syntactic test */
  for (int a = 0; a < n*2+1; a++) { // uses global hint
    /*@ widen_hints a, 88; */ // necessary
    for (int b = 0; b < a; b++) {
      t[b] = 1;
    }
  }

  /*@ loop widen_hints c, 88; // necessary
      loop widen_hints y, 1; // useless, just syntactic test:
                             // should be renamed to y_0
      loop widen_hints global:y, 2; // useless, just syntactic test:
                                    // should be renamed to y_0
  */
  for (int c = 0; c < n*2+1; c++) { // uses loop-local hint
    for (int d = 0; d < c; d++) {
      t[d] = 1;
    }
  }

  /*@
    loop widen_hints c, 88; // necessary (tests renaming of local variables)
  */
  for (int c = 0; c < n*2+1; c++) { // uses loop-local hint
    for (int d = 0; d < c; d++) {
      t[d] = 1;
    }
  }

  //@ widen_hints glob, 88;
  for (glob = 0; glob < n*2+1; glob++) { // uses hint based on global variable
    for (int j = 0; j < glob; j++) {
      t[j] = 1;
    }
  }

  f();
  g();

  return 0;
}
